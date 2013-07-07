#'
#'
setClass("Broker",
		representation(
			market = "Market", 
			open.orders = "environment", 
			closed.orders = "environment", 
			cost.model = "function"
		))

Broker <- function() {
	new("Broker", 
			market = new("Market"), 
			open.orders = new.env(emptyenv()), 
			closed.orders = new.env(emptyenv()), 
			cost.model = default_cost_model)
}

default_cost_model <- function(order) {
	return(0)
}

addMarket <- function(broker, market) {
			broker@market <- market
			return(broker)
		}
		
addTxnCostModel <- function(broker, model) {
	broker@cost.model <- model
	return(broker)
}

setMethod("getBar",
		signature("Broker"),
		function(object, instrument, timestamp) {
			getBar(object@market, instrument, timestamp)
		})

addOrder <- function(broker, order) {
	if (!inherits(order, "Order")) {
		stop("Only orders derived from class 'Order' may be added to Broker")
	}
	order <- setTxnCostModel(order, broker@cost.model)
	addOrderToBook(broker, order, order.book = "open.orders")
}

addOrderToBook <- function(broker, order, order.book) {
	order.list <- getOrders(broker, instrumentOf(order), order.book)
	order <- setID(order, as.integer(length(order.list) + 1))
	order.list <- c(order.list, list(order))
	assign(instrumentOf(order), order.list, slot(broker, order.book))
}

updateOrder <- function(broker, order) {
	if (status(order) == "closed") {
		removeFromOpenOrders(broker, order)
		addOrderToBook(broker, order, order.book = "closed.orders")
	} else {
		updateOpenOrdersBook(broker, order)
	}
}

updateOpenOrdersBook <- function(broker, order) {
	order.list <- openOrders(broker, instrumentOf(order))
	order.list[[getID(order)]] <- order
	assign(instrumentOf(order), order.list, broker@open.orders)
}

removeFromOpenOrders <- function(broker, order) {
	order.list <- openOrders(broker, instrumentOf(order))
	order.list[[getID(order)]] <- NULL
	assign(instrumentOf(order), order.list, broker@open.orders)
}

openOrders <- function(broker, instrument) {
	getOrders(broker, instrument, "open.orders")
}

closedOrders <- function(broker, instrument) {
	getOrders(broker, instrument, "closed.orders")
}

getOrders <- function(broker, instrument, type) {
	possible.types <- c("open.orders", "closed.orders")
	if (!type %in% possible.types) {
		stop(paste("order.type must be one of:", 
						paste(possible.types, collapse = ", ")))
	}
	order.list <- tryCatch(
			get(instrument, slot(broker, type)), 
			error = function(e) list())
}

activeInstruments <- function(broker) {
	return(ls(broker@open.orders))
}

tradedInstruments <- function(broker) {
	return(ls(broker@closed.orders))
}

#' Notify all orders of market actions
marketActivity <- function(broker, timestamp) {
	
	for (instrument in activeInstruments(broker)) {
		price.bar <- getBar(broker, instrument, timestamp)
		notifyOrders(broker, instrument, price.bar)
	}
	return(broker)
}

#' Notify each order of prices for day.
notifyOrders <- function(broker, instrument, price.bar) {
	for (order in openOrders(broker, instrument)) {
		notify(order, broker, price.bar)
	}
}

printOrderBook <- function(broker, portfolio) {
	
	print_orders <- function(order.list) {

		timestamp <- "1999-12-31"
		order_book <- xts(t(c(
								"Order.Qty" = 0, 
								"Order.Price" = NA, 
								"Order.Type" = "init", 
								"Order.Side" = "long",
								"Order.Threshold" = 0, 
								"Order.Status" = "closed", 
								"Order.StatusTime" = as.character(as.POSIXct(timestamp)), 
								"Prefer" = "", 
								"Order.Set" = "", 
								"Txn.Fees" = 0, 
								"Rule" = "")), 
				order.by = as.POSIXct(timestamp))
		
		for (order in order.list) {
			order_book <- rbind(order_book, bookEntry(order))
		}
		return(order_book)
	}
	
	
	order.book <- list()
	order.book[[portfolio]] <- lapply(as.list(broker@closed.orders), print_orders)
	class(order.book) <- "order_book"
	return(order.book)
}


















