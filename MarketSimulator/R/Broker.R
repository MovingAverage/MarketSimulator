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

addTxnCostModel <- function(broker, model) {
	broker@cost.model <- model
	return(broker)
}

addMarket <- function(broker, market) {
			broker@market <- market
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

#' Print closed orders as quantstrat order_book object
printOrderBook <- function(broker, portfolio) {
	
	print_orders <- function(order.list) {

		timestamp <- as.POSIXct(initDate())
		order.book <- xts(t(c(
								"Order.Qty" = 0, 
								"Order.Price" = NA, 
								"Order.Type" = "init", 
								"Order.Side" = "long",
								"Order.Threshold" = 0, 
								"Order.Status" = "closed", 
								"Order.StatusTime" = as.character(timestamp), 
								"Prefer" = "", 
								"Order.Set" = "", 
								"Txn.Fees" = 0, 
								"Rule" = "")), 
				order.by = timestamp)
		
		for (order in order.list) {
			order.book <- rbind(order.book, bookEntry(order))
		}
		return(order.book)
	}
	
	
	order_book <- list()
	order_book[[portfolio]] <- lapply(as.list(broker@closed.orders), print_orders)
	class(order_book) <- "order_book"
	return(order_book)
}

#' Updates named portfolio with executed transactions
#' 
#' This function is used to integrate MarketSimulator with the blotter package.
#' The PortfolioTxns function adds transactions to the portfolio with blotter:addTxn, 
#' which then allows further analysis and reporting to be done with blotter functionality.
#' 
#' @param broker the Broker object which contains the closed orders
#' @param portfolio a character string naming the portfolio for recording transactions.
#' @param verbose logical indicating whether each transaction is to be printed.
#' 
portfolioTxns <- function(broker, portfolio, verbose = TRUE) {
	
	for (instrument in tradedInstruments(broker)) {
		for (order in closedOrders(broker, instrument)) {
			
			addTxn(portfolio, instrument, 
					TxnDate = statusTime(order), 
					TxnQty = quantity(order), 
					TxnPrice = execution_price(order), 
					TxnFees = txnFees(order), 
					verbose = verbose)
			
		}
	}
	
	
}
















