#'
#'
setClass("Broker",
		representation(
			market = "Market", 
			orders = "environment"
		))

Broker <- function() {
	new("Broker", market = new("Market"), orders = new.env(emptyenv()))
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
	order.list <- getOrders(broker, instrumentOf(order))
	order <- setID(order, length(order.list) + 1)
	order.list <- c(order.list, list(order))
	assign(instrumentOf(order), order.list, broker@orders) 
}

updateOrder <- function(broker, order) {
	order.list <- getOrders(broker, instrumentOf(order))
	order.list[[getID(order)]] <- order
	assign(instrumentOf(order), order.list, broker@orders)
}

getOrders <- function(broker, instrument) {
	order.list <- tryCatch(
			get(instrument, broker@orders), 
			error = function(e) list())
	return(order.list)
}

notifyOrders <- function(broker, instrument, price.bar) {
	
	for (order in getOrders(broker, instrument)) {
		notify(order, broker, price.bar)
	}
}

instruments <- function(broker) {
	return(ls(broker@orders))
}

marketActivity <- function(broker, timestamp) {
	
	for (instrument in instruments(broker)) {
		price.bar <- getBar(broker, instrument, timestamp)
		notifyOrders(broker, instrument, price.bar)
	}
	return(broker)
}








