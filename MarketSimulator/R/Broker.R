#'
#'

setClass("Broker",
		representation(
			market = "list", 
			orders = "list"
		))
		
addMarket <- function(broker, market) {
			broker@market <- market
			return(broker)
		}

getBar <- function(broker, instrument, timestamp) {
	instrument <- getMarketInstrument(broker@market, instrument)
	return(instrument[timestamp])
	
}

addOrder <- function(broker, order) {
	if (!inherits(order, "MarketOrder")) {
		stop("Only orders derived from class 'MarketOrder' may be added to Broker")
	}
	broker@orders <- c(broker@orders, list(order))
	return(broker)
}

notifyOrders <- function(broker, price.bar) {
	
	for (order in broker@orders) {
		outcome <- notifyOrder(order, broker, price.bar)
	}
	
}
