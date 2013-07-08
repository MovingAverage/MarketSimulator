#'
#'

Manager <- function(strategy) {
	return(NULL)
}

getPositions <- function(manager, strategy) {
	positions(strategy)
}


balancePositions <- function(current.positions, ideal.positions) {
	
	percentage.commission <- 0.0008
	minimum.commission <- 6
	cost.threshold <- 0.002
	capital <- 10000
	
	size.of.change <- abs(ideal.positions - current.positions) * capital
	cost <- pmax(minimum.commission, size.of.change * percentage.commission)
	relative.cost <- cost / size.of.change
	not.effective <- relative.cost > cost.threshold
	ideal.positions[not.effective] <- current.positions[not.effective]
	return(ideal.positions)
}

createOrders <- function(manger, broker, changes) {
	
	current.equity <- currentEquity(broker)
	latest.prices <- latestPrices(broker)
	
	for (instrument in names(changes)) {
		position.size <- current.equity * changes[instrument] / latest.prices[instrument]
		order <- Order(instrument, buy = position.size)
		addOrder(broker, order)
	}
}




BackTest <- function(manager, broker, start.date, end.date) {
	
	for (timestamp in as.character(seq.Date(start.date, end.date, by = 1))) {
		timestamp <- as.Date(timestamp)
		placeOrders(manager, broker, timestamp)
		marketActivity(broker, timestamp)
	}
	return(broker)
}

placeOrders <- function(manager, broker, timestamp) {
	
	if (timestamp == as.Date("2013-06-07")) {
		addOrder(broker, Order("AMP.AX", buy = 100))
	}
	if (timestamp == as.Date("2013-06-13")) {
		addOrder(broker, Order("AMP.AX", sell = 100))
	}
}
