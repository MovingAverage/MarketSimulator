#'
#'
setClass("Manager",
		representation(
			strategy = "ANY", 
			cost.model = "function", 
			cost.threshold = "numeric", 
			positions = "Positions"
		))
		
Manager <- function(strategy) {
	manager <- new("Manager")
	manager@strategy <- strategy
	manager@cost.model <- function(value) {
		max(6, abs(value) * 0.0008)
	}
	manager@cost.threshold <- 0.002
	return(manager)
}

setMethod("targetPositions",
		signature("Manager"),
		function(object, timestamp) {
			targetPositions(object@strategy, timestamp)
		})

isViable <- function(manager, positions, target) {
	positions <- addTarget(positions, target)
	value <- transactionValue(positions, instrumentOf(target))
	cost <- tradeCost(manager, value)
	(cost / abs(value)) < manager@cost.threshold
}

tradeCost <- function(manager, value) {
	return(manager@cost.model(value))
}

placeOrders <- function(manager, broker, timestamp) {
	
	targets <- targetPositions(manager, timestamp)
	positions <- manager@positions
	
	for (target in targets) {
		if (isViable(manager, positions, target)) {
			positions <- addTarget(positions, target)
			positions <- fireOrder(positions, broker, instrumentOf(target))
		}
	}
	manager@positions <- positions
	return(manager)
}

updatePositions <- function(manager, broker) {
	transactions <- transactions(broker)
	clearTransactions(broker)
	manager <- updateAccount(manager, transactions)
	positions <- manager@positions
	latestPrices(positions) <- latestPrices(broker)
	for (instrument in activeInstruments(positions)) {
		position <- getPosition(positions, instrument)
		position <- updateSize(position, transactions)
		position <- updateOrders(position, broker)
		positions <- setPosition(positions, position)
	}
	manager@positions <- positions
	return(manager)
}

sendNotices <- function(manager, timestamp) {
	notices <- notices(manager@positions)
	if (nchar(notices) > 0) {
		message(paste0(timestamp, ": ", notices))
	}
	manager <- clearNotices(manager)
	return(manager)
}

setMethod("clearNotices",
		signature("Manager"),
		function(object) {
			object@positions <- clearNotices(object@positions)
			return(object)
		})

setupAccount <- function(manager, starting.equity) {
	manager@positions <- PositionSet(Account(starting.equity), numeric())
	return(manager)
}

setMethod("updateAccount",
		signature("Manager"),
		function(object, transactions) {
			object@positions <- updateAccount(object@positions, transactions)
			return(object)
		})

































