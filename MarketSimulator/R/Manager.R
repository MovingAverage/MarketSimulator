#'
#'
setClass("Manager",
		representation(
			strategy = "ANY", 
			cost.model = "function", 
			cost.threshold = "numeric", 
			positions = "list", 
			latest.prices = "numeric", 
			account = "Account"
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

setupAccount <- function(manager, starting.equity) {
	manager@account <- Account(starting.equity)
	return(manager)
}

setMethod("updateAccount",
		signature("Manager"),
		function(object, transactions) {
			object@account <- updateAccount(object@account, transactions)
			return(object)
		})

currentEquity <- function(manager) {
	cash <- cashIn(manager@account)
	position.list <- positions(manager)
	names(position.list) <- NULL
	values <- sapply(position.list, heldValue, 
			latest.prices = latestPrices(manager))
	return(cash + sum(values))
}

positions <- function(manager) {
	manager@positions
}

"positions<-" <- function(manager, value) {
	manager@positions <- value
	return(manager)
}

getPosition <- function(manager, instrument) {
	return(manager@positions[[instrument]])
}

setPosition <- function(manager, position) {
	manager@positions[instrumentOf(position)] <- position
	return(manager)
}

setMethod("activeInstruments",
		signature("Manager"),
		function(object) {
			sapply(object@positions, instrumentOf)
		})

addTarget <- function(manager, target) {
	position <- getPosition(manager, instrumentOf(target))
	if (is.null(position)) {
		position <- Position(instrumentOf(target))
	}
	position <- setTarget(position, target)
	manager <- setPosition(manager, position)
	return(manager)
}

setMethod("latestPrices",
		signature("Manager"),
		function(object) {
			object@latest.prices
		})

"latestPrices<-" <- function(positions, value) {
	positions@latest.prices <- value
	return(positions)
}

isViable <- function(manager, position) {
	value <- transactionValue(position, manager)
	cost <- tradeCost(manager, value)
	(cost / abs(value)) < manager@cost.threshold
}

tradeCost <- function(manager, value) {
	return(manager@cost.model(value))
}

placeOrders <- function(manager, broker, timestamp) {
	for (target in targetPositions(manager, timestamp)) {
		manager <- addTarget(manager, target)
	}
	for (position in positions(manager)) {
		if (isViable(manager, position)) {
			size <- transactionSize(position, manager)
			position <- sendOrders(position, size, broker)
			manager <- setPosition(manager, position)
		}
	}
	return(manager)
}

updateRecords <- function(manager, broker) {
	transactions <- getTransactions(broker)
	clearTransactions(broker)
	latestPrices(manager) <- latestPrices(broker)
	manager <- updateAccount(manager, transactions)
	for (position in positions(manager)) {
		position <- updateSize(position, transactions)
		position <- updateOrders(position, broker)
		manager <- setPosition(manager, position)
	}
	return(manager)
}

sendNotices <- function(manager, timestamp) {
	notices <- notices(manager)
	if (nchar(notices) > 0) {
		message(paste0(timestamp, ": ", notices))
	}
	manager <- clearNotices(manager)
	return(manager)
}

notices <- function(manager) {
	position.list <- manager@positions
	notices <- lapply(position.list, notice)
	return(paste(notices[sapply(notices, length) != 0], collapse = ", "))
}

setMethod("clearNotices",
		signature("Manager"),
		function(object) {
			object@positions <- lapply(object@positions, clearNotices)
			return(object)
		})





























