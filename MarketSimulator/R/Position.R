#'
#'
setClass("CurrentPosition",
		representation(
			instrument = "character",
			orders = "list", 
			size = "numeric", 
			target = "TargetPosition", 
			notice = "character"
		))

Position <- function(instrument, orders = list(), size = 0) {
	position <- new("CurrentPosition")
	position@instrument <- instrument
	
	if (length(orders)) {
		if (!all(list_objects_inherit_from("Order", orders))) {
			stop("orders must inherit from class Order")
		}
		if (!all(sapply(orders, instrumentOf) == instrument)) {
			stop("orders must be related to instrument")
		}
	}
	
	position@orders <- orders
	position@size <- size
	return(position)
}

list_objects_inherit_from <- function(parent.class, list) {
	return(sapply(list, function(x) inherits(x, parent.class)))
}

totalSize <- function(object) {
			order.size <- orderSize(object)
			held.size <- heldSize(object)
			return(order.size + held.size)
		}

heldSize <- function(position) {
	size <- position@size
	if (is.na(size)) {
		size <- 0
	}
	names(size) <- instrumentOf(position)
	return(size)
}

updateSize <- function(position, transactions) {
	instrument <- instrumentOf(position)
	if (instrument %in% row.names(transactions)) {
		new.size <- heldSize(position) + transactions[instrument, "size"]
		position@size <- new.size
	}
	return(position)
}

targetSize <- function(position) {
	size <- sizeOf(position@target)
	if (is.na(size)) {
		size <- 0
		names(size) <- instrumentOf(position)
	}
	return(size)
}

heldValue <- function(position, latest.prices, equity) {
	heldSize(position) * latest.prices[instrumentOf(position)]
}

heldFraction <- function(position, latest.prices, equity) {
	heldValue(position, latest.prices) / equity
}

orderSize <- function(position) {
	size <- 0
	names(size) <- instrumentOf(position)
	market.order <- market_order(position)
	if (length(market.order)) {
		size <- size + quantity(market.order)
	}
	return(size)
}

market_order <- function(position) {
	
	market.order <- position@orders[sapply(position@orders, class) == "MarketOrder"]
	if (length(market.order) > 1) {
		stop("More than one market order found for instrument")
	}
	if (length(market.order) == 1) {
		return(market.order[[1]])
	} else {
		return(market.order)
	}
}

updateOrders <- function(position, broker) {
	position@orders <- openOrders(broker, instrumentOf(position))
	return(position)
}


sendOrders <- function(position, size, broker) {
	if (orderSize(position) == 0) {
		order <- makeOrder(position, size)
		addOrder(broker, order)
		position <- addNotice(position, size)
	} else {
		order <- position@orders[[1]]
		if (quantity(order) != as.integer(size)) {
			quantity(order) <- size
			updateOrder(order, broker)
		}
	}
	return(position)
}

makeOrder <- function(position, size) {
	if (size > 0) {
		order <- Order(instrumentOf(position), buy = size)
	} else {
		order <- Order(instrumentOf(position), sell = size)
	}
	return(order)
}

notice <- function(position) {
	position@notice
}

setMethod("clearNotices",
		signature("CurrentPosition"),
		function(object) {
			object@notice <- character()
			return(object)
		})

addNotice <- function(position, size) {
	instrument <- instrumentOf(position)
	if (size > 0) {
		notice <- buyNotice(size, instrument)
	} else {
		if (targetSize(position) == 0) {
			notice <- sellAllNotice(size, instrument)
		} else {
			notice <- sellNotice(size, instrument)
		}
	}
	position@notice <- notice
	return(position)
}

buyNotice <- function(position, instrument) {
	paste("buy", position, instrument)
}

sellAllNotice <- function(sell.size, instrument) {
	paste0("sell all (", sell.size, ") ", instrument)
}

sellNotice <- function(position, instrument) {
	paste("sell", position, instrument)
}


#' List of positions
#' 
setClass("Positions",
		representation(
			positions = "list", 
			account = "Account", 
			latest.prices = "numeric"
		))

PositionSet <- function(account, latest.prices) {
	positions <- new("Positions")
	positions@account <- account
	positions@latest.prices <- latest.prices
	return(positions)
}

addPosition <- function(positions, instrument, orders = list(), size = 0) {
	position.list <- positions@positions
	position.list[instrument] <- Position(instrument, orders, size)
	positions@positions <- position.list
	return(positions)
}

getPosition <- function(positions, instrument) {
	return(positions@positions[[instrument]])
}

setPosition <- function(positions, position) {
	positions@positions[instrumentOf(position)] <- position
	return(positions)
}

setMethod("activeInstruments",
		signature("Positions"),
		function(object) {
			sapply(object@positions, instrumentOf)
		})

setMethod("updateAccount",
		signature("Positions"),
		function(object, transactions) {
			object@account <- updateAccount(object@account, transactions)
			return(object)
		})

currentEquity <- function(positions) {
			cash <- cashIn(positions@account)
			position.list <- positions@positions
			names(position.list) <- NULL
			values <- sapply(position.list, heldValue, 
					latest.prices = latestPrices(positions))
			return(cash + sum(values))
		}

setMethod("latestPrices",
		signature("Positions"),
		function(object) {
			object@latest.prices
		})

"latestPrices<-" <- function(positions, value) {
	positions@latest.prices <- value
	return(positions)
}

transactionValue <- function(positions, instrument) {
	equity <- currentEquity(positions)
	position <- getPosition(positions, instrument)
	held.fraction <- heldFraction(position, latestPrices(positions), equity)
	change <- targetSize(position) - held.fraction
	return(equity * change)
}

transactionSize <- function(positions, instrument) {
	size <- transactionValue(positions, instrument) / latestPrices(positions)[instrument]
	size <- as.integer(size)
	names(size) <- instrument
	return(size)
}

addTarget <- function(positions, target) {
	position <- getPosition(positions, instrumentOf(target))
	if (is.null(position)) {
		position <- Position(instrumentOf(target))
	}
	position@target <- target
	positions <- setPosition(positions, position)
	return(positions)
}

fireOrder <- function(positions, broker, instrument) {
	position <- getPosition(positions, instrument)
	size <- transactionSize(positions, instrument)
	position <- sendOrders(position, size, broker)
	positions <- setPosition(positions, position)
	return(positions)
}

notices <- function(positions) {
	position.list <- positions@positions
	notices <- lapply(position.list, notice)
	return(paste(notices[sapply(notices, length) != 0], collapse = ", "))
}

setMethod("clearNotices",
		signature("Positions"),
		function(object) {
			object@positions <- lapply(object@positions, clearNotices)
			return(object)
		})






















