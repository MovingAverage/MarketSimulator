#'
#'
setClass("CurrentPosition",
		representation(
			instrument = "character",
			status = "character", 
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
	position@target <- Target(instrument, size = 0)
	position@status <- Open()
	position@orders <- orders
	position@size <- size
	return(position)
}

list_objects_inherit_from <- function(parent.class, list) {
	return(sapply(list, function(x) inherits(x, parent.class)))
}

setTarget <- function(position, target) {
	
	old.target <- position@target
	position@target <- target
	
	if (sizeOf(old.target) == 0 && sizeOf(target) != 0) {
		position@status <- Open()
	}
	
	return(position)
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

orderSize <- function(position) {
	size <- 0
	names(size) <- instrumentOf(position)
	market.order <- size_orders(position)
	if (length(market.order)) {
		size <- size + quantity(market.order)
	}
	return(size)
}

size_orders <- function(position) {
	
	size.orders <- !sapply(position@orders, inherits, what = "StopLossOrder")
	size.orders <- position@orders[size.orders]
	if (length(size.orders) > 1) {
		stop("More than one market order found for instrument")
	}
	if (length(size.orders) == 1) {
		return(size.orders[[1]])
	} else {
		return(size.orders)
	}
}

updateOrders <- function(position, broker) {
	position@orders <- openOrders(broker, instrumentOf(position))
	return(position)
}

updateSize <- function(position, transactions) {
	
	for (transaction in transactions) {
		if (instrumentOf(position) == instrumentOf(transaction)) {
			new.size <- heldSize(position) + quantity(transaction)
			position@size <- new.size
			if (new.size == 0 & inherits(transaction, "StopLossOrder")) {
				position@status <- Stopped()
				message(paste("Stopped:", instrumentOf(transaction)))
			}
		}
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

heldValue <- function(position, latest.prices) {
	heldSize(position) * latest.prices[instrumentOf(position)]
}

heldFraction <- function(position, latest.prices, equity) {
	heldValue(position, latest.prices) / equity
}

transactionValue <- function(position, manager) {
	equity <- currentEquity(manager)
	held.fraction <- heldFraction(position, latestPrices(manager), equity)
	change <- targetSize(position) - held.fraction
	return(equity * change)
}

transactionSize <- function(position, manager) {
	size <- transactionValue(position, manager) / 
			latestPrices(manager)[instrumentOf(position)]
	size <- as.integer(size)
	names(size) <- instrumentOf(position)
	return(size)
}

sendOrders <- function(position, size, broker) {
	
	if (status(position)!= Stopped()) {
		if (orderSize(position) == 0) {
			
			if (targetSize(position) == 0) {
				for (order in position@orders) {
					order@status <- NullStatus()
					updateOrder(order, broker)
				}
			}
			
			order <- makeOrder(position@target, size)
			addOrder(broker, order)
			position <- addNotice(position, size)
		} else {
			order <- position@orders[[1]]
			if (quantity(order) != as.integer(size)) {
				quantity(order) <- size
				updateOrder(order, broker)
			}
		}
	}
	return(position)
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
			notice <- sellAllNotice(abs(size), instrument)
		} else {
			notice <- sellNotice(abs(size), instrument)
		}
	}
	position@notice <- notice
	return(position)
}

buyNotice <- function(size, instrument) {
	paste("buy", size, instrument)
}

sellAllNotice <- function(size, instrument) {
	paste0("sell all (", size, ") ", instrument)
}

sellNotice <- function(size, instrument) {
	paste("sell", size, instrument)
}


#' Position Status
Stopped <- function() {
	return("stopped")
}

Open <- function() {
	return("open")
}

setMethod("status",
		signature("CurrentPosition"),
		function(object) {
			return(object@status)
		})











