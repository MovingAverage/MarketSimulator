#'
#'
setClass("CurrentPosition",
		representation(
			instrument = "character",
			state = "PositionState", 
			orders = "list", 
			size = "numeric", 
			target = "TargetPositionInterface", 
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
	position@state <- Open()
	position@orders <- orders
	position@size <- size
	return(position)
}

list_objects_inherit_from <- function(parent.class, list) {
	return(sapply(list, function(x) inherits(x, parent.class)))
}

setTarget <- function(position, target) {
	
	old.target <- getTarget(position)
	position@target <- target
	
	if (!identical(sizeOf(old.target), sizeOf(target))) {
		if (sizeOf(old.target) != 0) {
			if (sizeOf(target) == 0) {
				status(position) <- Closed()
			} else {
				if (!identical(status(position), Stopped())) {
					status(position) <- Adjusting()
				}
			}
		} else {
			if (sizeOf(target) != 0) {
				status(position) <- Open()
			}
		}
	}
	
	return(position)
}

getTarget <- function(position) {
	position@target
}

setMethod("status",
		signature("CurrentPosition"),
		function(object) {
			return(object@state)
		})

setMethod("status<-",
		signature("CurrentPosition"),
		function(object, value) {
			object@state <- value
			return(object)
		})

ordersFor <- function(position) {
	position@orders
}

heldValue <- function(position, latest.prices) {
	sizeOf(position) * latest.prices[instrumentOf(position)]
}

totalSize <- function(position) {
	order.size <- orderSize(position)
	held.size <- sizeOf(position)
	return(order.size + held.size)
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
	
	size.orders <- !sapply(ordersFor(position), inherits, what = "StopLossOrder")
	size.orders <- ordersFor(position)[size.orders]
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
	
	transaction.instruments <- vapply(transactions, instrumentOf, character(1))
	relevant.transactions <- transaction.instruments == instrumentOf(position)
	transactions <- transactions[relevant.transactions]
	for (transaction in transactions) {
		new.size <- sizeOf(position) + quantity(transaction)
		position@size <- new.size
		if (new.size == 0 & inherits(transaction, "StopLossOrder")) {
			status(position) <- Stopped()
			message(paste(statusTime(transaction), ": Stopped:", 
							instrumentOf(transaction)))
		}
	}
	return(position)
}

targetSize <- function(position) {
	return(sizeOf(getTarget(position)))
}

targetQuantity <- function(position) {
	return(quantity(getTarget(position)))
}

transactionValue <- function(position, manager) {
	size <- transactionSize(position)
	price <- latestPrices(manager)[instrumentOf(position)]
	return(size * price)
}

transactionSize <- function(position) {
	return(targetQuantity(position) - sizeOf(position))
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

setMethod("show",
		signature("CurrentPosition"),
		function(object) {
			
			pos <- list(
					instrument = instrumentOf(object),
					state = class(status(object)), 
					current.size = sizeOf(object), 
					target.size = quantity(getTarget(object)), 
					stop.point = getTarget(object)@stop.point
					)
			
			pos <- pos[sapply(pos, length) == 1 & sapply(pos, class) != "S4"]
			
			print("Position:")
			show(as.data.frame(pos))
			print("Related orders:")
			show(object@orders)
		})









