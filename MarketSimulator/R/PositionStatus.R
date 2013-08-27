#'
#' 
setClass("PositionState", 
		contains = "VIRTUAL")
		
setGeneric("sendOrders",
		function(state, position, broker) {
			standardGeneric("sendOrders")
		})


#' Open position
#' 
setClass("OpenPositionState",
		contains = "PositionState")

Open <- function() {
	return(new("OpenPositionState"))
}

setMethod("sendOrders",
		signature("OpenPositionState"),
		function(state, position, broker) {
			if (orderSize(position) == 0) {
				size <- transactionSize(position)
				order <- makeOrder(getTarget(position), size)
				addOrder(broker, order)
				position <- addNotice(position, size)
			} else {
				adjustOrders(position, broker)
				adjustStops(position, broker)
			}
			return(position)
		})


#' Adjusting position
#' 
setClass("ClosedPositionState",
		contains = "PositionState")
		
Closed <- function() {
	return(new("ClosedPositionState"))
}

setMethod("sendOrders",
		signature("ClosedPositionState"),
		function(state, position, broker) {
			if (totalSize(position) != 0) {
				cancel_all_open_orders(position, broker)
				order <- makeOrder(getTarget(position), -sizeOf(position))
				addOrder(broker, order)
				position <- addNotice(position, -sizeOf(position))
			}
			return(position)
		})

cancel_all_open_orders <- function(position, broker) {
	orders <- ordersFor(position)
	orders <- lapply(orders, "status<-", NullStatus())
	lapply(orders, updateOrder, broker)
}


#' Filled position
#' 
setClass("FilledPositionState",
		contains = "PositionState")
		
Filled <- function() {
	return(new("FilledPositionState"))
}

setMethod("sendOrders",
		signature("FilledPositionState"),
		function(state, position, broker) {
			return(position)
		})


#' Stopped position
#' 
setClass("StoppedPositionState",
		contains = "PositionState")

Stopped <- function() {
	return(new("StoppedPositionState"))
}

setMethod("sendOrders",
		signature("StoppedPositionState"),
		function(state, position, broker) {
			cancel_all_open_orders(position, broker)
			return(position)
		})


#' Adjusting position
#' 
setClass("AdjustingPositionState",
		contains = "PositionState")
		
Adjusting <- function() {
	return(new("AdjustingPositionState"))
}

setMethod("sendOrders",
		signature("AdjustingPositionState"),
		function(state, position, broker) {
			transaction.size <- transactionSize(position)
			if (orderSize(position) == 0) {
				order <- makeOrder(getTarget(position), transaction.size)
				addOrder(broker, order)
				position <- addNotice(position, transaction.size)
				adjustStops(position, broker)
			} else {
				adjustOrders(position, broker)
				adjustStops(position, broker)
			}
			return(position)
		})

adjustOrders <- function(position, broker) {
	orders <- ordersFor(position)
	transact.qty <- transactionSize(position)
	stops <- vapply(orders, inherits, logical(1), "StopLossOrder")
	orders <- orders[!stops]
	order.quantities <- sapply(orders, quantity)
	missized.orders <- orders[order.quantities != transact.qty]
	lapply(missized.orders, replace_order_quantity, qty = transact.qty, broker = broker)
}

adjustStops <- function(position, broker) {
	target.qty <- -1 * targetQuantity(position)
	orders <- ordersFor(position)
	stop.orders <- orders[vapply(orders, inherits, logical(1), "StopLossOrder")]
	stop.quantities <- sapply(stop.orders, quantity)
	missized.stops <- stop.orders[stop.quantities != target.qty]
	lapply(missized.stops, replace_order_quantity, qty = target.qty, broker = broker)
}

replace_order_quantity <- function(order, qty, broker) {
	quantity(order) <- qty
	updateOrder(order, broker)
}










