#' 
#' State object controlling the order status
setClass("OrderStatus")

setGeneric("callUpdateProcedure",
		function(status, order, broker) {
			standardGeneric("callUpdateProcedure")
		})

setGeneric("status",
		function(object) {
			standardGeneric("status")
		})

#'
setClass("OpenStatus",
		contains = "OrderStatus")

OpenStatus <- function() {
	new("OpenStatus")
}

setMethod("status",
		signature(object = "OpenStatus"),
		function(object) {
			return("open")
		})

setMethod("callUpdateProcedure",
		signature(status ="OpenStatus"),
		function(status, order, broker) {
			replaceOrder(broker, order)
		})

#'
setClass("ClosedStatus",
		contains = "OrderStatus")

ClosedStatus <- function() {
	new("ClosedStatus")
}

setMethod("status",
		signature(object = "ClosedStatus"),
		function(object) {
			return("closed")
		})

setMethod("callUpdateProcedure",
		signature(status ="ClosedStatus"),
		function(status, order, broker) {
			closeOrder(broker, order)
		})

#'
setClass("NullStatus",
		contains = "OrderStatus")

NullStatus <- function() {
	new("NullStatus")
}

setMethod("status",
		signature(object = "NullStatus"),
		function(object) {
			return("cancelled")
		})

setMethod("callUpdateProcedure",
		signature(status = "NullStatus"),
		function(status, order, broker) {
			cancelOrder(broker, order)
		})
