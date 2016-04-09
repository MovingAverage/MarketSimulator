#'
#'
setClass("TargetPosition",
		representation(
				instrument = "character", 
				size = "numeric", 
				stop.point = "numeric", 
				quantity = "integer"
		), 
		contains = "TargetPositionInterface")

#' Create a TargetPosition object
#' 
#' This is the basic construction function for creating TargetPosition objects. It is 
#' used by Strategy objects to communicate the desired target sizes and other parameters.
#' 
Target <- function(instrument, size, stop.point = numeric()) {
	
	target <- new("TargetPosition")
	target@instrument <- instrument
	target@size <- size
	target@stop.point <- stop.point
	return(target)
}

setMethod("makeOrder",
		signature(target = "TargetPosition"),
		function(target, size) {
			if (length(target@stop.point) == 0 || size != quantity(target)) {
				if (size > 0) {
					order <- Order(instrumentOf(target), buy = size)
				} else {
					order <- Order(instrumentOf(target), sell = size)
				}
			} else {
				if (size > 0) {
					order <- MarketWithStop(instrumentOf(target), buy = size,  
							stop.point = target@stop.point)
				} else {
					order <- MarketWithStop(instrumentOf(target), sell = size, 
							stop.point = target@stop.point)
				}
			}
			return(order)
		})

asDataFrame.Target <- function(x) {
	target <- list(
			instrument = instrumentOf(x), 
			size = sizeOf(x), 
			stop.point = x@stop.point, 
			quantity = quantity(x))
	
	target <- target[sapply(target, length) == 1 & sapply(target, class) != "S4"]
	
	as.data.frame(target)
}




setMethod("show",
		signature("TargetPosition"),
		function(object) {
			show(asDataFrame.Target(object))
		})


#' Breakout Target Position
#' 
#' This target position will enter the position if the price exceeds the limit set either
#' at the open or high (low) of the trading day. This is used to enter the position only 
#' if the price continues higher for example.
#' 
setClass("BreakoutTargetPosition",
		representation(
				limit = "xts"
		),
		contains = "TargetPosition")

BreakoutTarget <- function(instrument, size, limit) {
	
	target <- new("BreakoutTargetPosition")
	target@instrument <- instrument
	target@size <- size
	target@limit <- limit
	return(target)
}

setMethod("makeOrder",
		signature(target = "BreakoutTargetPosition"),
		function(target, size) {
			if (size > 0) {
				order <- BreakoutLimit(instrumentOf(target), buy = size, 
						at = target@limit)
			} else {
				order <- Order(instrumentOf(target), sell = size)
			}
		})






















