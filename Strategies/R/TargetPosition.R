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















