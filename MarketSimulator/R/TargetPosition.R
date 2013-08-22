#'
#'
setClass("TargetPosition",
		representation(
				instrument = "character", 
				size = "numeric", 
				stop.point = "numeric"
		))

Target <- function(instrument, size, stop.point = numeric()) {
	
	target <- new("TargetPosition")
	target@instrument <- instrument
	target@size <- size
	target@stop.point <- stop.point
	return(target)
}

sizeOf <- function(position) {
	size <- position@size
	names(size) <- instrumentOf(position)
	return(size)
}

makeOrder <- function(target, size) {
	if (length(target@stop.point) == 0) {
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
}















