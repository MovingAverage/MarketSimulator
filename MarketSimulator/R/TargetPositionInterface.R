#'
#'
setClass("TargetPositionInterface",
		contains = "VIRTUAL")
		
setGeneric("makeOrder",
		function(target, size) {
			standardGeneric("makeOrder")
		}, 
		valueClass = "Order")

setClass("EmptyTarget",
		representation(
			instrument = "character",	
			size = "numeric"
		),
		contains = "TargetPositionInterface")
		
EmptyTarget <- function(instrument) {
	target <- new("EmptyTarget")
	target@instrument <- instrument
	target@size <- 0
	return(target)
}

