#'
#'
setClass("TargetPositionInterface",
		contains = "VIRTUAL")
		
setGeneric("makeOrder",
		function(target = "TargetPositionInterface", size) {
			standardGeneric("makeOrder")
		}, 
		valueClass = "Order")
