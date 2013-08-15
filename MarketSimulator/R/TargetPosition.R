#'
#'
setClass("TargetPosition",
		representation(
				instrument = "character", 
				size = "numeric"
		))

sizeOf <- function(position) {
	size <- position@size
	names(size) <- instrumentOf(position)
	return(size)
}

Target <- function(instrument, size) {
	
	target <- new("TargetPosition")
	target@instrument <- instrument
	target@size <- size
	return(target)
}


