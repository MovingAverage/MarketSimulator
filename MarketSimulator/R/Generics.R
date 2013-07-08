#'
#' These generics are shared by multiple files. Moved here so that the generic is 
#' loaded first before the methods, to avoid "compiling" errors.
#' 


setGeneric("getBar",
		function(object, instrument, timestamp, ...) {
			standardGeneric("getBar")
		})

initDate <- function() {
	return(as.POSIXct("1999-12-31"))
}
