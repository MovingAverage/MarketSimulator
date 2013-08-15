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

zero_named_vector <- function(names) {
	vec <- numeric(length(names))
	names(vec) <- names
	return(vec)
}

setGeneric("tradeableInstruments",
		function(object, ...) {
			standardGeneric("tradeableInstruments")
		})

setGeneric("targetPositions",
		function(object, timestamp) {
			standardGeneric("targetPositions")
		})

setGeneric("activeInstruments",
		function(object, ...) {
			standardGeneric("activeInstruments")
		})

setGeneric("latestPrices",
		function(object, ...) {
			standardGeneric("latestPrices")
		})

setGeneric("clearNotices",
		function(object) {
			standardGeneric("clearNotices")
		})

instrumentOf <- function(object, ...) {
			return(object@instrument)
		}

		
		
