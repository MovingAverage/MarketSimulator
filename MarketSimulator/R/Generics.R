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

setGeneric("status",
		function(object) {
			standardGeneric("status")
		})

setGeneric("status<-",
		function(object, value) {
			standardGeneric("status<-")
		})

setGeneric("quantity",
		function(object) {
			return(object@quantity)
		})

setGeneric("quantity<-", 
		function(object, value) {
			object@quantity <- as.integer(value)
			return(object)
		})

instrumentOf <- function(object, ...) {
			return(object@instrument)
		}
		
sizeOf <- function(object) {
			size <- object@size
			names(size) <- instrumentOf(object)
			return(size)
		}		
		
