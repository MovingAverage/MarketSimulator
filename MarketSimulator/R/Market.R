#'
#'
setClass("Market",
		representation(
			instruments = "list"
		))
		
Market <- function(instruments) {
	
	if (missing(instruments)) {
		instruments <- list()
	}
	market <- new("Market")
	market@instruments <- instruments
	return(market)
}

setMethod("getBar",
		signature("Market"),
		function(object, instrument, timestamp) {
			instrument <- object@instruments[[instrument]]
			return(instrument[timestamp])
		})
