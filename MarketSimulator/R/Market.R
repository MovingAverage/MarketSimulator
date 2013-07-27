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

setMethod("tradeableInstruments",
		signature("Market"),
		function(object) {
			# sort was throwing warning of little use about applying is.na to NULL object.
			return(suppressWarnings(sort(names(object@instruments))))
		})

setMethod("show",
		signature("Market"),
		function(object) {
			date <- function(instrument, when) as.character(when(instrument))
			market.frame <- data.frame(
					start = sapply(object@instruments, date, when = start), 
					end = sapply(object@instruments, date, when = end))
			show(market.frame)
		})

