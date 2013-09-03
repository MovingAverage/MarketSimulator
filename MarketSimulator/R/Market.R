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

active_market <- function(price.bar) {
	volume.ok <- not_NA_or_Zero(Vo(price.bar))
	open.ok <- not_NA_or_Zero(Op(price.bar))
	close.ok <- not_NA_or_Zero(Cl(price.bar))
	return(volume.ok & open.ok & close.ok)
}

not_NA_or_Zero <- function(value) {
	isTRUE(!(is.na(value) || value == 0))
}

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

dailyReturns <- function(market, type = "arithmetic") {
	
	if (!type %in% c("arithmetic", "log")) {
		stop("Type must be 'arithmetic' or 'log'")
	}
	
	returns <- xts()
	
	for (instrument in tradeableInstruments(market)) {
		mktdata <- market@instruments[[instrument]]
		returns <- merge(returns, dailyReturn(mktdata, type = type))
	}
	names(returns) <- tradeableInstruments(market)
	return(returns)
}
















