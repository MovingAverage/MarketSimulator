#'
#'

setClass("Strategy",
		representation(
			indicator = "function",
			instruments = "character", 
			signals = "xts", 
			positions = "xts"
		))
		
strategySetup <- function(strategy, market) {
	
	signals <- xts()
	
	for (instrument in strategy@instruments) {
		mktdata <- market@instruments[[instrument]]
		ema5 <- strategy@indicator(Cl(mktdata), n = 5)
		ema10 <- strategy@indicator(Cl(mktdata), n = 10)
		signals <- merge(signals, ema5 < ema10)
	}
	names(signals) <- strategy@instruments
	strategy@signals <- signals
	strategy@positions <- signals * 1
	return(strategy)
}

setMethod("targetPositions",
		signature("Strategy"),
		function(object, timestamp) {
			positions <- as.numeric(object@positions[timestamp])
			positions / sum(positions)
			if (length(positions)) {
				names(positions) <- object@instruments
			}
			return(positions)
		})

