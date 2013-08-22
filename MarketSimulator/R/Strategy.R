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
		ema3 <- strategy@indicator(Cl(mktdata), n = 3)
		ema5 <- strategy@indicator(Cl(mktdata), n = 5)
		signals <- merge(signals, ema3 < ema5)
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
			if (length(positions)) {
				names(positions) <- object@instruments
				if (isTRUE(sum(positions) > 0)) {
					positions <- positions / sum(positions)
				}
			}
			
			targets <- list()
			for (instrument in names(positions)) {
				size <- as.numeric(positions[instrument])
				if (is.na(size)) size <- 0
				if (size > 0) {
					targets[instrument] <- Target(instrument, size, stop.point = 0.02)
				} else {
					targets[instrument] <- Target(instrument, size)
				}
				
			}
			
			return(targets)
		})














