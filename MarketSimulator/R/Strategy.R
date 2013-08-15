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
				targets[instrument] <- Target(instrument, size)
			}
			
			return(targets)
		})



#2007-01-05: buy 999 AMP.AX
#2007-01-09: sell all (-999) AMP.AX
#2007-02-01: buy 989 AMP.AX
#2007-02-06: sell 0 AMP.AX
#2007-02-07: sell -1 AMP.AX
#2007-02-08: sell all (-988) AMP.AX
#2007-02-16: buy 981 AMP.AX
#2007-02-19: sell all (-981) AMP.AX

