#'
#'

setClass("Strategy",
		representation(
			indicator = "function",
			instrument = "character", 
			signal = "xts", 
			positions = "xts"
		))
		

strategySetup <- function(strategy, market) {
	
	mktdata <- market@instruments[[strategy@instrument]]
	ema5 <- strategy@indicator(Cl(mktdata), n = 5)
	ema10 <- strategy@indicator(Cl(mktdata), n = 10)
	strategy@signal <- ema5 < ema10
	strategy@positions <- strategy@signal * 1
	return(strategy)
}

setMethod("targetPositions",
		signature("Strategy"),
		function(object, timestamp) {
			position <- as.numeric(object@positions[timestamp])
			if (length(position)) {
				names(position) <- object@instrument
			}
			return(position)
		})


#2008-04-14: buy 1133
#2008-04-19: no target
#2008-04-20: no target
#2008-04-22: sell all 1133
#2008-04-24: buy 1166
#2008-04-25: buy 1166
#2008-04-26: no target
#2008-04-27: no target
#2008-04-28: non-finite position
#2008-04-29: sell -1177.73582474227
#2008-04-30: sell -2398.72924648787