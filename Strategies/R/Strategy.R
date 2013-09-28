#'
#'
setClass("Strategy",
		representation(
			indicator = "function",
			stop.point = "numeric", 
			instruments = "character", 
			signals = "xts", 
			positions = "xts", 
			primed = "logical"
		), 
		contains = "StrategyInterface")

#' Create a crossover strategy
#' 
#' @export
Crossover <- function(indicator, fast.pd, slow.pd, stop.point = numeric()) {
	
	force(indicator)
	force(fast.pd)
	force(slow.pd)
	
	strategy <- new("Strategy")
	
	indicator.fun <- function(mktdata) {
		fast <- indicator(Cl(mktdata), n = fast.pd)
		slow <- indicator(Cl(mktdata), n = slow.pd)
		return(fast < slow)
	}
	
	strategy@indicator <- indicator.fun
	strategy@stop.point <- stop.point
	return(strategy)
}

positions <- function(strategy) {
	return(strategy@positions)
}

"positions<-" <- function(strategy, value) {
	strategy@positions <- value
	return(strategy)
}

index.Strategy <- function(x, ...) {
	index(x@positions)
}

#' Prime the strategy with signal and positions
#' 
#' Calculates the signal and position information with the market data information 
#' provided. When called the strategy indicator function is called with the market
#' data for each instrument in the supplied Market. Typically the strategy will be primed
#' before conducting a backtest on the market supplied.
#' 
setMethod("primeStrategy",
		signature(strategy = "Strategy"),
		function(strategy, market) {
			signals <- xts()
			strategy@instruments <- tradeableInstruments(market)
			
			for (instrument in strategy@instruments) {
				mktdata <- market@instruments[[instrument]]
				signals <- merge(signals, strategy@indicator(mktdata))
			}
			names(signals) <- strategy@instruments
			strategy@signals <- signals
			positions(strategy) <- signals * 1
			return(strategy)
		})

#' Return the Target Positions for the day
#' 
#' A list of Target positions is returned for each instrument to be adjusted.
#' 
setMethod("getTargets",
		signature(strategy = "Strategy"),
		function(strategy, timestamp) {
			positions <- positionSizes(strategy, timestamp)
			positions <- selectPositions(positions, strategy)
			targets <- createTargets(positions, strategy)
			return(targets)
		})

setGeneric("positionSizes",
		function(strategy, timestamp, ...) {
			standardGeneric("positionSizes")
		})

setMethod("positionSizes",
		signature(strategy = "Strategy"),
		function(strategy, timestamp) {
			positions <- as.numeric(positions(strategy)[as.character(timestamp)])
			if (length(positions)) {
				names(positions) <- strategy@instruments
			}
			return(positions)
		})

selectPositions <- function(positions, strategy) {
	if (isTRUE(sum(positions) > 0)) {
		positions <- positions / sum(positions != 0)
	}
	return(positions)
}

createTargets <- function(positions, strategy) {
	targets <- list()
	for (instrument in names(positions)) {
		size <- as.numeric(positions[instrument])
		if (!is.na(size)) {
			if (size > 0) {
				targets[instrument] <- Target(instrument, size, 
						stop.point = strategy@stop.point)
			} else {
				targets[instrument] <- Target(instrument, size)
			}
		}
	}
	return(targets)
}









