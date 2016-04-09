#'
#'
setClass("Strategy",
		representation(
				indicator = "function",
				instruments = "character", 
				signals = "xts", 
				positions = "xts", 
				target.builder = "TargetBuilder"
		), 
		contains = "StrategyInterface")

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

setMethod("show",
		signature("Strategy"),
		function(object) {
			date <- function(instrument, when) as.character(when(instrument))
			market.frame <- data.frame(
					start = sapply(object@instruments, date, when = start), 
					end = sapply(object@instruments, date, when = end))
			print("Show method for Strategy not finished")
			show(market.frame)
		})

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
				mktdata <- market[instrument]
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
			targets <- buildTargets(strategy@target.builder, positions, strategy)
			return(targets)
		})

setGeneric("positionSizes",
		function(strategy, timestamp, ...) {
			standardGeneric("positionSizes")
		})

setMethod("positionSizes",
		signature(strategy = "Strategy"),
		function(strategy, timestamp) {
			timestamp <- as.character(timestamp)
			positions <- as.numeric(positions(strategy)[timestamp])
			if (length(positions)) {
				names(positions) <- strategy@instruments
				positions[is.na(positions)] <- 0
			}
			attr(positions, "timestamp") <- timestamp
			return(positions)
		})


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
	strategy@target.builder <- TargetBuilder(, stop.point)
	return(strategy)
}

RSIreversal <- function(rsi.pd, entry.level, exit.level) {
	
	force(rsi.pd)
	force(entry.level)
	if (missing(exit.level)) {
		exit.level <- entry.level
	}
	
	strategy <- new("Strategy")
	
	indicator.fun <- function(mktdata) {
		rsi <- RSI(Cl(mktdata), n = rsi.pd)
		signal <- xts(rep(NA, length(index(rsi))), order.by = index(rsi))
		entries <- which(diff(rsi < entry.level) == 1)
		exits <- which(diff(rsi > exit.level) == 1)
		signal[entries] <- 1
		signal[exits] <- 0
		return(na.locf(signal))
	}
	
	strategy@indicator <- indicator.fun
	strategy@target.builder <- TargetBuilder()
	return(strategy)
}


setClass("LimitBreakoutStrategy",
		representation(
				limits = "xts", 
				target.builder = "BreakoutTargetBuilder"
		),
		contains = "Strategy")

setMethod("primeStrategy",
		signature(strategy = "LimitBreakoutStrategy"),
		function(strategy, market) {
			
			strategy <- callNextMethod()
			
			previous.close <- xts()
			for (instrument in strategy@instruments) {
				previous.close <- merge(previous.close, Cl(market[instrument]))
			}
			names(previous.close) <- strategy@instruments
			strategy@limits <- previous.close
			return(strategy)
		})

getLimit <- function(strategy, instrument, timestamp) {
	strategy@limits[timestamp, instrument]
}

CrossoverBreakout <- function(indicator, fast.pd, slow.pd, stop.point = numeric()) {
	force(indicator)
	force(fast.pd)
	force(slow.pd)
	
	strategy <- new("LimitBreakoutStrategy")
	
	indicator.fun <- function(mktdata) {
		fast <- indicator(Cl(mktdata), n = fast.pd)
		slow <- indicator(Cl(mktdata), n = slow.pd)
		return(fast > slow)
	}
	
	strategy@indicator <- indicator.fun
	strategy@target.builder <- BreakoutTargetBuilder(, stop.point)
	return(strategy)
}





















