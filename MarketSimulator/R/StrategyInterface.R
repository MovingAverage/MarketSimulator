#'
#'
setClass("StrategyInterface",
		contains = "VIRTUAL")


#' Get the list of position targets
#' 
#' The strategy is required to return a named list of objects which conform to the 
#' TargetPositionInterface.
#' 
#' @export 
setGeneric("getTargets",
		function(strategy, timestamp) {
			standardGeneric("getTargets")
		})

#' Prime the strategy with market data
#' 
#' Market data is sent to strategy to calculate any preparations in readines for 
#' backtesting.
#' 
#' @export 
setGeneric("primeStrategy",
		function(strategy, market) {
			standardGeneric("primeStrategy")
		}, 
		valueClass = "StrategyInterface")
		

