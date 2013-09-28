#'
#'
setClass("EnsembleStrategy",
		representation(
			strategies = "list"
		),
		contains = "Strategy")
		
#' Constructor for EnsembleStrategy objects
#' 
#' Creates an ensemble strategy from the supplied strategy contructor and input 
#' parameters.
#' 
#' @export 
Ensemble <- function(constructor, indicator, fast, slow) {
	
	strategy <- new("EnsembleStrategy")
	for (i in seq_along(fast)) {
		strategy@strategies <- c(strategy@strategies, 
				constructor(indicator, fast.pd = fast[i], slow.pd = slow[i]))
	}
	return(strategy)
}

index.EnsembleStrategy <- function(x, ...) {
	index(x@strategies[[1]])
}

setMethod("primeStrategy",
		signature(strategy = "EnsembleStrategy"),
		function(strategy, market) {
			strategy <- prime_sub_strategies(strategy, market)
			positions(strategy) <- merge_positions(strategy)
			return(strategy)
		})

prime_sub_strategies <- function(strategy, market) {
	strategies <- strategy@strategies
	strategies <- lapply(strategies, primeStrategy, market = market)
	instruments <- lapply(strategies, slot, name = "instruments")
	instruments <- sort(unique(unlist(instruments)))
	strategy@instruments <- instruments
	strategy@strategies <- strategies
	return(strategy)
}

merge_positions <- function(strategy) {
	positions.list <- lapply(strategy@strategies, slot, "positions")
	all.positions <- xts()
	for (positions in positions.list) {
		all.positions <- merge(all.positions, positions)
	}
	
	positions <- xts(array(NA, 
					c(length(index(all.positions)), length(strategy@instruments))), 
					index(all.positions))
	names(positions) <- strategy@instruments
	
	for (instrument in names(positions)) {
		instrument.pos <- all.positions[, grepl(instrument, names(all.positions))]
		positions[, instrument] <- rowMeans(instrument.pos, na.rm = TRUE)
	}
	
	return(na.locf(positions))
}



























