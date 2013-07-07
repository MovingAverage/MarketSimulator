#'
#'

Manager <- function(strategy) {
	return(NULL)
}

getPositions <- function(manager, strategy) {
	positions(strategy)
}


balancePositions <- function(current.positions, ideal.positions) {
	
	percentage.commission <- 0.0008
	minimum.commission <- 6
	cost.threshold <- 0.002
	capital <- 10000
	
	size.of.change <- abs(ideal.positions - current.positions) * capital
	cost <- pmax(minimum.commission, size.of.change * percentage.commission)
	relative.cost <- cost / size.of.change
	not.effective <- relative.cost > cost.threshold
	ideal.positions[not.effective] <- current.positions[not.effective]
	return(ideal.positions)
}

