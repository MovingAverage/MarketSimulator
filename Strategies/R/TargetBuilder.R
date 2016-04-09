#'
#'
setClass("TargetBuilder",
		representation(
			selector = "function", 
			stop.point = "numeric"
		))
		
TargetBuilder <- function(selector = normalisedPositions, stop.point = numeric()) {
	builder <- new("TargetBuilder")
	builder@selector <- selector
	builder@stop.point <- stop.point
	return(builder)
}

buildTargets <- function(builder, positions, strategy, ...) {
	positions <- builder@selector(positions)
	targets <- createTargets(builder, positions, strategy)
	return(targets)
}

setGeneric("createTargets",
		function(builder, positions, strategy, ...) {
			targets <- list()
			for (instrument in names(positions)) {
				size <- as.numeric(positions[instrument])
				if (!is.na(size)) {
					if (size > 0) {
						targets[instrument] <- Target(instrument, size, 
								stop.point = builder@stop.point)
					} else {
						targets[instrument] <- Target(instrument, size)
					}
				}
			}
			return(targets)
		})


#' Position Selectors
#' 
#' 
normalisedPositions <- function(positions) {
	if (isTRUE(sum(positions) > 0)) {
		positions <- positions / max(1, sum(positions, na.rm = TRUE))
	}
	return(positions)
}

#' Max Position Selector
#' 
#' Returns a Selector function which will choose a maximum number of positions as
#' requested. The selector considers existing positions and will keep them to avoid too 
#' many transactions where possible.
#' 
max_positions_selector <- function(num.positions, constant.size = TRUE) {
	
	stopifnot(is.numeric(num.positions))
	force(constant.size)
	
	existing.positions <- numeric()
	
	selector <- function(positions) {
		
		if (num.positions > sum(positions > 0, na.rm = TRUE)) {
			positions <- normalisedPositions(positions)
		} else {
			biggest <- names(rev(sort(positions))[seq(num.positions)])
			cut.off.size <- min(positions[biggest])
			
			if (sum(positions >= cut.off.size) > num.positions) {
				
				qualifying <- names(positions)[positions >= cut.off.size]
				existing.held <- names(existing.positions)[existing.positions > 0]
				existing.qualifiers <- qualifying %in% existing.held
				existing.qualifiers <- qualifying[existing.qualifiers]
				if (length(existing.qualifiers) == num.positions) {
					biggest <- existing.qualifiers
				} else {
					remaining.positions <- num.positions - length(existing.qualifiers)
					biggest <- names(rev(sort(positions))[seq(remaining.positions)])
					biggest <- c(existing.qualifiers, biggest)
				}
			}
			
			removed <- names(positions)[!(names(positions) %in% biggest)]
			
			positions[removed] <- 0
			positions <- normalisedPositions(positions)
		}
		if (constant.size) {
			positions[positions > 0] <- 1 / num.positions
		}
		existing.positions <<- positions
		return(positions)
	}
	return(selector)
}



setClass("BreakoutTargetBuilder",
		contains = "TargetBuilder")
		
BreakoutTargetBuilder <- function(selector = normalisedPositions, 
		stop.point = numeric()) {
	builder <- new("BreakoutTargetBuilder")
	builder@selector <- selector
	builder@stop.point <- stop.point
	return(builder)
}

setMethod("createTargets",
		signature(builder = "BreakoutTargetBuilder"),
		function(builder, positions, strategy) {
			targets <- list()
			for (instrument in names(positions)) {
				size <- as.numeric(positions[instrument])
				if (!is.na(size)) {
					if (size > 0) {
						timestamp <- attr(positions, "timestamp")
						limit <- getLimit(strategy, instrument, timestamp)
						targets[instrument] <- BreakoutTarget(instrument, size, 
								limit = limit)
					} else {
						targets[instrument] <- Target(instrument, size)
					}
				}
			}
			return(targets)
		})



























