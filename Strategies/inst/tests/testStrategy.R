#'
#'

context("__ Strategy __")

context("Calculating position sizes")

	test_that("Strategy returns named vector of sizes", {
				
				strategy <- new("Strategy")
				timestamp <- as.Date("2010-04-20")
				strategy@positions <- xts(t(c(AMP = 1, BHP = 1)), order.by = timestamp)
				
				expect_that(positionSizes(strategy, timestamp), 
						matchesObject(c(AMP = 1, BHP = 1)))
			})
	
	test_that("Normalised positions sizes", {
				
				strategy <- new("Strategy")
				positions <- c(AMP = 1, BHP = 1)
				
				expect_that(selectPositions(positions, strategy), 
						matchesObject(c(AMP = 0.5, BHP = 0.5)))
			})
	
	
context("Creating Targets")

	test_that("Targets returned in named list", {
				
				positions <- c(AMP = 0.5, BHP = 0.5)
				strategy <- new("Strategy")
				strategy@stop.point <- numeric()
				
				expected.targets <- list(
						AMP = Target("AMP", 0.5), 
						BHP = Target("BHP", 0.5))
				
				expect_that(createTargets(positions, strategy), 
						matchesObject(expected.targets))
			})
	
	test_that("Missing values do not create targets", {
				
				positions <- c(AMP = 0.5, BHP = NA)
				strategy <- new("Strategy")
				strategy@stop.point <- numeric()
				
				expected.targets <- list(AMP = Target("AMP", 0.5))
				
				expect_that(createTargets(positions, strategy), 
						matchesObject(expected.targets))
			})
	

context("Priming Strategies")

	test_that("Primed flag set after priming", {
				
				strategy <- new("Strategy")
				mktdata <- xts(1, order.by = as.Date("2010-01-01"))
				strategy@indicator <- function(mktdata) mktdata
				
				market <- Market(list(AMP = mktdata))
				
				strategy <- primeStrategy(strategy, market)
				
				expect_that(strategy@primed, matchesObject(TRUE))
			})
	
	test_that("Strategy not recalculated if already primed", {
				
				strategy <- new("Strategy")
				mktdata <- xts(1, order.by = as.Date("2010-01-01"))
				strategy@indicator <- function(mktdata) stop("Shouldn't be called")
				
				market <- Market(list(AMP = mktdata))
				
				primed(strategy) <- TRUE
				strategy <- tryCatch(primeStrategy(strategy, market), 
						error = function(e) strategy <<- NULL)
				
				expect_that(strategy, is_a("Strategy"))
			})
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

