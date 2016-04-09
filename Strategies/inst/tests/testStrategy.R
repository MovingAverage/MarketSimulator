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

test_that("Strategy sets NA positions to 0", {
			
			strategy <- new("Strategy")
			timestamp <- as.Date("2010-04-20")
			strategy@positions <- xts(t(c(AMP = NA, BHP = 1)), order.by = timestamp)
			
			expect_that(positionSizes(strategy, timestamp), 
					matchesObject(c(AMP = 0, BHP = 1)))
		})

test_that("Strategy returns timestamp attribute on positions", {
			
			strategy <- new("Strategy")
			timestamp <- as.Date("2010-04-20")
			strategy@positions <- xts(t(c(AMP = NA, BHP = 1)), order.by = timestamp)
			
			expect_that(attr(positionSizes(strategy, timestamp), "timestamp"),  
					matchesObject(as.character(timestamp)))
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















