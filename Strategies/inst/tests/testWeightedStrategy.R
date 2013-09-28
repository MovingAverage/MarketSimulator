#'
#'

context("__ Weighted Strategy __")

context("Creation of weighting")

	test_that("WeightedStrategy constructor", {
				
				expected.base <- Crossover(EMA, 3, 5)
				strategy <- Weighted(expected.base)
				expect_that(strategy@base, matchesObject(expected.base))
			})

	test_that("WeightedStrategy primes base Strategy", {
				
				market <- Market(loadStocks("AMP.AX"))
				strategy <- Weighted(Crossover(EMA, 3, 5))
				
				expected.base <- primeStrategy(Crossover(EMA, 3, 5), market)
				strategy <- primeStrategy(strategy, market)
				
				expect_that(strategy@base, matchesObject(expected.base))
				expect_that(strategy@instruments, matchesObject("AMP.AX"))
			})
	
	test_that("WeightedStrategy adjusts positions", {
				
				market <- Market(loadStocks("AMP.AX"))
				
				weighting_fun <- function(strategy, market) {
					positions <- strategy@positions
					returns <- positions * dailyReturns(market)
					returns[is.na(returns)] <- 0
					weights <- lag(rollmeanr(returns, k = 20, fill = NA))
					weights[is.na(weights)] <- 0
					weights <- weights > 0
					return(weights * 1)
				}
				
				strategy <- Weighted(Crossover(EMA, 3, 5), weighting.fun = weighting_fun)
				
				strategy <- primeStrategy(strategy, market)
				base <- primeStrategy(Crossover(EMA, 3, 5), market)
				expected.weights <- weighting_fun(base, market)
				expected.positions <- base@positions * expected.weights
				
				expect_that(strategy@weights, matchesObject(expected.weights))
				expect_that(strategy@positions, matchesObject(expected.positions))
			})

