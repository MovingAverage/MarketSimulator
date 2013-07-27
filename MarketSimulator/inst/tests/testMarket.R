#'
#'

context("__ Market __")

context("Market creation")

	test_that("Market set up with single instrument", {
				
				AMP <- loadStocks("AMP.AX")
				market <- Market(AMP)
				
				expect_that(length(market@instruments), equals(1))
				expect_that(market@instruments[[1]], matchesObject(AMP[[1]]))
			})
	
	test_that("Market lists instruments", {
				
				instruments <- c("AMP.AX", "BHP.AX")
				market <- Market(loadStocks(instruments))
				
				expect_that(tradeableInstruments(market), matchesObject(instruments))
			})

context("Instrument values")

	test_that("Market returns price bar for instrument", {
				
				AMP <- loadStocks("AMP.AX")
				market <- Market(AMP)
				timestamp <- index(AMP[[1]])[1]
				
				expect_that(getBar(market, "AMP.AX", timestamp), 
						matchesObject(AMP[[1]][1]))
			})

	