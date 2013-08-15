#'
#'

context("__ Target Change __")

context("Basic information")

	test_that("Target position reports on instrument and size", {
				
				position <- Target("AMP", size = 0.5)
				
				expect_that(sizeOf(position), matchesObject(c(AMP = 0.5)))
				expect_that(instrumentOf(position), matchesObject("AMP"))
			})
	
	
	