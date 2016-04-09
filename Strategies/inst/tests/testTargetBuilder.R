#'
#'

context("__ Target Builder __")

context("Creating Targets")

	test_that("Targets returned in named list", {
				
				positions <- c(AMP = 0.5, BHP = 0.5)
				builder <- TargetBuilder()
				
				expected.targets <- list(
						AMP = Target("AMP", 0.5), 
						BHP = Target("BHP", 0.5))
				
				expect_that(createTargets(builder, positions), 
						matchesObject(expected.targets))
			})
	
	test_that("Missing values do not create targets", {
				
				positions <- c(AMP = 0.5, BHP = NA)
				builder <- TargetBuilder()
				
				expected.targets <- list(AMP = Target("AMP", 0.5))
				
				expect_that(createTargets(builder, positions), 
						matchesObject(expected.targets))
			})


context("Normalised Position Selector")

	test_that("Normalised positions sizes", {
				
				positions <- c(AMP = 1, BHP = 1)
				
				expect_that(normalisedPositions(positions), 
						matchesObject(c(AMP = 0.5, BHP = 0.5)))
			})
	
	test_that("Normalised positions sizes", {
				
				positions <- c(AMP = 2, BHP = 2)
				
				expect_that(normalisedPositions(positions), 
						matchesObject(c(AMP = 0.5, BHP = 0.5)))
			})
	

context("Max Number of Positions Selector")
	
	test_that("Selector returns max number of positions", {
				
				positions4 <- c(AMP = 1, BHP = 1, CBA = 1, WDC = 1)
				positions1 <- c(AMP = 1, BHP = 0, CBA = 0, WDC = 0)
				
				
				selector.2 <- max_positions_selector(2)
				selector.3 <- max_positions_selector(3)
				
				expect_that(length(selector.2(positions4)), 
						matchesObject(length(positions4)))
				expect_that(names(selector.2(positions4)), 
						matchesObject(names(positions4)))
				expect_that(sum(selector.2(positions4) > 0), matchesObject(2))
				expect_that(sum(selector.3(positions4) > 0), matchesObject(3))
				expect_that(sum(selector.2(positions1) > 0), matchesObject(1))
				expect_that(sum(selector.3(positions1) > 0), matchesObject(1))
			})
	
	test_that("Selector returns positions with highest value", {
				
				positions <- c(AMP = 2, BHP = 1, CBA = 2, WDC = 1)
				
				selector <- max_positions_selector(2)
				
				expected.positions <- c(AMP = 0.5, BHP = 0, CBA = 0.5, WDC = 0)
				
				expect_that(selector(positions), matchesObject(expected.positions))
			})
	
	test_that("Selector favours existing positions", {
				
				positions1 <- c(AMP = 0, BHP = 0, CBA = 1, WDC = 0)
				positions2 <- c(AMP = 3, BHP = 1, CBA = 1, WDC = 1)
				
				selector <- max_positions_selector(2)
				
				expected.positions <- c(AMP = 0.75, BHP = 0, CBA = 0.25, WDC = 0)
				
				selector(positions1)
				existing <- environment(selector)$existing.positions
				
				expect_that(existing, matchesObject(positions1))
				expect_that(selector(positions2), matchesObject(expected.positions))
			})
	
	test_that("Selector chooses all existing", {
				
				positions1 <- c(AMP = 1, BHP = 0, CBA = 1, WDC = 0)
				positions2 <- c(AMP = 1, BHP = 1, CBA = 1, WDC = 1)
				
				selector <- max_positions_selector(2)
				
				expected.positions <- c(AMP = 0.5, BHP = 0, CBA = 0.5, WDC = 0)
				
				selector(positions1)
				expect_that(selector(positions2), matchesObject(expected.positions))
			})
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
