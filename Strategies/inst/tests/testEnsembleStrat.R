#'
#'

context("__ Ensemble Strategy __")

context("Ensemble creation")

test_that("Ensemble created with multiple strategies", {
			
			strategy <- Ensemble(Crossover, EMA, c(3, 5), c(5, 10))
			
			expected.strats <- list(
					Crossover(EMA, 3, 5), 
					Crossover(EMA, 5, 10))
			
			expect_that(length(strategy@strategies), matchesObject(2))
			expect_that(strategy@strategies, matchesObject(expected.strats))
		})

test_that("Ensemble primed with two Crossovers", {
			
			strategy <- Ensemble(Crossover, EMA, c(3, 5), c(5, 10))
			
			market <- Market(loadStocks("AMP.AX"))
			
			strategy <- primeStrategy(strategy, market)
			
			expected.strats <- list(
					primeStrategy(Crossover(EMA, 3, 5), market), 
					primeStrategy(Crossover(EMA, 5, 10), market))
			
			expect_that(strategy@strategies, matchesObject(expected.strats))
			expect_that(strategy@instruments, matchesObject("AMP.AX"))
		})

test_that("Positions merged from sub strategies", {
			
			pos1 <- xts(t(c(AMP = 0.5, BHP = 1)), as.Date("2010-04-20"))
			pos2 <- xts(t(c(AMP = 1, BHP = 0)), as.Date("2010-04-20"))
			
			expected.pos <- xts(t(c(AMP = 0.75, BHP = 0.5)), as.Date("2010-04-20"))
			
			strat1 <- strat2 <- new("Strategy")
			strat1@positions <- pos1
			strat2@positions <- pos2
			strat1@instruments <- strat2@instruments <- c("AMP", "BHP")
			
			strategy <- new("EnsembleStrategy")
			strategy@strategies <- list(strat1, strat2)
			strategy@instruments <- c("AMP", "BHP")
			
			expect_that(merge_positions(strategy), matchesObject(expected.pos))
		})

test_that("Positions merged if not uniform", {
			
			dates1 <- seq.Date(from = as.Date("2010-04-20"), by = 1, length.out = 3)
			pos1 <- xts(rbind(
							c(AMP = 0, BHP = 1), 
							c(AMP = 0, BHP = 1), 
							c(AMP = 0, BHP = 1)), dates1)
			
			dates2 <- c(as.Date("2010-04-20"), as.Date("2010-04-22"))
			pos2 <- xts(rbind(
							c(BHP = 1, CBA = 1), 
							c(BHP = 0, CBA = 0)), dates2)
			
			strat1 <- strat2 <- new("Strategy")
			strat1@positions <- pos1
			strat2@positions <- pos2
			
			expected.pos <- xts(rbind(
							c(AMP = 0, BHP = 1, CBA = 1), 
							c(AMP = 0, BHP = 1, CBA = 1), 
							c(AMP = 0, BHP = 0.5, CBA = 0)), as.POSIXct(dates1))
			
			strategy <- new("EnsembleStrategy")
			strategy@strategies <- list(strat1, strat2)
			strategy@instruments <- c("AMP", "BHP", "CBA")
			
			expect_that(merge_positions(strategy), 
					matchesObject(expected.pos, ignore.attributes = FALSE))
		})


context("Ensemble targets")

test_that("Ensemble returns the average target size", {
			
			ensemble <- Ensemble(Crossover, EMA, c(3, 5), c(5, 10))
			market <- Market(loadStocks("AMP.AX"))
			ensemble <- primeStrategy(ensemble, market)
			ensemble@target.builder <- TargetBuilder()
			
			strat1 <- primeStrategy(Crossover(EMA, 3, 5), market) 
			strat2 <- primeStrategy(Crossover(EMA, 5, 10), market)
			
			dates <- as.character(index(strat1@positions))
			
			expected_target <- function(date) {
				target1 <- getTargets(strat1, date)[[1]]
				target2 <- getTargets(strat2, date)[[1]]
				expected.size <- mean(c(sizeOf(target1), sizeOf(target2)))
				return(Target("AMP.AX", size = expected.size))
			}
			
			expect_that(getTargets(ensemble, dates[10])[[1]], 
					matchesObject(expected_target(dates[10])))
			expect_that(getTargets(ensemble, dates[50])[[1]], 
					matchesObject(expected_target(dates[50])))
			expect_that(getTargets(ensemble, dates[100])[[1]], 
					matchesObject(expected_target(dates[100])))
		})

test_that("Ensemble handles missing positions", {
			
			timestamp <- as.Date("2010-04-20")
			strat2 <- strat1 <- new("Strategy")
			strat1@positions <- xts(t(c(AMP = 0, BHP = NA)), order.by = timestamp)
			strat2@positions <- xts(t(c(AMP = 0, BHP = 1)), order.by = timestamp)
			strat2@instruments <- strat1@instruments <- c("AMP", "BHP")
			
			ensemble <- new("EnsembleStrategy")
			ensemble@strategies <- list(strat1, strat2)
			ensemble@instruments <- c("AMP", "BHP")
			ensemble@positions <- merge_positions(ensemble)
			
			expect_that(positionSizes(ensemble, timestamp), 
					matchesObject(c(AMP = 0, BHP = 1)))
		})

test_that("Ensemble handles mismatched positions", {
			
			timestamp <- as.Date("2010-04-20")
			strat3 <- strat2 <- strat1 <- new("Strategy")
			strat1@positions <- xts(t(c(AMP = 0, CBA = 0.5)), timestamp)
			strat2@positions <- xts(t(c(AMP = 0, BHP = 1)), timestamp)
			strat3@positions <- xts(t(c(AMP = 1, BHP = 0, CBA = 1)), timestamp)
			strat1@instruments <- c("AMP", "CBA")
			strat2@instruments <- c("AMP", "BHP")
			strat3@instruments <- c("AMP", "BHP", "CBA")
			
			ensemble <- new("EnsembleStrategy")
			ensemble@strategies <- list(strat1, strat2, strat3)
			ensemble@instruments <- c("AMP", "BHP", "CBA")
			ensemble@positions <- merge_positions(ensemble)
			
			expected.positions <- c(AMP = 1/3, BHP = 0.5, CBA = 0.75)
			attr(expected.positions, "timestamp") <- as.character(timestamp)
			
			expect_that(positionSizes(ensemble, timestamp), 
					matchesObject(expected.positions, ignore.attributes = FALSE))
		})





































