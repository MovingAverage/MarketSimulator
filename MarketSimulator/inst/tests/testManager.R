#'
#'

context("__ Manager __")

context("Interfacing with Strategy")

	test_that("Manager contacts Strategy for positions", {
				
				strategy <- Mock()
				mockMethod(strategy, "positions")
				manager <- Manager(strategy)
				getPositions(manager, strategy)
				
				expect_that(strategy, called_once("positions"))
			})
	
context("Position calculations")

	test_that("Positions are taken if none currently", {
				
				manager <- Manager(Mock())
				current.positions <- c(AMP = 0, BHP = 0, CBA = 0)
				ideal.positions <- c(AMP = 0.3, BHP = 0, CBA = 0.6)
				
				target.positions <- balancePositions(current.positions, ideal.positions)
				
				expect_that(target.positions, matchesObject(ideal.positions))
			})
	
	test_that("Positions are ignored if too costly", {
				
				manager <- Manager(Mock())
				current.positions <- c(AMP = 0.25, BHP = 0, CBA = 0)
				ideal.positions <- c(AMP = 0.3, BHP = 0, CBA = 0.6)
				expected.positions <- c(AMP = 0.25, BHP = 0, CBA = 0.6)
				
				target.positions <- balancePositions(current.positions, ideal.positions)
				
				expect_that(target.positions, matchesObject(expected.positions))
			})



