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
	
context("Creating Orders")

	test_that("Manager creates buy order", {
				
				manager <- Manager(Mock())
				broker <- Mock("Broker")
				equity <- 10000
				AMP.price <- c(AMP = 50)
				mockMethod(broker, "currentEquity", equity)
				mockMethod(broker, "latestPrices", AMP.price)
				mockMethod(broker, "addOrder")
				
				changed.positions <- c(AMP = 0.5)
				order.size <- equity * changed.positions / AMP.price
				expected.order <- Order("AMP", buy = order.size)
				
				createOrders(manager, broker, changed.positions)
				
				expect_that(broker, called_once_with("addOrder", expected.order))
			})
	
	test_that("Manager creates two buy orders", {
				
				manager <- Manager(Mock())
				broker <- Mock("Broker")
				equity <- 10000
				prices <- c(AMP = 50, BHP = 10)
				mockMethod(broker, "currentEquity", equity)
				mockMethod(broker, "latestPrices", prices)
				mockMethod(broker, "addOrder")
				
				changed.positions <- c(AMP = 0.5, BHP = 0.3)
				amp.size <- equity * changed.positions["AMP"] / prices["AMP"]
				amp.order <- Order("AMP", buy = amp.size)
				bhp.size <- equity * changed.positions["BHP"] / prices["BHP"]
				bhp.order <- Order("BHP", buy = bhp.size)
				
				createOrders(manager, broker, changed.positions)
				
				expect_that(broker, calls_made(
								addOrder(broker, amp.order), 
								addOrder(broker, bhp.order)))
			})
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	



