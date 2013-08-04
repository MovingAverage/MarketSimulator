#'
#'

context("__ Manager __")

context("Interfacing with Strategy")

	test_that("Manager contacts Strategy for positions", {
				
				strategy <- Mock()
				mockMethod(strategy, "targetPositions")
				manager <- Manager(strategy)
				targetPositions(manager, timestamp = "")
				
				expect_that(strategy, called_once("targetPositions"))
			})
	
context("Balancing current with target fractions")

	test_that("Positions are taken if none currently", {
				
				current.positions <- c(AMP = 0, BHP = 0, CBA = 0)
				ideal.positions <- c(AMP = 0.35, BHP = 0, CBA = 0.6)
				expected.changes <- c(AMP = 0.35, BHP = 0, CBA = 0.6)
				
				target.changes <- balancePositions(current.positions, ideal.positions)
				
				expect_that(target.changes, matchesObject(expected.changes))
			})
	
	test_that("No changes if already have correct positions", {
				
				current.positions <- c(AMP = 0.5, CBA = 0)
				ideal.positions <- c(AMP = 0.5, CBA = 0.6)
				expected.changes <- c(AMP = 0, CBA = 0.6)
				
				target.changes <- balancePositions(current.positions, ideal.positions)
				
				expect_that(target.changes, matchesObject(expected.changes))
			})
	
	test_that("Positions are ignored if too costly", {
				
				current.positions <- c(AMP = 0.25, BHP = 0, CBA = 0)
				ideal.positions <- c(AMP = 0.3, BHP = 0, CBA = 0.6)
				expected.changes <- c(AMP = 0, BHP = 0, CBA = 0.6)
				
				target.changes <- balancePositions(current.positions, ideal.positions)
				
				expect_that(target.changes, matchesObject(expected.changes))
			})
	
	test_that("Positions are balanced if not equal length", {
				
				current.positions <- numeric(0)
				ideal.positions <- c(AMP = 0.35, BHP = 0, CBA = 0.6)
				expected.changes <- c(AMP = 0.35, BHP = 0, CBA = 0.6)
				
				target.changes <- balancePositions(current.positions, ideal.positions)
				
				expect_that(target.changes, matchesObject(expected.changes))
			})
	
	test_that("Positions not changed if missing current fraction", {
				
				current.positions <- c(AMP = 0.5, BHP = NA)
				ideal.positions <- c(AMP = 0.5, BHP = 0.5)
				expected.changes <- c(AMP = 0, BHP = 0)
				
				target.changes <- balancePositions(current.positions, ideal.positions)
				
				expect_that(target.changes, matchesObject(expected.changes))
			})
	
	test_that("Positions not changed if missing target", {
				
				current.positions <- c(AMP = 0.5, BHP = 0)
				ideal.positions <- c(AMP = 0.5, BHP = NA)
				expected.changes <- c(AMP = 0, BHP = 0)
				
				target.changes <- balancePositions(current.positions, ideal.positions)
				
				expect_that(target.changes, matchesObject(expected.changes))
			})

context("Position size calculations")

	test_that("Zero position size changes are not returned", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "currentPositionFractions", c(AMP = 0, BHP = 0.5))
				mockMethod(broker, "currentEquity", 10000)
				mockMethod(broker, "currentPositions", c(AMP = 0, BHP = 100))
				mockMethod(broker, "latestPrices", c(AMP = 50, BHP = 50))
				
				target.fraction <- c(AMP = 0.5, BHP = 0.5)
				
				sizes <- positionSizes(broker, target.fraction)
				
				expect_that(sizes, matchesObject(c(AMP = 100)))
			})
	
	test_that("Sell all target uses current position size", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "currentPositionFractions", c(AMP = 0.5, BHP = 0.5))
				mockMethod(broker, "currentEquity", 10000)
				mockMethod(broker, "currentPositions", c(AMP = 100, BHP = 100))
				mockMethod(broker, "latestPrices", c(AMP = 30, BHP = 50))
				
				target.fraction <- c(AMP = 0, BHP = 0.5)
				
				sizes <- positionSizes(broker, target.fraction)
				
				expect_that(sizes, matchesObject(c(AMP = -100)))
			})
	
context("Placing Orders")

	test_that("Manager creates buy order", {
				
				cleanMockMethods()
				broker <- Mock("Broker")
				equity <- 10000
				AMP.price <- c(AMP = 50)
				mockMethod(broker, "currentEquity", equity)
				mockMethod(broker, "latestPrices", AMP.price)
				mockMethod(broker, "currentPositions", c(AMP = 0))
				mockMethod(broker, "addOrder")
				
				changed.positions <- c(AMP = 0.5)
				strategy <- Mock()
				mockMethod(strategy, "targetPositions", changed.positions)
				order.size <- equity * changed.positions / as.numeric(AMP.price)
				expected.order <- Order("AMP", buy = order.size)
				
				manager <- Manager(strategy)
				placeOrders(manager, broker, timestamp = "")
				
				expect_that(broker, called_once_with("addOrder", expected.order))
			})
	
	test_that("Manager creates two buy orders", {
				
				broker <- Mock("Broker")
				equity <- 10000
				prices <- c(AMP = 50, BHP = 10)
				mockMethod(broker, "currentEquity", equity)
				mockMethod(broker, "latestPrices", prices)
				mockMethod(broker, "currentPositions", c(AMP = 0))
				mockMethod(broker, "addOrder")
				
				changed.positions <- c(AMP = 0.5, BHP = 0.3)
				strategy <- Mock()
				mockMethod(strategy, "targetPositions", changed.positions)
				amp.size <- equity * changed.positions["AMP"] / prices["AMP"]
				amp.order <- Order("AMP", buy = amp.size)
				bhp.size <- equity * changed.positions["BHP"] / prices["BHP"]
				bhp.order <- Order("BHP", buy = bhp.size)
				
				manager <- Manager(strategy)
				placeOrders(manager, broker, timestamp = "")
				
				expect_that(broker, has_calls(
								addOrder(broker, amp.order), 
								addOrder(broker, bhp.order)))
			})
	
	test_that("Manager creates buy and sell order", {
				
				broker <- Mock("Broker")
				equity <- 10000
				prices <- c(AMP = 50, BHP = 10)
				mockMethod(broker, "currentEquity", equity)
				mockMethod(broker, "latestPrices", prices)
				mockMethod(broker, "currentPositions", c(AMP = 0))
				mockMethod(broker, "addOrder")
				
				changed.positions <- c(AMP = 0.5, BHP = -0.3)
				strategy <- Mock()
				mockMethod(strategy, "targetPositions", changed.positions)
				amp.size <- equity * changed.positions["AMP"] / prices["AMP"]
				amp.order <- Order("AMP", buy = amp.size)
				bhp.size <- equity * -changed.positions["BHP"] / prices["BHP"]
				bhp.order <- Order("BHP", sell = bhp.size)
				
				manager <- Manager(strategy)
				placeOrders(manager, broker, timestamp = "")
				
				expect_that(broker, has_calls(
								addOrder(broker, amp.order), 
								addOrder(broker, bhp.order)))
			})
	
	test_that("Order not added if has correct position", {
				
				broker <- Mock("Broker")
				equity <- 10000
				prices <- c(AMP = 50)
				target.position <- c(AMP = 0.5)
				current.position <- target.position * equity / prices
				mockMethod(broker, "currentEquity", equity)
				mockMethod(broker, "latestPrices", prices)
				mockMethod(broker, "currentPositions", current.position)
				mockMethod(broker, "addOrder")
				
				strategy <- Mock()
				mockMethod(strategy, "targetPositions", target.position)
				
				manager <- Manager(strategy)
				placeOrders(manager, broker, timestamp = "")
				
				expect_that(broker, not_called("addOrder"))
			})
	
	test_that("Sell order placed at current position if sell all", {
				
				broker <- Mock("Broker")
				AMP.price <- c(AMP = 45)
				mockMethod(broker, "currentEquity", 10000)
				mockMethod(broker, "latestPrices", AMP.price)
				mockMethod(broker, "currentPositions", c(AMP = 100))
				mockMethod(broker, "addOrder")
				
				target.positions <- c(AMP = 0)
				strategy <- Mock()
				mockMethod(strategy, "targetPositions", target.positions)
				expected.order <- Order("AMP", sell = 100)
				
				manager <- Manager(strategy)
				records <- placeOrders(manager, broker, timestamp = "")
				
				expect_that(broker, called_once_with("addOrder", expected.order))
				expect_that(records, matchesObject("sell all (-100) AMP"))
			})
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	



