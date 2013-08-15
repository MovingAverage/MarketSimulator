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
	
	
context("Determining viability of target")

	test_that("Not viable if transaction value too low", {
				
				manager <- Manager(Mock("Strategy"))
				
				position <- Mock("Positions")
				trade.value <- 1000
				mockMethod(position, "transactionValue", trade.value)
				mockMethod(position, "addTarget", position)
				target <- Target("AMP", numeric())
				
				expect_that(tradeCost(manager, trade.value), equals(6))
				expect_that(isViable(manager, position, target), is_false())
			})
	
	test_that("Transaction viable for bigger position", {
				
				manager <- Manager(Mock("Strategy"))
				
				position <- Mock("Positions")
				trade.value <- 6000
				mockMethod(position, "transactionValue", trade.value)
				mockMethod(position, "addTarget", position)
				target <- Target("AMP", numeric())
				
				expect_that(tradeCost(manager, trade.value), equals(6))
				expect_that(isViable(manager, position, target), is_true())
			})
	
	test_that("Transaction not viable for small negative size", {
				
				manager <- Manager(Mock("Strategy"))
				
				position <- Mock("Positions")
				trade.value <- -500
				mockMethod(position, "transactionValue", trade.value)
				mockMethod(position, "addTarget", position)
				target <- Target("AMP", numeric())
				
				expect_that(tradeCost(manager, trade.value), equals(6))
				expect_that(isViable(manager, position, target), is_false())
			})
	
	test_that("Transaction viable for large negative size", {
				
				manager <- Manager(Mock("Strategy"))
				
				position <- Mock("Positions")
				trade.value <- -10000
				mockMethod(position, "transactionValue", trade.value)
				mockMethod(position, "addTarget", position)
				target <- Target("AMP", numeric())
				
				expect_that(tradeCost(manager, trade.value), equals(10000 * 0.0008))
				expect_that(isViable(manager, position, target), is_true())
			})
	
	
context("Updating Positions")

	test_that("If no transactions only latest price updated", {
				
				account <- Account(5000)
				latest.prices <- c(AMP = 10, BHP = 5)
				positions <- PositionSet(account, latest.prices)
				
				manager <- Manager(Mock("Strategy"))
				manager@positions <- positions
				
				new.prices <- c(AMP = 10.5, BHP = 4.5)
				positions@latest.prices <- new.prices
				
				broker <- Mock("Broker")
				mockMethod(broker, "transactions", list())
				mockMethod(broker, "latestPrices", new.prices)
				
				manager <- updatePositions(manager, broker)
				
				expect_that(manager@positions, matchesObject(positions))
			})
	
	test_that("Open orders updated if no market action", {
				
				account <- Account(5000)
				latest.prices <- c(AMP = 10, BHP = 5)
				positions <- PositionSet(account, latest.prices)
				amp.position <- Position("AMP", size = 100)
				positions <- setPosition(positions, amp.position)
				
				manager <- Manager(Mock("Strategy"))
				manager@positions <- positions
				
				amp.sell.order <- Order("AMP", sell = 100)
				target.amp.position <- Position("AMP", list(amp.sell.order), size = 100)
				positions <- setPosition(positions, target.amp.position)
				new.prices <- c(AMP = 10.5, BHP = 4.5)
				positions@latest.prices <- new.prices
				
				broker <- Mock("Broker")
				mockMethod(broker, "transactions", list())
				mockMethod(broker, "latestPrices", new.prices)
				mockMethod(broker, "openOrders", list(amp.sell.order))
				
				manager <- updatePositions(manager, broker)
				
				expect_that(manager@positions, matchesObject(positions))
			})
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	



