#'
#'

context("__ Manager __")

context("Getting target positions")

	test_that("Manager contacts Strategy for positions", {
				
				strategy <- Mock()
				mockMethod(strategy, "targetPositions")
				manager <- Manager(strategy)
				targetPositions(manager, timestamp = "")
				
				expect_that(strategy, called_once("targetPositions"))
			})
	
	test_that("Manager only returns target if valid latestPrices", {
				
				strategy <- Mock()
				mockMethod(strategy, "targetPositions", list(amp = 1, bhp = 2, cba = 3))
				manager <- Manager(strategy)
				latestPrices(manager) <- c(amp = 10, bhp = 0, cba = NA)
				targets <- targetPositions(manager, timestamp = "")
				
				expect_that(names(targets), matchesObject("amp"))
			})
	
	
context("Accounting calculations")
	
	test_that("Positions calculate current equity", {
				
				latest.prices <- c(AMP = 20, BHP = 10)
				
				manager <- Manager(Mock("Strategy"))
				latestPrices(manager) <- latest.prices
				manager <- setupAccount(manager, 5000)
				manager <- setPosition(manager, Position("AMP", size = 100))
				manager <- setPosition(manager, Position("BHP", size = 200))
				
				expect_that(currentEquity(manager), matchesObject(9000))
			})

	
context("Determining viability of target")

	test_that("Not viable if transaction value too low", {
				
				manager <- Manager(Mock("Strategy"))
				
				position <- Mock("CurrentPosition")
				trade.value <- 1000
				mockMethod(position, "transactionValue", trade.value)
				position@instrument <- "AMP"
				
				expect_that(tradeCost(manager, trade.value), equals(6))
				expect_that(isViable(manager, position), is_false())
			})
	
	test_that("Transaction viable for bigger position", {
				
				manager <- Manager(Mock("Strategy"))
				
				position <- Mock("CurrentPosition")
				trade.value <- 6000
				mockMethod(position, "transactionValue", trade.value)
				position@instrument <- "AMP"

				expect_that(tradeCost(manager, trade.value), equals(6))
				expect_that(isViable(manager, position), is_true())
			})
	
	test_that("Transaction not viable for small negative size", {
				
				manager <- Manager(Mock("Strategy"))
				
				position <- Mock("CurrentPosition")
				trade.value <- -500
				mockMethod(position, "transactionValue", trade.value)
				position@instrument <- "AMP"
				
				expect_that(tradeCost(manager, trade.value), equals(6))
				expect_that(isViable(manager, position), is_false())
			})
	
	test_that("Transaction viable for large negative size", {
				
				manager <- Manager(Mock("Strategy"))
				
				position <- Mock("CurrentPosition")
				trade.value <- -10000
				mockMethod(position, "transactionValue", trade.value)
				position@instrument <- "AMP"
				
				expect_that(tradeCost(manager, trade.value), equals(10000 * 0.0008))
				expect_that(isViable(manager, position), is_true())
			})


context("Handling positions")

	test_that("Manager reports on active instruments", {
				
				instruments <- c("AMP", "BHP", "CBA")
				manager <- Manager(Mock("Strategy"))
				
				for (instrument in instruments) {
					manager <- setPosition(manager, Position(instrument))
				}
				
				expect_that(activeInstruments(manager), matchesObject(instruments))
			})
	
	test_that("Creates new position when adding Target if needed", {
				
				manager <- Manager(Mock("Strategy"))
				manager <- setupAccount(manager, 10000)
				latestPrices(manager) <- c(AMP = 1)
				target <- Target("AMP", 1)
				manager <- addTarget(manager, target)
				target <- setTargetQuantity(manager, target)
				
				expected.position <- Position("AMP")
				expected.position@target <- target
				
				expect_that(getPosition(manager, "AMP"), matchesObject(expected.position))
			})
	
	
context("Updating Positions")

	cleanMockMethods()

	test_that("If no transactions only latest price updated", {
				
				strategy <- Mock("Strategy")
				mockMethod(strategy, "targetPositions", list())
				manager <- Manager(strategy)
				
				new.prices <- c(AMP = 10.5, BHP = 4.5)
				
				broker <- Mock("Broker")
				mockMethod(broker, "getTransactions", list())
				mockMethod(broker, "latestPrices", new.prices)
				setTodaysDate(broker, "2007-01-01")
				
				manager <- updateRecords(manager, broker)
				
				expect_that(positions(manager), matchesObject(list()))
				expect_that(latestPrices(manager), matchesObject(new.prices))
			})
	
	test_that("Open orders updated if no market action", {
				
				account <- Account(5000)
				latest.prices <- c(AMP = 10, BHP = 5)
				amp.position <- Position("AMP", size = 100)
				
				strategy <- Mock("Strategy")
				mockMethod(strategy, "targetPositions", list())
				manager <- Manager(strategy)
				manager <- setPosition(manager, amp.position)
				
				amp.sell.order <- Order("AMP", sell = 100)
				target.amp.position <- Position("AMP", list(amp.sell.order), size = 100)
				new.prices <- c(AMP = 10.5, BHP = 4.5)
				
				broker <- Mock("Broker")
				mockMethod(broker, "getTransactions", list())
				mockMethod(broker, "latestPrices", new.prices)
				mockMethod(broker, "openOrders", list(amp.sell.order))
				
				manager <- updateRecords(manager, broker)
				
				expect_that(positions(manager)[[1]], matchesObject(target.amp.position))
			})

	test_that("Position size added with transaction", {
				
				order <- Order("AMP", buy = 100)
				order@execution.price <- xts(1, as.Date("2007-01-01"))
				order@txn.cost.model <- function(order) 6
				
				broker <- new("Broker")
				setTransactions(broker, list(order))
				setLatestPrices(broker, numeric())
				
				strategy <- Mock("Strategy")
				mockMethod(strategy, "targetPositions", list())
				manager <- Manager(strategy)
				manager <- setPosition(manager, Position("AMP"))
				
				manager <- updateRecords(manager, broker)
				
				position <- getPosition(manager, "AMP")
				
				expect_that(totalSize(position), matchesObject(100))
			})
	
	test_that("Position size subtracted with transaction", {
				
				order <- Order("AMP", sell = 100)
				order@execution.price <- xts(1, as.Date("2007-01-01"))
				order@txn.cost.model <- default_cost_model
				
				broker <- new("Broker")
				setTransactions(broker, list(order))
				setLatestPrices(broker, numeric())
				
				strategy <- Mock("Strategy")
				mockMethod(strategy, "targetPositions", list())
				manager <- Manager(strategy)
				manager <- setPosition(manager, Position("AMP", size = 100))
				
				manager <- updateRecords(manager, broker)
				
				position <- getPosition(manager, "AMP")
				
				expect_that(totalSize(position), matchesObject(0))
			})
	
	test_that("Position status set to stopped after stop order execution", {
				
				date <- as.Date("2010-04-20")
				order <- Stop("AMP", sell = 100, at = xts(10, order.by = date))
				order@execution.price <- xts(10, date)
				order@txn.cost.model <- default_cost_model
				
				broker <- new("Broker")
				setTransactions(broker, list(order))
				setLatestPrices(broker, numeric())
				
				strategy <- Mock("Strategy")
				mockMethod(strategy, "targetPositions", list())
				manager <- Manager(strategy)
				manager <- setPosition(manager, Position("AMP", size = 100))
				
				manager <- updateRecords(manager, broker)
				
				position <- getPosition(manager, "AMP")
				
				expect_that(status(position), matchesObject(Stopped()))
			})
	
	
context("Placing Orders")

	test_that("Orders not sent if position is up to date", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				
				position <- Position("AMP", size = 1000)
				position <- setTarget(position, Target("AMP", size = 1))
				status(position) <- Filled()
				
				manager <- Manager(Mock("Strategy"))
				latestPrices(manager) <- c(AMP = 5)
				manager <- setupAccount(manager, 0)
				manager <- setPosition(manager, position)
				
				manager <- placeOrders(manager, broker)
				
				expect_that(broker, not_called("addOrder"))
			})
	
	test_that("Orders sent for all positions", {
				
				manager <- Manager(Mock("Strategy"))
				latestPrices(manager) <- c(AMP = 5, BHP = 10)
				manager <- setupAccount(manager, 10000)
				manager <- setPosition(manager, Position("AMP"))
				manager <- setPosition(manager, Position("BHP"))
				manager <- addTarget(manager, Target("AMP", size = 0.5))
				manager <- addTarget(manager, Target("BHP", size = 0.4))
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				
				manager <- placeOrders(manager, broker)
				
				expect_that(broker, has_calls(
								addOrder(broker, Order("AMP", buy = 1000)), 
								addOrder(broker, Order("BHP", buy = 400))))
			})
	
	
context("Order notices")
	
	test_that("Multiple notices", {
				
				manager <- Manager(Mock("Strategy"))
				
				amp <- Position("AMP")
				amp@target <- Target("AMP", 10)
				amp <- addNotice(amp, -100)
				manager <- setPosition(manager, amp)
				
				bhp <- Position("BHP")
				bhp <- addNotice(bhp, 100)
				manager <- setPosition(manager, bhp)
				
				expected.notice <- "sell 100 AMP, buy 100 BHP"
				
				expect_that(notices(manager), matchesObject(expected.notice))
			})
	
	test_that("Two empty notices", {
				
				manager <- Manager(Mock("Strategy"))
				manager <- setPosition(manager, Position("AMP"))
				manager <- setPosition(manager, Position("BHP"))
				
				expect_that(nchar(notices(manager)), equals(0))
			})
	
	test_that("One notice out of two", {
				
				manager <- Manager(Mock("Strategy"))
				manager <- setPosition(manager, Position("AMP"))
				
				bhp <- Position("BHP")
				bhp <- addNotice(bhp, 100)
				manager <- setPosition(manager, bhp)
				
				expect_that(notices(manager), matchesObject("buy 100 BHP"))
			})
	
	test_that("Manager clears notices after sending", {
				
				manager <- Manager(Mock("Strategy"))
				
				bhp <- Position("BHP")
				bhp <- addNotice(bhp, 100)
				manager <- setPosition(manager, bhp)
				
				timestamp <- "2010-04-20"
				
				expect_that(sendNotices(manager, timestamp), 
						shows_message("2010-04-20: buy 100 BHP"))
				expect_that(sendNotices(manager, timestamp), shows_message(""))
			})
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	



