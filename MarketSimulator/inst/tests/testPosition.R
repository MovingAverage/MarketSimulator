#'
#'

context("__ Positions __")

context("Creating Position object")

	test_that("CurrentPosition created from Account and Order", {
				
				order <- Order("AMP", buy = 100)
				account <- Account(1)
				position <- Position("AMP", list(order), size = 0)
				
				expect_that(position, is_a("CurrentPosition"))
				expect_that(instrumentOf(position), equals("AMP"))
				expect_that(market_order(position), matchesObject(order))
			})
	
	test_that("CurrentPosition throws error if wrong orders given", {
				
				bhp.orders <- list(Order("BHP", buy = 100))
				
				expect_that(Position("AMP", list(1)), throws_error("class Order"))
				expect_that(Position("AMP", bhp.orders), throws_error("instrument"))
			})

context("Current positions")

	test_that("Empty position produces zero vector", {
				
				position <- Position("AMP")
				
				expect_that(totalSize(position), 
						matchesObject(c(AMP = 0), ignore.attributes = FALSE))
			})
	
	test_that("Position with open market order returns size", {
				
				position <- Position("AMP", list(Order("AMP", buy = 100)))
				
				expect_that(totalSize(position), 
						matchesObject(c(AMP = 100), ignore.attributes = FALSE))
			})
	
	test_that("Position with current holding returns holding", {
				
				account <- Account(1000)
				position <- Position("AMP", list(), size = 100)
				
				expect_that(totalSize(position), 
						matchesObject(c(AMP = 100), ignore.attributes = FALSE))
			})
	
	test_that("Position with holding and open order adds them", {
				
				account <- Account(1000)
				position <- Position("AMP", list(Order("AMP", buy = 100)), size = 100)
				
				expect_that(totalSize(position), 
						matchesObject(c(AMP = 200), ignore.attributes = FALSE))
			})
	
	test_that("Position with open order and stop ignores stop", {
				
				position <- Position("AMP", list(
								Order("AMP", buy = 100), 
								Stop("AMP", sell = 100, at = xts())))
				position2 <- Position("AMP", list(
								Stop("AMP", sell = 100, at = xts()), 
								Order("AMP", buy = 100)))
				
				expect_that(totalSize(position), 
						matchesObject(c(AMP = 100), ignore.attributes = FALSE))
				expect_that(totalSize(position2), 
						matchesObject(c(AMP = 100), ignore.attributes = FALSE))
			})
	
	
context("List of positions")
	
	test_that("Positions reports on position names", {
				
				instruments <- c("AMP", "BHP", "CBA")
				account <- Mock("Account")
				mockMethod(account, "holdings", data.frame(
								size = numeric(3), row.names = instruments))
				position <- PositionSet(account, numeric())
				
				for (instrument in instruments) {
					position <- addPosition(position, instrument)
				}
				
				expect_that(activeInstruments(position), matchesObject(instruments))
			})
	
	test_that("Adding positions", {
				
				instruments <- c("AMP", "BHP", "CBA")
				account <- Account(1000)
				sizes <- c(10, 20, 30)
				names(sizes) <- instruments
				position <- PositionSet(account, numeric())
				
				for (instrument in instruments) {
					position <- addPosition(position, instrument, 
							size = sizes[instrument])
				}
				
				expect_that(activeInstruments(position), matchesObject(instruments))
				expect_that(totalSize(position@positions[[1]]), matchesObject(10))
			})
	
	test_that("Creates new position when adding Target if needed", {
				
				positions <- PositionSet(Account(0), numeric())
				target <- Target("AMP", 100)
				positions <- addTarget(positions, target)
				
				expected.position <- Position("AMP")
				expected.position@target <- target
				
				expect_that(getPosition(positions, "AMP"), 
						matchesObject(expected.position))
			})

	
context("Accounting calculations")

	test_that("Positions calculate current equity", {
				
				account <- Account(5000)
				latest.prices <- c(AMP = 20, BHP = 10)
				positions <- PositionSet(account, latest.prices)
				amp <- Position("AMP")
				amp@size <- 100
				bhp <- Position("BHP")
				bhp@size <- 200
				
				positions <- setPosition(positions, amp)
				positions <- setPosition(positions, bhp)
				
				expect_that(currentEquity(positions), matchesObject(9000))
			})
	
	
context("Expected transaction value and size")
	
	test_that("Transaction size always given as integer", {
				
				account <- Account(10000)
				latest.prices <- c(AMP = 5)
				positions <- PositionSet(account, latest.prices)
				positions <- addPosition(positions, "AMP")
				positions <- addTarget(positions, Target("AMP", size = 1/7))
				size <- as.integer((1/7) * 10000 / latest.prices)
				
				expect_that(transactionSize(positions, "AMP"), 
						matchesObject(c(AMP = size), ignore.attributes = FALSE))
			})
	
	test_that("When no existing position", {
				
				account <- Account(10000)
				latest.prices <- c(AMP = 5)
				position <- PositionSet(account, latest.prices)
				position <- addPosition(position, "AMP")
				position <- addTarget(position, Target("AMP", size = 0.5))
				
				expect_that(transactionValue(position, "AMP"), matchesObject(5000))
				expect_that(transactionSize(position, "AMP"), matchesObject(1000))
			})
	
	test_that("When has existing position", {
				
				account <- Account(8000)
				latest.prices <- c(AMP = 5)
				position <- PositionSet(account, latest.prices)
				position <- addPosition(position, "AMP", size = 400)
				position <- addTarget(position, Target("AMP", size = 0.5))
				
				expect_that(transactionValue(position, "AMP"), matchesObject(3000))
				expect_that(transactionSize(position, "AMP"), matchesObject(600))
			})
	
	test_that("When has no position, but open order", {
				
				account <- Account(10000)
				latest.prices <- c(AMP = 5)
				position <- PositionSet(account, latest.prices)
				position <- addPosition(position, "AMP", list(Order("AMP", buy = 400)))
				position <- addTarget(position, Target("AMP", size = 0.5))
				
				expect_that(transactionValue(position, "AMP"), matchesObject(5000))
				expect_that(transactionSize(position, "AMP"), matchesObject(1000))
			})
	
	test_that("When existing position and open orders", {
				
				account <- Account(9000)
				latest.prices <- c(AMP = 5)
				position <- PositionSet(account, latest.prices)
				position <- addPosition(position, "AMP", 
						list(Order("AMP", buy = 200)), size = 200)
				position <- addTarget(position, Target("AMP", size = 0.5))
				
				expect_that(transactionValue(position, "AMP"), matchesObject(4000))
				expect_that(transactionSize(position, "AMP"), matchesObject(800))
			})
	
	
context("Sending orders to Broker")	
	
	test_that("Position sends Market buy order", {
				
				account <- Account(10000)
				latest.prices <- c(AMP = 10)
				position <- PositionSet(account, latest.prices)
				position <- addPosition(position, "AMP")
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				
				target <- Target("AMP", size = 0.5)
				
				position <- addTarget(position, target)
				position <- fireOrder(position, broker, "AMP")
				
				expect_that(broker, 
						called_once_with("addOrder", Order("AMP", buy = 500)))
			})
	
	test_that("Position sends Market sell order", {
				
				account <- Account(5000)
				latest.prices <- c(AMP = 10)
				position <- PositionSet(account, latest.prices)
				position <- addPosition(position, "AMP", size = 500)
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				
				target <- Target("AMP", size = 0)
				
				position <- addTarget(position, target)
				position <- fireOrder(position, broker, "AMP")
				
				expect_that(broker, 
						called_once_with("addOrder", Order("AMP", sell = 500)))
			})
	
	test_that("Orders sent for all positions", {
				
				account <- Account(10000)
				latest.prices <- c(AMP = 5, BHP = 10)
				positions <- PositionSet(account, latest.prices)
				positions <- addPosition(positions, "AMP")
				positions <- addPosition(positions, "BHP")
				
				strategy <- Mock("Strategy")
				mockMethod(strategy, "targetPositions", 
						list(Target("AMP", size = 0.5), Target("BHP", size = 0.4)))
				
				manager <- Manager(strategy)
				manager@positions <- positions
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				
				manager <- placeOrders(manager, broker, timestamp = "")
				
				expect_that(broker, has_calls(
								addOrder(broker, Order("AMP", buy = 1000)), 
								addOrder(broker, Order("BHP", buy = 400))))
			})
	
	test_that("Order not sent if existing open order", {
				
				account <- Account(10000)
				latest.prices <- c(AMP = 10)
				position <- PositionSet(account, latest.prices)
				position <- addPosition(position, "AMP", list(Order("AMP", buy = 500)))
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				mockMethod(broker, "replaceOrder")
				
				target <- Target("AMP", size = 0.5)
				
				position <- addTarget(position, target)
				position <- fireOrder(position, broker, "AMP")
				
				expect_that(broker, not_called("addOrder"))
				expect_that(broker, not_called("replaceOrder"))
			})
	
	test_that("Order replaced if existing but not correct size", {
				
				account <- Account(10000)
				latest.prices <- c(AMP = 10)
				position <- PositionSet(account, latest.prices)
				order <- Order("AMP", buy = 300)
				order <- setID(order, 1L) 
				position <- addPosition(position, "AMP", list(order))
				
				broker <- Mock("Broker")
				mockMethod(broker, "replaceOrder")
				
				target <- Target("AMP", size = 0.5)
				
				position <- addTarget(position, target)
				position <- fireOrder(position, broker, "AMP")
				
				expected.order <- order
				expected.order@quantity <- 500L
				
				expect_that(broker, has_calls(replaceOrder(broker, expected.order)))
			})
	
context("Position order notices")

	test_that("Buy notice", {
				
				position <- Position("AMP")
				position <- addNotice(position, 100)
				
				test_that(notice(position), matchesObject("buy 100 AMP"))
			})
	
	test_that("Sell notice", {
				
				position <- Position("AMP")
				position@target <- Target("AMP", 10)
				position <- addNotice(position, -100)
				
				test_that(notice(position), matchesObject("sell 100 AMP"))
			})
	
	test_that("Sell all notice", {
				
				position <- Position("AMP")
				position@target <- Target("AMP", 0)
				position <- addNotice(position, -100)
				
				test_that(notice(position), matchesObject("sell all (100) AMP"))
			})
	
	test_that("Empty notice returns empty character vector", {
				
				position <- Position("AMP")
				
				expect_that(notice(position), matchesObject(character(0)))
			})
	
	test_that("Multiple notices", {
				
				account <- Account(10000)
				latest.prices <- c(AMP = 1)
				positions <- PositionSet(account, latest.prices)
				
				amp <- Position("AMP")
				amp@target <- Target("AMP", 10)
				amp <- addNotice(amp, -100)
				positions <- setPosition(positions, amp)
				
				bhp <- Position("BHP")
				bhp <- addNotice(bhp, 100)
				positions <- setPosition(positions, bhp)
				
				expected.notice <- "sell 100 AMP, buy 100 BHP"
				
				test_that(notices(positions), equals(expected.notice))
			})
	
	test_that("Two empty notices", {
				
				account <- Account(10000)
				latest.prices <- c(AMP = 1)
				positions <- PositionSet(account, latest.prices)
				positions <- addPosition(positions, "AMP")
				positions <- addPosition(positions, "BHP")
				
				test_that(notices(positions), equals(character(0)))
			})
	
	test_that("One notice out of two", {
				
				account <- Account(10000)
				latest.prices <- c(AMP = 1)
				positions <- PositionSet(account, latest.prices)
				
				positions <- addPosition(positions, "AMP")
				
				bhp <- Position("BHP")
				bhp <- addNotice(bhp, 100)
				positions <- setPosition(positions, bhp)
				
				test_that(notices(positions), equals("buy 100 BHP"))
			})
	

context("Updating Positions after transactions")

				cleanMockMethods()

	test_that("Position size added with transaction", {
				
				broker <- Mock("Broker")
				transaction <- data.frame(size = 100, price = 1, costs = 6, 
						row.names = "AMP")
				mockMethod(broker, "transactions", transaction)
				mockMethod(broker, "latestPrices", numeric())
				mockMethod(broker, "tradeableInstruments", "AMP")
				
				manager <- Manager(Mock("Strategy"))
				manager@positions <- PositionSet(Account(0), numeric())
				manager@positions <- addPosition(manager@positions, "AMP")
				
				manager <- updatePositions(manager, broker)
				
				position <- getPosition(manager@positions, "AMP")
				
				expect_that(totalSize(position), matchesObject(100))
			})
	
	test_that("Position size subtracted with transaction", {
				
				broker <- Mock("Broker")
				transaction <- data.frame(size = -100, price = 1, costs = 6, 
						row.names = "AMP")
				mockMethod(broker, "transactions", transaction)
				mockMethod(broker, "latestPrices", numeric())
				mockMethod(broker, "tradeableInstruments", "AMP")
				
				manager <- Manager(Mock("Strategy"))
				manager@positions <- PositionSet(Account(0), numeric())
				amp <- Position("AMP", size = 100)
				manager@positions <- setPosition(manager@positions, amp)
				
				manager <- updatePositions(manager, broker)
				
				position <- getPosition(manager@positions, "AMP")
				
				expect_that(totalSize(position), matchesObject(0))
			})
	
	
	
	
	
	
	
	




	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	