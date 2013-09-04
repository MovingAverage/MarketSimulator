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
				expect_that(size_orders(position), matchesObject(order))
			})
	
	test_that("CurrentPosition throws error if wrong orders given", {
				
				bhp.orders <- list(Order("BHP", buy = 100))
				
				expect_that(Position("AMP", list(1)), throws_error("class Order"))
				expect_that(Position("AMP", bhp.orders), throws_error("instrument"))
			})

	
context("Position size calculations")

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
				
				position <- Position("AMP", list(), size = 100)
				
				expect_that(totalSize(position), 
						matchesObject(c(AMP = 100), ignore.attributes = FALSE))
			})
	
	test_that("Position with holding and open order adds them", {
				
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
	
	test_that("Position with MarketWithStop order reports on size", {
				
				position <- Position("AMP", list(
								MarketWithStop("AMP", buy = 100, stop.point = 0.02)))
				
				expect_that(totalSize(position), 
						matchesObject(c(AMP = 100), ignore.attributes = FALSE))
			})


context("Expected transaction value and size")
	
	test_that("Transaction size always given as integer", {
				
				latest.prices <- c(AMP = 5)
				
				manager <- Manager(Mock("StrategyInterface"))
				latestPrices(manager) <- latest.prices
				manager <- setupAccount(manager, 10000)
				manager <- setPosition(manager, Position("AMP"))
				manager <- addTarget(manager, Target("AMP", size = 1/7))
				position <- getPosition(manager, "AMP")
				size <- as.integer((1/7) * 10000 / latest.prices)
				
				expect_that(transactionSize(position), 
						matchesObject(c(AMP = size), ignore.attributes = FALSE))
			})
	
	test_that("When no existing position", {
				
				latest.prices <- c(AMP = 5)
				
				manager <- Manager(Mock("StrategyInterface"))
				latestPrices(manager) <- latest.prices
				manager <- setupAccount(manager, 10000)
				manager <- setPosition(manager, Position("AMP"))
				manager <- addTarget(manager, Target("AMP", size = 0.5))
				position <- getPosition(manager, "AMP")
				
				expect_that(transactionValue(position, manager), matchesObject(5000))
				expect_that(transactionSize(position), matchesObject(1000))
			})
	
	test_that("When has existing position", {
				
				latest.prices <- c(AMP = 5)
				
				manager <- Manager(Mock("StrategyInterface"))
				latestPrices(manager) <- latest.prices
				manager <- setupAccount(manager, 8000)
				manager <- setPosition(manager, Position("AMP", size = 400))
				manager <- addTarget(manager, Target("AMP", size = 0.5))
				position <- getPosition(manager, "AMP")
				
				expect_that(transactionValue(position, manager), matchesObject(3000))
				expect_that(transactionSize(position), matchesObject(600))
			})
	
	test_that("When has no position, but open order", {
				
				latest.prices <- c(AMP = 5)
				
				manager <- Manager(Mock("StrategyInterface"))
				latestPrices(manager) <- latest.prices
				manager <- setupAccount(manager, 10000)
				manager <- setPosition(manager, 
						Position("AMP", list(Order("AMP", buy = 400))))
				manager <- addTarget(manager, Target("AMP", size = 0.5))
				position <- getPosition(manager, "AMP")
				
				expect_that(transactionValue(position, manager), matchesObject(5000))
				expect_that(transactionSize(position), matchesObject(1000))
			})
	
	test_that("When existing position and open orders", {
				
				latest.prices <- c(AMP = 5)
				
				manager <- Manager(Mock("StrategyInterface"))
				latestPrices(manager) <- latest.prices
				manager <- setupAccount(manager, 9000)
				manager <- setPosition(manager, 
						Position("AMP", list(Order("AMP", buy = 200)), size = 200))
				manager <- addTarget(manager, Target("AMP", size = 0.5))
				position <- getPosition(manager, "AMP")
				
				expect_that(transactionValue(position, manager), matchesObject(4000))
				expect_that(transactionSize(position), matchesObject(800))
			})	
	
	
context("Sending orders to Broker")	
	
	test_that("Market order sent for new target", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				
				position <- Position("AMP")
				target <- Target("AMP", 0.5)
				quantity(target) <- 100
				position <- setTarget(position, target)
				expected.order <- Order("AMP", buy = 100)
				
				position <- sendOrders(Open(), position, broker)
				
				expect_that(broker, called_once_with("addOrder", expected.order))
				expect_that(notice(position), matchesObject("buy 100 AMP"))
			})
	
	test_that("Sell order sent for zero target", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				
				position <- Position("AMP", size = 100)
				expected.order <- Order("AMP", sell = 100)
				target <- Target("AMP", 0)
				quantity(target) <- 0
				position <- setTarget(position, target)
				
				position <- sendOrders(Open(), position, broker)
				
				expect_that(broker, called_once_with("addOrder", expected.order))
				expect_that(notice(position), matchesObject("sell all (100) AMP"))
			})
	
	test_that("Market and Stop order sent", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				
				position <- Position("AMP")
				target <- Target("AMP", 0.5, stop.point = 0.02)
				quantity(target) <- 100
				position <- setTarget(position, target)
				
				position <- sendOrders(Open(), position, broker)
				
				expect_that(broker, called_once_with("addOrder", 
								MarketWithStop("AMP", buy = 100, stop.point = 0.02)))
				expect_that(notice(position), matchesObject("buy 100 AMP"))
			})
	
	test_that("Position sends Market buy order", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				
				manager <- Mock("Manager")
				mockMethod(manager, "isViable", TRUE)
				mockMethod(manager, "latestPrices", c(AMP = 10))
				manager <- setupAccount(manager, 10000)
				
				position <- Position("AMP")
				manager <- setPosition(manager, position)
				manager <- addTarget(manager, Target("AMP", size = 0.5))
				
				manager <- placeOrders(manager, broker)
				
				expect_that(status(position), matchesObject(Open()))
				expect_that(broker, 
						called_once_with("addOrder", Order("AMP", buy = 500)))
			})
	
	test_that("Position sends Market sell order", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				
				manager <- Mock("Manager")
				mockMethod(manager, "isViable", TRUE)
				mockMethod(manager, "latestPrices", c(AMP = 10))
				manager <- setupAccount(manager, 5000)
				manager <- setPosition(manager, Position("AMP", size = 500))
				manager <- addTarget(manager, Target("AMP", size = 0))
				
				manager <- placeOrders(manager, broker)
				
				expect_that(broker, 
						called_once_with("addOrder", Order("AMP", sell = 500)))
			})
	
	test_that("Order not sent if existing open order", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				mockMethod(broker, "replaceOrder")
				
				manager <- Mock("Manager")
				mockMethod(manager, "isViable", TRUE)
				mockMethod(manager, "latestPrices", c(AMP = 10))
				manager <- setupAccount(manager, 10000)
				
				position <- Position("AMP", list(Order("AMP", buy = 500)))
				position <- setTarget(position, Target("AMP", size = 0.5))
				manager <- setPosition(manager, position)
				manager <- addTarget(manager, Target("AMP", size = 0.5))
				
				manager <- placeOrders(manager, broker)
				
				expect_that(broker, not_called("addOrder"))
				expect_that(broker, not_called("replaceOrder"))
			})
	
	test_that("Order replaced if existing but not correct size", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "replaceOrder")
				
				manager <- Mock("Manager")
				mockMethod(manager, "isViable", TRUE)
				mockMethod(manager, "latestPrices", c(AMP = 10))
				manager <- setupAccount(manager, 10000)
				
				order <- Order("AMP", buy = 300)
				order <- setID(order, "o1") 
				
				target <- Target("AMP", size = 0.5)
				
				position <- Position("AMP", list(order))
				position@target <- target
				manager <- setPosition(manager, position)
				manager <- addTarget(manager, target)
				
				position <- placeOrders(manager, broker)
				
				expected.order <- order
				expected.order@quantity <- 500L
				
				expect_that(broker, has_calls(replaceOrder(broker, expected.order)))
			})
	
	test_that("Order not sent if position stopped", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				
				manager <- Mock("Manager")
				mockMethod(manager, "isViable", TRUE)
				mockMethod(manager, "latestPrices", c(AMP = 10))
				mockMethod(manager, "targetPositions", list(Target("AMP", size = 0.5)))
				manager <- setupAccount(manager, 10000)
				
				position <- Position("AMP", size = 0)
				position@target <- Target("AMP", size = 0.5)
				position@state <- Stopped()
				manager <- setPosition(manager, position)
				
				manager <- placeOrders(manager, broker)
				
				expect_that(broker, not_called("addOrder"))
			})
	
	test_that("Position cancels stops when closed", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				mockMethod(broker, "cancelOrder")
				
				manager <- Manager(Mock("StrategyInterface"))
				manager <- setupAccount(manager, 10000)
				
				stop.order <- Stop("AMP", sell = 1000, at = xts())
				stop.order <- setID(stop.order, "o1")
				stop.order@status <- NullStatus()
				
				sell.order <- Order("AMP", sell = 1000)
				
				position <- Position("AMP", size = 1000)
				position@target <- Target("AMP", size = 1, stop.point = 0.02)
				position@orders <- list(o1 = stop.order)
				
				manager <- setPosition(manager, position)
				manager <- addTarget(manager, Target("AMP", size = 0))
				position <- getPosition(manager, "AMP")
				
				position <- sendOrders(status(position), position, broker)
				
				expect_that(broker, called_once_with("cancelOrder", stop.order))
				expect_that(broker, called_once_with("addOrder", sell.order))
			})
	
	test_that("Position adjusts existing stops when size modified", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				mockMethod(broker, "replaceOrder")
				
				current.stop <- Stop("AMP", sell = 1000, at = xts())
				current.stop <- setID(current.stop, "o1")
				current.stop@status <- OpenStatus()
				
				position <- Position("AMP", size = 1000)
				position@target <- Target("AMP", size = 1, stop.point = 0.02)
				position@orders <- list(o1 = current.stop)
				
				revised.target <- Target("AMP", size = 0.7, stop.point = 0.02)
				quantity(revised.target) <- 700
				position <- setTarget(position, revised.target)
				
				new.stop <- current.stop
				quantity(new.stop) <- -700L
				sell.order <- Order("AMP", sell = 300)
				
				position <- sendOrders(status(position), position, broker)
				
				expect_that(broker, called_once_with("replaceOrder", new.stop))
				expect_that(broker, called_once_with("addOrder", sell.order))
			})
	
	test_that("Position considers open orders when adjusting size", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "addOrder")
				mockMethod(broker, "replaceOrder")
				
				current.stop <- Stop("AMP", sell = 700, at = xts())
				current.stop <- setID(current.stop, "o1")
				
				current.adjust <- Order("AMP", sell = 300)
				current.adjust <- setID(current.adjust, "o2")
				
				position <- Position("AMP", 
						list(o1 = current.stop, o2 = current.adjust), 
						size = 1000)
				target <- Target("AMP", size = 0.7, stop.point = 0.02)
				quantity(target) <- 700
				position@target <- target
				status(position) <- Adjusting()
				
				position <- setTarget(position, position@target)
				
				position <- sendOrders(status(position), position, broker)
				
				expect_that(broker, not_called("replaceOrder"))
			})
	

context("Position state transitions")

	test_that("New position set to Open", {
				
				position <- Position("AMP")
				
				position <- setTarget(position, Target("AMP", size = 1))
				
				expect_that(status(position), matchesObject(Open()))
			})

	test_that("Closed position returns to Open", {
				
				position <- Position("AMP", size = 0)
				position@state <- Closed()
				
				new.target <- Target("AMP", size = 1, stop.point = 0.01)
				
				position <- setTarget(position, new.target)
				
				expect_that(status(position), matchesObject(Open()))
			})
	
	test_that("Stopped position Closed when sent zero target", {
				
				position <- Position("AMP", size = 0)
				position@state <- Stopped()
				position@target <- Target("AMP", size = 0.5)
				
				new.target <- Target("AMP", size = 0)
				
				position <- setTarget(position, new.target)
				
				expect_that(status(position), matchesObject(Closed()))
			})
	
	test_that("Open position Closed when sent zero target", {
				
				position <- Position("AMP", size = 100)
				position@target <- Target("AMP", size = 0.5)
				
				new.target <- Target("AMP", size = 0)
				
				position <- setTarget(position, new.target)
				
				expect_that(status(position), matchesObject(Closed()))
			})
	
	test_that("Open position set to Stopped", {
				
				position <- Position("AMP", size = 1000)
				position@target <- Target("AMP", size = 0.5)
				
				order <- Stop("AMP", sell = 1000, at = xts())
				
				position <- updateSize(position, list(order))
				
				expect_that(status(position), matchesObject(Stopped()))
			})
	
	test_that("Position set to Filled after reaching full size", {
				
				position <- Position("AMP", size = 1000)
				position@target <- Target("AMP", size = 1)
				
				manager <- Manager(Mock("StrategyInterface"))
				manager <- setupAccount(manager, 0)
				latestPrices(manager) <- c("AMP" = 10)
				manager <- setPosition(manager, position)
				
				manager <- addTarget(manager, Target("AMP", size = 1))
				
				position <- getPosition(manager, "AMP")
				
				expect_that(status(position), matchesObject(Filled()))
			})
	
	test_that("Position set to Adjusting if size of target changes", {
				
				position <- Position("AMP", size = 100)
				position@target <- Target("AMP", size = 1)
				status(position) <- Filled()
				
				position <- setTarget(position, Target("AMP", size = 0.5))
				
				expect_that(status(position), matchesObject(Adjusting()))
			})
	
	test_that("Stopped position not changed if set adjusting target", {
				
				position <- Position("AMP", size = 0)
				position@target <- Target("AMP", size = 1)
				status(position) <- Stopped()
				
				position <- setTarget(position, Target("AMP", size = 0.5))
				
				expect_that(status(position), matchesObject(Stopped()))
			})
	
	
context("Position order notices")

	test_that("Buy notice", {
				
				position <- Position("AMP")
				position <- addNotice(position, 100)
				
				expect_that(notice(position), matchesObject("buy 100 AMP"))
			})
	
	test_that("Sell notice", {
				
				position <- Position("AMP")
				position@target <- Target("AMP", 10)
				position <- addNotice(position, -100)
				
				expect_that(notice(position), matchesObject("sell 100 AMP"))
			})
	
	test_that("Sell all notice", {
				
				position <- Position("AMP")
				position@target <- Target("AMP", 0)
				position <- addNotice(position, -100)
				
				expect_that(notice(position), matchesObject("sell all (100) AMP"))
			})
	
	test_that("Empty notice returns empty character vector", {
				
				position <- Position("AMP")
				
				expect_that(notice(position), matchesObject(character(0)))
			})

	

	
	
	
	




	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	