#'
#' High level test suites for MarketSimulator package

context("__ Use Cases __")

context("Order execution")

	test_that("Position changed at open", {
				
				stocks <- loadStocks(c("AMP.AX", "BHP.AX"))
				stocks <- lapply(stocks, function(stock) stock[-1])
				market <- Market(stocks)
				amp.open <- Op(stocks[[1]][1])
				bhp.open <- Op(stocks[[2]][1])
				timestamp <- index(stocks[[1]][1])
				
				broker <- Broker()
				broker <- addMarket(broker, market)
				
				addOrder(broker, Order("AMP.AX", buy = 100))
				addOrder(broker, Order("BHP.AX", sell = 100))
				
				market.report <- marketActivity(broker, timestamp)
				amp.orders <- closedOrders(broker, "AMP.AX")
				bhp.orders <- closedOrders(broker, "BHP.AX")
				
				expect_that(status(amp.orders[[1]]), matchesObject("closed"))
				expect_that(status(bhp.orders[[1]]), matchesObject("closed"))
				expect_that(execution_price(amp.orders[[1]]), matchesObject(amp.open))
				expect_that(execution_price(bhp.orders[[1]]), matchesObject(bhp.open))
			})
	
	test_that("Sell Stop set and executed on day", {
				
				AMP.bar <- loadStocks("AMP.AX")[[1]][2]
				AMP.bar[, "AMP.AX.Open"] <- 10.0
				AMP.bar[, "AMP.AX.High"] <- 10.15
				AMP.bar[, "AMP.AX.Low"] <- 9.85
				AMP.bar[, "AMP.AX.Close"] <- 10.1
				timestamp <- index(AMP.bar)
				
				market <- Mock("Market")
				mockMethod(market, "tradeableInstruments", "AMP.AX")
				mockMethod(market, "getBar", return.value = AMP.bar)
				
				broker <- Broker()
				broker <- addMarket(broker, market)
				
				addOrder(broker, Order("AMP.AX", buy = 100))
				addOrder(broker, Stop("AMP.AX", sell = 100, at = Op(AMP.bar) * 0.99))
				
				marketActivity(broker, timestamp)
				amp.orders <- closedOrders(broker, "AMP.AX")
				
				expect_that(status(amp.orders[[1]]), matchesObject("closed"))
				expect_that(status(amp.orders[[2]]), matchesObject("closed"))
			})
	
	test_that("Sell Stop set but not executed on day", {
				
				AMP.bar <- loadStocks("AMP.AX")[[1]][2]
				AMP.bar[, "AMP.AX.Open"] <- 10.0
				AMP.bar[, "AMP.AX.High"] <- 10.15
				AMP.bar[, "AMP.AX.Low"] <- 10.0
				AMP.bar[, "AMP.AX.Close"] <- 10.1
				timestamp <- index(AMP.bar)
				
				market <- Mock("Market")
				mockMethod(market, "tradeableInstruments", "AMP.AX")
				mockMethod(market, "getBar", return.value = AMP.bar)
				
				broker <- Broker()
				broker <- addMarket(broker, market)
				
				addOrder(broker, Order("AMP.AX", buy = 100))
				addOrder(broker, Stop("AMP.AX", sell = 100, at = Op(AMP.bar) * 0.99))
				
				marketActivity(broker, timestamp)
				open.orders <- openOrders(broker, "AMP.AX")
				closed.orders <- closedOrders(broker, "AMP.AX")
				
				expect_that(status(closed.orders[[1]]), matchesObject("closed"))
				expect_that(status(open.orders[[1]]), matchesObject("open"))
			})
	
	
context("Price sequences")

	test_that("Market order executed and position stopped out next day", {
				
				OHLC <- paste("AMP.AX", 
						c("Open", "High", "Low", "Close", "Volume", "Adjusted"), 
						sep = ".")
				day1 <- c(10, 10.2, 9.9, 10, 1000, 0)
				names(day1) <- OHLC
				day2 <- c(9.9, 10.0, 9.5, 9.6, 1000, 0)
				names(day2) <- OHLC
				day1 <- xts(t(day1), order.by = as.Date("2007-01-01"))
				days <- rbind(day1, xts(t(day2), order.by = as.Date("2007-01-02")))
				
				broker <- Broker()
				broker <- addMarket(broker, Market(list("AMP.AX" = days)))
				
				strategy <- Mock("Strategy")
				mockMethod(strategy, "targetPositions", list(Target("AMP.AX", 1, 0.02)))
				
				manager <- Manager(strategy)
				manager <- setupAccount(manager, 10000)
				manager <- setPosition(manager, Position("AMP.AX"))
				
				order <- MarketWithStop("AMP.AX", buy = 1000, stop.point = 0.02)
				addOrder(broker, order)
				
				BackTest(manager, broker, as.Date("2007-01-01"), as.Date("2007-01-02"))
				
				expect_that(length(openOrders(broker, "AMP.AX")), matchesObject(0))
				expect_that(length(closedOrders(broker, "AMP.AX")), matchesObject(2))
			})
	
	
	
	
	
	
	