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
				timstamp <- index(AMP.bar)
				
				market <- Mock("Market")
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
				timstamp <- index(AMP.bar)
				
				market <- Mock("Market")
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
	

	
	
	
	
	
	
	
	
	
	