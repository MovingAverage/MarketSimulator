#'
#' Broker object manages orders and checks them against daily data.
#' Accepts orders, and returns executed orders.
#' Reads market data information (xts), as per \link{quantmod}.

load_all("D:/Code/R/Development/Packages/mockR")
load_all("D:/Code/R/Development/Packages/DataHandlers")

context("__ Broker __")

context("Reading market data")

	test_that("Broker keeps reference to market data objects", {
				
				broker <- Broker()
				market <- Market() 
				broker <- addMarket(broker, market)
				expect_that(broker@market, equals(market))
			})

	test_that("Broker gets bar data for timestamp", {
				
				market <- Mock("Market")
				ticker <- "AMP.AX"
				AMP.AX <- loadStocks(ticker)[[1]]
				timestamp <- "2010-01-04"
				mockMethod(market, "getBar", AMP.AX[timestamp])
				
				broker <- Broker()
				broker <- addMarket(broker, market)

				bar <- getBar(broker, ticker, timestamp)
				
				expect_that(market, called_once_with("getBar", ticker, timestamp))
				expect_that(bar, equals(AMP.AX[timestamp]))
			})


context("Order handling")

	test_that("Broker only accepts orders of class 'MarketOrder'", {
				
				broker <- Broker()
				expect_that(addOrder(broker, list()), throws_error())
			})
	
	test_that("Broker assigns orderID to order when added", {
				
				broker <- Broker()
				order1 <- Order("AMP", buy = 100)
				order2 <- Order("AMP", buy = 100)
				addOrder(broker, order1)
				addOrder(broker, order2)
				orders <- getOrders(broker, "AMP")
				
				expect_that(getID(orders[[1]]), equals(1))
				expect_that(getID(orders[[2]]), equals(2))
			})
	
	test_that("Broker stores open orders", {
				
				order1 <- Order("AMP", buy = 100)
				order2 <- Order("AMP", buy = 100)
				broker <- Broker()
				addOrder(broker, order1)
				addOrder(broker, order2)
				
				order1 <- setID(order1, 1)
				order2 <- setID(order2, 2)
				
				expect_that(getOrders(broker, "AMP"), matchesObject(list(order1, order2)))
			})
	
	test_that("Broker updates orders", {
				
				order <- Order("AMP", buy = 100)
				broker <- Broker()
				addOrder(broker, order)
				order@status <- "closed"
				order <- setID(order, 1)
				updateOrder(broker, order)
				
				orders <- getOrders(broker, "AMP")
				
				expect_that(length(orders), equals(1))
				expect_that(status(orders[[1]]), equals("closed"))
			})
	
	test_that("Broker notifies orders of new price bar", {
				
				broker <- Broker()
				
				order1 <- Mock("MarketOrder")
				order2 <- Mock("MarketOrder")
				mockMethod(list(order1, order2), "notify", order1)
				mockMethod(list(order1, order2), "instrumentOf", "AMP")
				mockMethod(list(order1, order2), "getID", 1)
				addOrder(broker, order1)
				addOrder(broker, order2)
				
				price.bar <- loadStocks("AMP.AX")[[1]][2]
				notifyOrders(broker, "AMP", price.bar)
				
				expect_that(order1, called_once_with("notify", broker, price.bar))
				expect_that(order2, called_once_with("notify", broker, price.bar))
			})
	
	test_that("Broker reports on instruments with orders", {
				
				broker <- Broker()
				
				addOrder(broker, Order("AMP", buy = 100))
				addOrder(broker, Order("BHP", buy = 100))
				addOrder(broker, Order("CBA", sell = 100))
				
				expect_that(instruments(broker), matchesObject(c("AMP", "BHP", "CBA")))
			})
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

