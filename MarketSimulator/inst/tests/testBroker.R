#'
#' Broker object manages orders and checks them against daily data.
#' Accepts orders, and returns executed orders.
#' Reads market data information (xts), as per \link{quantmod}.

load_all("D:/Code/R/Development/Packages/mockR")
load_all("D:/Code/R/Development/Packages/DataHandlers")

context("__ Broker __")

context("Reading market data")

	test_that("Broker keeps pointer to market data objects", {
				
				broker <- new("Broker")
				market <- list()
				broker <- addMarket(broker, market)
				expect_that(broker@market, equals(market))
			})

	test_that("Broker gets bar data for timestamp", {
				
				market <- Mock("list")
				AMP.AX <- loadStocks("AMP.AX")[[1]]
				mockMethod(market, "getMarketInstrument", AMP.AX)
				
				broker <- new("Broker")
				broker <- addMarket(broker, market)

				ticker <- "AMP.AX"
				timestamp <- "2010-01-04"
				bar <- getBar(broker, ticker, "2010-01-04")
				
				expect_that(market, called_once_with("getMarketInstrument", ticker))
				expect_that(bar, equals(AMP.AX[timestamp]))
			})


context("Order handling")

	test_that("Broker only accepts orders of class 'MarketOrder'", {
				
				broker <- new("Broker")
				expect_that(addOrder(broker, list()), throws_error())
			})
	
	test_that("Broker stores open orders", {
				
				order1 <- Mock("MarketOrder")
				order2 <- Mock("MarketOrder")
				broker <- new("Broker")
				broker <- addOrder(broker, order1)
				broker <- addOrder(broker, order2)
				
				expect_that(broker@orders, matchesObject(list(order1, order2)))
			})
	
	test_that("Broker notifies orders of new price bar", {
				
				broker <- new("Broker")
				market <- Mock("list")
				AMP <- loadStocks("AMP.AX")[[1]]
				mockMethod(market, "getMarketInstrument", AMP)
				
				order1 <- Mock("MarketOrder")
				order2 <- Mock("MarketOrder")
				mockMethod(list(order1, order2), "notifyOrder")
				
				broker <- addMarket(broker, market)
				broker <- addOrder(broker, order1)
				broker <- addOrder(broker, order2)
				
				price.bar <- AMP[1, ]
				notifyOrders(broker, price.bar)
				
				expect_that(order1, called_once_with("notifyOrder", broker, price.bar))
				expect_that(order2, called_once_with("notifyOrder", broker, price.bar))
			})
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

