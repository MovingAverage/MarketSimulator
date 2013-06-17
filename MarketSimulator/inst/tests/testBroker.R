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

