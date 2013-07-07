#'
#' Broker object manages orders and checks them against daily data.
#' Accepts orders, and returns executed orders.
#' Reads market data information (xts), as per \link{quantmod}.

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
				orders <- openOrders(broker, "AMP")
				
				expect_that(getID(orders[[1]]), equals(1))
				expect_that(getID(orders[[2]]), equals(2))
			})
	
	test_that("Broker assigns transaction cost function to order", {
				
				broker <- Broker()
				txn_cost_model <- function(order) NULL
				broker <- addTxnCostModel(broker, txn_cost_model)
				order <- Order("AMP", buy = 100)
				addOrder(broker, order)
				order <- openOrders(broker, "AMP")[[1]]
				
				expect_that(order@txn.cost.model, matchesObject(txn_cost_model))
			})
	
	test_that("Broker stores open orders", {
				
				order1 <- Order("AMP", buy = 100)
				order2 <- Order("AMP", buy = 100)
				broker <- Broker()
				addOrder(broker, order1)
				addOrder(broker, order2)
				
				order1 <- setID(order1, 1L)
				order1 <- setTxnCostModel(order1, default_cost_model)
				order2 <- setID(order2, 2L)
				order2 <- setTxnCostModel(order2, default_cost_model)
				
				expect_that(openOrders(broker, "AMP"), 
						matchesObject(list(order1, order2)))
			})
	
	test_that("Broker updates orders", {
				
				order <- Order("AMP", buy = 100)
				broker <- Broker()
				addOrder(broker, order)
				order@status <- "closed"
				order <- setID(order, 1L)
				updateOrder(broker, order)
				
				orders <- closedOrders(broker, "AMP")
				
				expect_that(length(orders), equals(1))
				expect_that(status(orders[[1]]), equals("closed"))
			})
	
	test_that("Broker notifies orders of new price bar", {
				
				broker <- Broker()
				
				order1 <- Mock("MarketOrder")
				order2 <- Mock("MarketOrder")
				mockMethod(list(order1, order2), "notify", order1)
				mockMethod(list(order1, order2), "instrumentOf", "AMP")
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
				addOrderToBook(broker, Order("CBA", sell = 100), "closed.orders")
				
				expect_that(activeInstruments(broker), matchesObject(c("AMP", "BHP")))
				expect_that(tradedInstruments(broker), matchesObject("CBA"))
			})

context("Order book storage")

	test_that("Order removed from open orders", {
				
				broker <- Broker()
				
				amp.order <- Order("AMP", buy = 100)
				addOrder(broker, amp.order)
				bhp.order <- Order("BHP", buy = 100)
				addOrder(broker, bhp.order)
				amp.order <- setID(amp.order, 1L)
				bhp.order <- setID(bhp.order, 1L)
				bhp.order <- setTxnCostModel(bhp.order, default_cost_model)
				
				removeFromOpenOrders(broker, amp.order)
				
				expect_that(openOrders(broker, "AMP"), matchesObject(list()))
				expect_that(openOrders(broker, "BHP"), matchesObject(list(bhp.order)))
			})

	test_that("Closed orders moved to closed order book", {
				
				order <- Order("AMP", buy = 100)
				
				broker <- Broker()
				addOrder(broker, order)
				
				order <- setID(order, 1L)
				order@status <- "closed"
				updateOrder(broker, order)
				
				expect_that(openOrders(broker, "AMP"), matchesObject(list()))
				expect_that(closedOrders(broker, "AMP"), matchesObject(list(order)))
			})

	test_that("Closed orders not notified of market activity", {
				
				open.order1 <- Mock("Order")
				open.order2 <- Mock("Order")
				closed.order <- Mock("Order")
				
				mockMethod(list(open.order1, open.order2, closed.order), "instrumentOf", 
						return.value = "AMP.AX")
				mockMethod(list(open.order1, open.order2, closed.order), "notify")
				
				broker <- Broker()
				addOrder(broker, open.order1)
				addOrder(broker, open.order2)
				addOrder(broker, closed.order)
				
				closed.order <- setID(closed.order, 3L)
				closed.order@status <- "closed"
				updateOrder(broker, closed.order)
				
				price.bar <- loadStocks("AMP.AX")[[1]][2]
				notifyOrders(broker, "AMP.AX", price.bar)
				
				expect_that(open.order1, called_once("notify"))
				expect_that(open.order2, called_once("notify"))
				expect_that(closed.order, not_called("notify"))
			})
	
context("Order book output")

	test_that("Broker prints order book", {
				
				broker <- Broker()
				
				order <- Order("AMP", buy = 100)
				order@status <- "closed"
				order@execution.price <- xts(10.0, order.by = as.Date("2010-02-04"))
				order@txn.cost.model <- default_cost_model
				
				addOrderToBook(broker, order, "closed.orders")
				
				timestamp <- "1999-12-31"
				expected.order.book <- xts(t(c(
								"Order.Qty" = 0, 
								"Order.Price" = NA, 
								"Order.Type" = "init", 
								"Order.Side" = "long",
								"Order.Threshold" = 0, 
								"Order.Status" = "closed", 
								"Order.StatusTime" = as.character(as.POSIXct(timestamp)), 
								"Prefer" = "", 
								"Order.Set" = "", 
								"Txn.Fees" = 0, 
								"Rule" = "")), 
						order.by = as.POSIXct(timestamp))
				expected.order.book <- rbind(expected.order.book, bookEntry(order))
				
				order.book <- printOrderBook(broker, portfolio = "test")
				
				expect_that(order.book, is_a("order_book"))
				expect_that(names(order.book), equals("test"))
				expect_that(names(order.book[[1]]), equals("AMP"))
				expect_that(order.book[[1]][[1]], matchesObject(expected.order.book))
			})
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

