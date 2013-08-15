#'
#' Broker object manages orders and checks them against daily data.
#' Accepts orders, and returns executed orders.
#' Reads market data information (xts), as per \link{quantmod}.

context("__ Broker __")

cleanMockMethods()

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
	
	test_that("Broker records current day for future reference", {
				
				market <- Mock("Market")
				ticker <- "AMP.AX"
				AMP.AX <- loadStocks(ticker)[[1]]
				timestamp <- "2010-01-04"
				mockMethod(market, "getBar", AMP.AX[timestamp])
				
				broker <- Broker()
				broker <- addMarket(broker, market)
				
				bar <- getBar(broker, ticker, timestamp)
				
				expect_that(today(broker), 
						matchesObject(timestamp, ignore.attributes = FALSE))
			})


context("Order handling")

	test_that("Broker only accepts orders of class 'MarketOrder'", {
				
				broker <- Broker()
				expect_that(addOrder(broker, list()), throws_error())
			})
	
	test_that("Broker assigns orderID to order when added", {
				
				broker <- Broker()
				order1 <- Order("AMP", buy = 100)
				order2 <- Limit("AMP", buy = 100, at = xts())
				addOrder(broker, order1)
				addOrder(broker, order2)
				orders <- openOrders(broker, "AMP")
				
				expect_that(getID(orders[[1]]), equals(1))
				expect_that(getID(orders[[2]]), equals(2))
			})
	
	test_that("Broker assigns submission date when order added", {
				
				broker <- Broker()
				todays.date <- as.POSIXct("2010-04-20")
				setTodaysDate(broker, todays.date)
				
				addOrder(broker, Order("AMP", buy = 100))
				order <- openOrders(broker, "AMP")[[1]]
				
				expect_that(submissionTime(order), 
						matchesObject(todays.date, ignore.attributes = FALSE))
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
				order2 <- Stop("AMP", buy = 100, at = xts())
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
				order@status <- ClosedStatus()
				order@execution.price <- xts(10, order.by = as.Date("2010-04-20"))
				order <- setID(order, 1L)
				order <- setTxnCostModel(order, default_cost_model)
				updateOrder(order, broker)
				
				orders <- closedOrders(broker, "AMP")
				
				expect_that(length(orders), equals(1))
				expect_that(status(orders[[1]]), equals("closed"))
			})
	
	test_that("Broker checks if similar existing Market order", {
				
				broker <- Broker()
				addOrder(broker, Order("AMP", buy = 100))
				
				expect_that(hasSimilarOrder(broker, Order("AMP", buy = 50)), is_true())
				expect_that(hasSimilarOrder(broker, Order("AMP", sell = 50)), is_true())
				expect_that(hasSimilarOrder(broker, Order("BHP", buy = 50)), is_false())
				expect_that(hasSimilarOrder(broker, Limit("AMP", buy = 50, at = xts())), 
						is_false())
				expect_that(hasSimilarOrder(broker, Stop("AMP", buy = 50, at = xts())), 
						is_false())
			})
	
	test_that("Broker combines orders into one when appropriate", {
				
				broker <- Broker()
				
				addOrder(broker, Order("AMP", buy = 100))
				addOrder(broker, Order("AMP", buy = 100))
				
				addOrder(broker, Order("BHP", buy = 100))
				addOrder(broker, Order("BHP", sell = 100))
				
				expected.order <- Order("AMP", buy = 200)
				expected.order <- setID(expected.order, 1L)
				expected.order <- setTxnCostModel(expected.order, broker@cost.model)
				
				amp.orders <- openOrders(broker, "AMP")
				bhp.orders <- openOrders(broker, "BHP")
				
				expect_that(length(amp.orders), equals(1))
				expect_that(amp.orders[[1]], matchesObject(expected.order))
				expect_that(length(bhp.orders), matchesObject(0))
			})

	
context("Notifications of market activity")
	
	test_that("Broker does nothing if no active orders", {
				
				broker <- Broker()
				market <- Mock("Market")
				mockMethod(market, "getBar")
				broker <- addMarket(broker, market)
				
				marketActivity(broker, as.Date("2010-03-05"))
				
				expect_that(market, not_called("getBar"))
			})

	test_that("Broker notifies orders of new price bar", {
				
				broker <- Broker()
				
				order1 <- Mock("MarketOrder")
				order2 <- Mock("LimitOrder")
				mockMethod(list(order1, order2), "notify", order1)
				mockMethod(list(order1, order2), "instrumentOf", "AMP")
				mockMethod(list(order1, order2), "submissionTime", initDate())
				
				addOrderToBook(broker, order1, order.book = "open.orders")
				addOrderToBook(broker, order2, order.book = "open.orders")
				
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
				expect_that(activeAndTradedInstruments(broker), 
						matchesObject(c("AMP", "BHP", "CBA")))
			})
	
	test_that("Broker takes note of prices after market activity", {
				
				broker <- Broker()
				stocks <- loadStocks(c("AMP.AX", "BHP.AX", "CBA.AX"))
				market <- Market(stocks)
				broker <- addMarket(broker, market)
				addOrder(broker, Order("AMP.AX", buy = 100))
				addOrder(broker, Order("BHP.AX", buy = 100))
				addOrderToBook(broker, Order("CBA.AX", sell = 100), "closed.orders")
				
				timestamp <- index(stocks[[1]][2])
				latest.prices <- c(
								AMP.AX = as.numeric(Cl(stocks[["AMP.AX"]][timestamp])),
								BHP.AX = as.numeric(Cl(stocks[["BHP.AX"]][timestamp])),
								CBA.AX = as.numeric(Cl(stocks[["CBA.AX"]][timestamp])))
				
				marketActivity(broker, timestamp)
				
				expect_that(latestPrices(broker), 
						matchesObject(latest.prices, ignore.attributes = FALSE))
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
				order@status <- ClosedStatus()
				order@execution.price <- xts(10, order.by = as.Date("2010-04-20"))
				order <- setTxnCostModel(order, default_cost_model)
				updateOrder(order, broker)
				
				
				expect_that(openOrders(broker, "AMP"), matchesObject(list()))
				expect_that(closedOrders(broker, "AMP"), matchesObject(list(order)))
			})

	test_that("Closed orders not notified of market activity", {
				
				open.order <- Mock("MarketOrder")
				closed.order <- Mock("MarketOrder")
				
				mockMethod(list(open.order, closed.order), "instrumentOf", 
						return.value = "AMP.AX")
				mockMethod(list(open.order, closed.order), "submissionTime", 
						return.value = initDate())
				mockMethod(list(open.order, closed.order), "notify")
				mockMethod(list(open.order, closed.order), "quantity", 
						return.value = 100)
				
				broker <- Broker()
				addOrder(broker, open.order)
				addOrderToBook(broker, closed.order, order.book = "closed.orders")
				
				price.bar <- loadStocks("AMP.AX")[[1]][2]
				notifyOrders(broker, "AMP.AX", price.bar)
				
				expect_that(open.order, called_once("notify"))
				expect_that(closed.order, not_called("notify"))
			})
	
context("Transaction records")

	test_that("Broker stores transaction record for closed order", {
				
				cleanMockMethods()
				amp.transaction <- data.frame(
						size = 50, price = 0.8, costs = 6, row.names = "AMP")
				bhp.transaction <- data.frame(
						size = 100, price = 0.5, costs = 6, row.names = "BHP")
				transactions <- rbind(amp.transaction, bhp.transaction)
				
				amp.order <- Mock("MarketOrder")
				amp.order@status <- ClosedStatus()
				amp.order@instrument <- "AMP"
				amp.order <- setID(amp.order, 1L)
				mockMethod(amp.order, "writeTransaction", amp.transaction)
				
				bhp.order <- Mock("MarketOrder")
				bhp.order@status <- ClosedStatus()
				bhp.order@instrument <- "BHP"
				bhp.order <- setID(bhp.order, 2L)
				mockMethod(bhp.order, "writeTransaction", bhp.transaction)
				
				broker <- Broker()
				addOrder(broker, amp.order)
				addOrder(broker, bhp.order)
				
				updateOrder(amp.order, broker)
				updateOrder(bhp.order, broker)
				
				expect_that(transactions(broker), matchesObject(transactions))
			})
	
	test_that("Broker clears transactions", {
				
				broker <- Broker()
				amp.transaction <- data.frame(
						size = 50, price = 0.8, costs = 6, row.names = "AMP")
				bhp.transaction <- data.frame(
						size = 100, price = 0.5, costs = 6, row.names = "BHP")
				transactions <- rbind(amp.transaction, bhp.transaction)
				assign("transactions", transactions, broker@transactions)
				
				clearTransactions(broker)
				
				expect_that(transactions(broker), matchesObject(data.frame()))
			})
	
context("Order book output")

	test_that("Broker prints order book", {
				
				broker <- Broker()
				
				order <- Order("AMP", buy = 100)
				order@status <- ClosedStatus()
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
	
	test_that("Broker adds transactions to portfolio", {
				
				portfolio <- Mock("character")
				# Method for adding transaction to blotter portfolio
				mockMethod(portfolio, "addTxn")
				
				broker <- Broker()
				
				instrument <- "AMP"
				order <- Order(instrument, buy = 100)
				order@status <- ClosedStatus()
				order@execution.price <- xts(10.0, order.by = as.Date("2010-02-04"))
				order@txn.cost.model <- default_cost_model
				
				addOrderToBook(broker, order, "closed.orders")
				
				portfolioTxns(broker, portfolio, verbose = TRUE)
				
				expect_that(portfolio, has_calls(addTxn(portfolio, instrument, 
										TxnDate = statusTime(order), 
										TxnQty = quantity(order), 
										TxnPrice = execution_price(order), 
										TxnFees = txnFees(order), 
										verbose = TRUE)))
			})
	
	
	

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

