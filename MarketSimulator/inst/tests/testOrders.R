#'
#'

context("__ Orders __")

context("Order creation")

	test_that("Order requires an instrument identifier and order size", {
				
				instrument <- "AMP.AX"
				
				expect_that(Order(), throws_error("Require an instrument identifier"))
				expect_that(Order("AMP.AX"), throws_error("Require an order quantity"))
				expect_that(Order(100, 100), throws_error("character"))
				expect_that(Order("AMP.AX", buy = 100), is_a("MarketOrder"))
				expect_that(Order(instrument, sell = 100), is_a("MarketOrder"))
			})
	
	test_that("Order quantity processing", {
				
				expect_that(Order("AMP.AX", buy = 10, sell = 10), throws_error())
				expect_that(quantity(Order("A", buy = 10)), equals(10L))
				expect_that(quantity(Order("A", buy = -10)), equals(10L))
				expect_that(quantity(Order("A", sell = 10)), equals(-10L))
				expect_that(quantity(Order("A", sell = -10)), equals(-10L))
			})
	
	test_that("Order reports on related instrument", {
				
				ticker <- "AMP"
				market.order <- Order(ticker, buy = 100)
				stop.order <- Stop(ticker, buy = 100, at = xts())
				limit.order <- Limit(ticker, buy = 100, at = xts())
				
				expect_that(instrumentOf(market.order), matchesObject(ticker))
				expect_that(instrumentOf(stop.order), matchesObject(ticker))
				expect_that(instrumentOf(limit.order), matchesObject(ticker))
			})
	
	test_that("Setting quantity of order converts to integer", {
				
				order <- new("Order")
				quantity(order) <- 100
				expect_that(quantity(order), equals(100L))
			})

context("Order time stamping")
	
	test_that("Order timestamped when status changed", {
				
				timestamp <- as.POSIXct("2010-04-21")
				price.bar <- xts(t(c("AMP.Open" = 10, "AMP.Volume" = 1000)), 
						order.by = timestamp)
				broker <- Mock("Broker")
				mockMethod(broker, "closeOrder")
				
				order <- Order("AMP", buy = 100)
				notify(order, broker, price.bar)
				
				notified.order <- get_call_argument(broker, "closeOrder", 1)
				
				expect_that(statusTime(notified.order), matchesObject(timestamp))
			})
	
context("Order printing")

	test_that("Orders print as per blotter order book", {
				
				timestamp <- as.POSIXct("2010-04-20")
				price.bar <- xts(10.0, order.by = timestamp)
				order <- Order("AMP", buy = 100)
				order@status <- ClosedStatus()
				order@execution.price <- price.bar
				order@txn.cost.model <- default_cost_model
				submissionTime(order) <- timestamp
				
				output <- bookEntry(order)
				
				expected.names <- c("Order.Qty", "Order.Price", "Order.Type", 
						"Order.Side", "Order.Threshold", "Order.Status", 
						"Order.StatusTime", "Prefer", "Order.Set", "Txn.Fees", "Rule")
				
				expect_that(names(output), matchesObject(expected.names))
				expect_that(index(output), matchesObject(timestamp))
				expect_that(output[, "Order.Qty"], matchesObject("100"))
				expect_that(output[, "Order.Price"], matchesObject("10"))
				expect_that(output[, "Order.Status"], matchesObject("closed"))
				expect_that(output[, "Order.Type"], matchesObject("MarketOrder"))
				expect_that(output[, "Txn.Fees"], matchesObject("0"))
			})
	
	test_that("Orders write transaction record", {
				
				timestamp <- as.POSIXct("2010-04-20")
				price.bar <- xts(10.0, order.by = timestamp)
				order <- Order("AMP", buy = 100)
				order@status <- ClosedStatus()
				order@execution.price <- price.bar
				order@txn.cost.model <- default_cost_model
				submissionTime(order) <- timestamp
				
				output <- writeTransaction(order)
				
				expected.output <- data.frame(size = 100, price = 10.0, costs = 0, 
						row.names = "AMP")
				
				expect_that(output, matchesObject(expected.output, 
								ignore.attributes = FALSE))
			})

context("Merging orders")

	test_that("Merge orders throws errors when not similar", {
				
				amp.market <- Order("AMP", buy = 100)
				bhp.market <- Order("BHP", buy = 100)
				amp.limit <- Limit("AMP", buy = 100, at = xts())
				
				expect_that(mergeOrders(amp.market, amp.limit), 
						throws_error("Attempt to merge different order types"))
				expect_that(mergeOrders(amp.market, bhp.market), 
						throws_error("Instruments differ between orders"))
			})
	
	test_that("Two buy Market orders merged into one", {
				
				existing.order <- Order("AMP", buy = 10)
				existing.order <- setID(existing.order, "o1")
				
				new.order1 <- Order("AMP", buy = 10)
				expected.order1 <- Order("AMP", buy = 20)
				expected.order1 <- setID(expected.order1, getID(existing.order))
				
				new.order2 <- Order("AMP", sell = 5)
				expected.order2 <- Order("AMP", buy = 5)
				expected.order2 <- setID(expected.order2, getID(existing.order))
				
				expect_that(mergeOrders(existing.order, new.order1), 
						matchesObject(expected.order1))
				expect_that(mergeOrders(existing.order, new.order2), 
						matchesObject(expected.order2))
				
			})
	
	test_that("Stop orders are not merged unless same trigger price", {
				
				amp <- loadStocks("AMP.AX")[[1]][2]
				
				stop1 <- Stop("AMP", sell = 1000, at = Op(amp) * 0.98)
				stop2 <- Stop("AMP", sell = 500, at = Op(amp) * 0.98)
				stop3 <- Stop("AMP", buy = 700, at = Op(amp) * 0.98)
				stop4 <- Stop("AMP", buy = 700, at = Op(amp) * 0.95)
				
				expected.merge12 <- Stop("AMP", sell = 1500, at = Op(amp) * 0.98)
				expected.merge13 <- Stop("AMP", sell = 300, at = Op(amp) * 0.98)
				
				expect_that(areSimilar(stop1, stop2), is_true())
				expect_that(areSimilar(stop1, stop3), is_true())
				expect_that(areSimilar(stop2, stop3), is_true())
				expect_that(areSimilar(stop1, stop4), is_false())
				expect_that(mergeOrders(stop1, stop2), matchesObject(expected.merge12))
				expect_that(mergeOrders(stop1, stop3), matchesObject(expected.merge13))
			})
	
	
	
context("Market order on open processing")
	
	test_that("Order takes no action if not related to instrument", {
				
				broker <- Broker()
				AMP <- loadStocks("AMP.AX")[[1]]
				price.bar <- AMP[2]
				
				bhp.order <- Order("BHP.AX", 100)
				addOrder(broker, bhp.order)
				
				notify(bhp.order, broker, price.bar)
				order <- openOrders(broker, "BHP.AX")[[1]]
				
				expect_that(status(order), equals("open"))
			})
	
	test_that("MarketOrder executes on Open", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "closeOrder")
				AMP <- loadStocks("AMP.AX")[[1]]
				price.bar <- AMP[2]
				
				order <- Order("AMP.AX", buy = 100)
				expected.order <- order
				expected.order@status <- ClosedStatus()
				expected.order@execution.price <- Op(price.bar)
				submissionTime(expected.order) <- initDate()
				statusTime(expected.order) <- index(price.bar)
				
				notify(order, broker, price.bar)
				
				expect_that(broker, called_once_with("closeOrder", expected.order))
			})
	
	test_that("MarketOrder takes no action if no market activity", {
				
				AMP <- loadStocks("AMP.AX")[[1]]
				price.bar <- AMP[1]
				missing.volume <- missing.open <- zero.volume <- zero.open <- price.bar
				missing.volume[, "AMP.AX.Volume"] <- NA
				missing.open[, "AMP.AX.Open"] <- NA
				zero.volume[, "AMP.AX.Volume"] <- 0
				zero.open[, "AMP.AX.Open"] <- 0
				weekend.day <- AMP["2007-01-06"]
				
				broker <- Broker()
				order <- Order("AMP.AX", buy = 100)
				addOrder(broker, order)
				
				notify(order, broker, missing.volume)
				notify(order, broker, missing.open)
				notify(order, broker, zero.volume)
				notify(order, broker, zero.open)
				notify(order, broker, weekend.day)
				
				order <- openOrders(broker, "AMP.AX")[[1]]
				
				expect_that(active_market(missing.volume), is_false())
				expect_that(active_market(missing.open), is_false())
				expect_that(active_market(zero.volume), is_false())
				expect_that(active_market(zero.open), is_false())
				expect_that(active_market(weekend.day), is_false())
				expect_that(status(order), equals("open"))
			})

context("Stop loss order processing") 

	test_that("Creating stop loss orders", {
				
				stop.loss <- Stop("AMP.AX", sell = 100, at = xts())
				expect_that(Stop("AMP.AX", sell = 100), throws_error())
				expect_that(is(stop.loss, "Order"), is_true())
			})
	
	test_that("Stop order takes no action when market inactive", {
				
				price.bar <- loadStocks("AMP.AX")[[1]][2]
				price.bar[, "AMP.AX.Open"] <- 10.0
				price.bar[, "AMP.AX.Low"] <- 9.90
				price.bar[, "AMP.AX.Volume"] <- 0
				
				order <- Stop("AMP", buy = 100, at = Op(price.bar))
				order <- setID(order, "o1")
				broker <- Broker()
				addOrder(broker, order)
				notify(order, broker, price.bar)
				order <- openOrders(broker, "AMP")[[1]]
				
				expect_that(status(order), equals("open"))
			})
	
	test_that("Sell Stop loss executes when limit breached during day", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "closeOrder")
				price.bar <- loadStocks("AMP.AX")[[1]][2]
				stop.order <- Stop("AMP.AX", sell = 100, at = Op(price.bar) - 0.01)
				expected.order <- stop.order
				expected.order@status <- ClosedStatus()
				expected.order@execution.price <- limit_price(stop.order)
				submissionTime(expected.order) <- initDate()
				statusTime(expected.order) <- index(price.bar)

				notify(stop.order, broker, price.bar)
				
				expect_that(broker, called_once_with("closeOrder", expected.order))
			})
	
	test_that("Sell Stop loss executes when limit breached at open", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "closeOrder")
				price.bar <- loadStocks("AMP.AX")[[1]][2]
				stop.order <- Stop("AMP.AX", sell = 100, at = Op(price.bar) + 0.01)
				expected.order <- stop.order
				expected.order@status <- ClosedStatus()
				expected.order@execution.price <- Op(price.bar)
				submissionTime(expected.order) <- initDate()
				statusTime(expected.order) <- index(price.bar)
				
				notify(stop.order, broker, price.bar)
				
				expect_that(broker, called_once_with("closeOrder", expected.order))
			})
	
	test_that("Buy Stop loss executes when limit breached during day", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "closeOrder")
				price.bar <- loadStocks("AMP.AX")[[1]][2]
				stop.order <- Stop("AMP.AX", buy = 100, at = Op(price.bar) + 0.01)
				
				expected.order <- stop.order
				expected.order@status <- ClosedStatus()
				expected.order@execution.price <- limit_price(stop.order)
				submissionTime(expected.order) <- initDate()
				statusTime(expected.order) <- index(price.bar)
				
				notify(stop.order, broker, price.bar)
				
				expect_that(broker, called_once_with("closeOrder", expected.order))
			})
	
	test_that("Buy Stop loss executes when limit breached at open", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "closeOrder")
				price.bar <- loadStocks("AMP.AX")[[1]][2]
				stop.order <- Stop("AMP.AX", buy = 100, at = Op(price.bar) - 0.01)
				
				expected.order <- stop.order
				expected.order@status <- ClosedStatus()
				expected.order@execution.price <- Op(price.bar)
				submissionTime(expected.order) <- initDate()
				statusTime(expected.order) <- index(price.bar)
				
				notify(stop.order, broker, price.bar)
				
				expect_that(broker, called_once_with("closeOrder", expected.order))
			})
	
	test_that("Sell Stop Loss executes on day after submitted", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "closeOrder")
				price.bar <- loadStocks("AMP.AX")[[1]][2]
				
				at.price <- xts(as.numeric(Op(price.bar) + 0.01), index(price.bar) - 1)
				stop.order <- Stop("AMP.AX", sell = 100, at = at.price)
				expected.order <- stop.order
				expected.order@status <- ClosedStatus()
				expected.order@execution.price <- Op(price.bar)
				submissionTime(expected.order) <- initDate()
				statusTime(expected.order) <- index(price.bar)
				
				notify(stop.order, broker, price.bar)
				
				expect_that(broker, called_once_with("closeOrder", expected.order))
			})

	
context("Limit order processing")

	test_that("Creating Limit orders", {
				
				limit.order <- Limit("AMP", buy = 100, at = xts())
				expect_that(is(limit.order, "Order"), is_true())
				expect_that(instrumentOf(limit.order), equals("AMP"))
			})
	
	test_that("Limit order takes no action when market inactive", {
				
				price.bar <- loadStocks("AMP.AX")[[1]][2]
				price.bar[, "AMP.AX.Open"] <- 10.0
				price.bar[, "AMP.AX.Low"] <- 9.90
				price.bar[, "AMP.AX.Volume"] <- 0
				
				order <- Limit("AMP", buy = 100, at = Op(price.bar))
				order <- setID(order, "o1")
				broker <- Broker()
				addOrder(broker, order)
				notify(order, broker, price.bar)
				order <- openOrders(broker, "AMP")[[1]]
				
				expect_that(status(order), equals("open"))
			})
	
	test_that("Buy Limit executes when target price breached during day", {
				
				price.bar <- loadStocks("AMP.AX")[[1]][2]
				price.bar[, "AMP.AX.Open"] <- 10.0
				price.bar[, "AMP.AX.Low"] <- 9.90
				
				order <- Limit("AMP", buy = 100, at = Op(price.bar) - 0.05)
				
				expected.order <- order
				expected.order@status <- ClosedStatus()
				expected.order@execution.price <- limit_price(order)
				submissionTime(expected.order) <- initDate()
				statusTime(expected.order) <- index(price.bar)
				
				broker <- Mock("Broker")
				mockMethod(broker, "closeOrder")
				notify(order, broker, price.bar)
				
				expect_that(broker, called_once_with("closeOrder", expected.order))
			})
	
	test_that("Buy Limit executes when target price breached at open", {
				
				price.bar <- loadStocks("AMP.AX")[[1]][2]
				price.bar[, "AMP.AX.Open"] <- 10.0
				price.bar[, "AMP.AX.Low"] <- 9.90
				
				order <- Limit("AMP.AX", buy = 100, at = Op(price.bar) + 0.05)
				broker <- Broker()
				addOrder(broker, order)
				
				order <- setID(order, "o1")
				order <- setTxnCostModel(order, default_cost_model)
				
				notify(order, broker, price.bar)
				order <- closedOrders(broker, "AMP.AX")[[1]]
				
				expect_that(status(order), equals("closed"))
				expect_that(execution_price(order), equals(Op(price.bar)))
			})
	
	
	test_that("Sell Limit executes when target price breached during day", {
				
				price.bar <- loadStocks("AMP.AX")[[1]][2]
				price.bar[, "AMP.AX.Open"] <- 10.0
				price.bar[, "AMP.AX.High"] <- 10.10
				
				order <- Limit("AMP.AX", sell = 100, at = Op(price.bar) + 0.05)
				broker <- Broker()
				addOrder(broker, order)
				
				order <- setID(order, "o1")
				order <- setTxnCostModel(order, default_cost_model)
				
				notify(order, broker, price.bar)
				order <- closedOrders(broker, "AMP.AX")[[1]]
				
				expect_that(status(order), equals("closed"))
				expect_that(execution_price(order), equals(limit_price(order)))
			})
	
	test_that("Sell Limit executes when target price breached at open", {
				
				price.bar <- loadStocks("AMP.AX")[[1]][2]
				price.bar[, "AMP.AX.Open"] <- 10.0
				price.bar[, "AMP.AX.High"] <- 10.10
				
				order <- Limit("AMP.AX", sell = 100, at = Op(price.bar) - 0.05)
				broker <- Broker()
				addOrder(broker, order)
				
				order <- setID(order, "o1")
				order <- setTxnCostModel(order, default_cost_model)
				
				notify(order, broker, price.bar)
				order <- closedOrders(broker, "AMP.AX")[[1]]
				
				expect_that(status(order), matchesObject("closed"))
				expect_that(execution_price(order), equals(Op(price.bar)))
			})
	
	
context("Market with Stop processing")

				cleanMockMethods()

	test_that("Market with Stop sends stop when Market executed", {
				
				stop.loss <- 0.02
				order <- MarketWithStop("AMP", buy = 100, stop.point = stop.loss)
				order <- setID(order, "o1")
				price.bar <- loadStocks("AMP.AX")[[1]][2]
				
				broker <- Broker()
				setTodaysDate(broker, index(price.bar))
				addOrder(broker, order)
				
				expected.market <- Order("AMP", buy = 100)
				expected.market@execution.price <- Op(price.bar)
				expected.market <- setID(expected.market, "o1")
				expected.market@status <- ClosedStatus()
				statusTime(expected.market) <- as.POSIXct(index(price.bar))
				
				expected.stop <- Stop("AMP", sell = 100, 
						at = Op(price.bar) * (1 - stop.loss))
				expected.stop <- setID(expected.stop, "o2")
				expected.stop <- setTxnCostModel(expected.stop, broker@cost.model) 
				submissionTime(expected.stop) <- as.POSIXct(index(price.bar))
				
				
				notify(order, broker, price.bar)
				
				expect_that(closedOrders(broker, "AMP")[[1]], 
						matchesObject(expected.market))
				expect_that(openOrders(broker, "AMP")[[1]], matchesObject(expected.stop))
				expect_that(length(openOrders(broker, "AMP")), equals(1))
			})
	
	
	
	

