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
				expect_that(quantity(Order("A", sell = 10)), equals(-10L))
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
	
context("Market order on open processing")
	
	test_that("Order takes no action if not related to instrument", {
				
				broker <- Broker()
				AMP <- loadStocks("AMP.AX")[[1]]
				price.bar <- AMP[2]
				
				bhp.order <- Order("BHP.AX", 100)
				addOrder(broker, bhp.order)
				
				notify(bhp.order, broker, price.bar)
				order <- getOrders(broker, "BHP.AX")[[1]]
				
				expect_that(status(order), equals("open"))
			})
	
	test_that("MarketOrder executes on Open", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "updateOrder")
				AMP <- loadStocks("AMP.AX")[[1]]
				price.bar <- AMP[2]
				
				order <- Order("AMP.AX", 100)
				expected.order <- order
				expected.order@status <- "closed"
				expected.order@execution.price <- Op(price.bar)
				
				notify(order, broker, price.bar)
				
				expect_that(broker, called_once_with("updateOrder", expected.order))
			})
	
	test_that("MarketOrder takes no action if no market activity", {
				
				AMP <- loadStocks("AMP.AX")[[1]]
				price.bar <- AMP[1]
				missing.volume <- missing.open <- zero.volume <- zero.open <- price.bar
				missing.volume[, "AMP.AX.Volume"] <- NA
				missing.open[, "AMP.AX.Open"] <- NA
				zero.volume[, "AMP.AX.Volume"] <- 0
				zero.open[, "AMP.AX.Open"] <- 0
				
				broker <- Broker()
				order <- Order("AMP.AX", 100)
				addOrder(broker, order)
				
				notify(order, broker, missing.volume)
				notify(order, broker, missing.open)
				notify(order, broker, zero.volume)
				notify(order, broker, zero.open)
				
				order <- getOrders(broker, "AMP.AX")[[1]]
				
				expect_that(active_market(missing.volume), is_false())
				expect_that(active_market(missing.open), is_false())
				expect_that(active_market(zero.volume), is_false())
				expect_that(active_market(zero.open), is_false())
				expect_that(status(order), equals("open"))
			})

context("Stop loss order processing") 

	test_that("Creating stop loss orders", {
				
				stop.loss <- Stop("AMP.AX", sell = 100, at = xts())
				expect_that(Stop("AMP.AX", sell = 100), throws_error())
				expect_that(is(stop.loss, "Order"), is_true())
			})

	test_that("Stop loss executes sell when limit breached", {
				
				broker <- Mock("Broker")
				mockMethod(broker, "updateOrder")
				AMP <- loadStocks("AMP.AX")[[1]]
				stop.order <- Stop("AMP.AX", sell = 100, at = Op(AMP[2]) - 0.01)
				expected.order <- stop.order
				expected.order@status <- "closed"
				expected.order@execution.price <- limit_price(stop.order)

				order <- notify(stop.order, broker, AMP[2])
				
				expect_that(broker, called_once_with("updateOrder", expected.order))
			})
	
	
	
	
	
	
	

