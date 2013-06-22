#'
#'

context("__ Orders __")

context("Order creation")

	test_that("Order requires an instrument identifier on creation", {
				
				instrument <- "AMP.AX"
				
				expect_that(Order(), throws_error("Require an instrument identifier"))
				expect_that(Order("AMP.AX"), is_a("MarketOrder"))
				expect_that(Order(instrument), is_a("MarketOrder"))
				expect_that(Order(1), throws_error("character"))
				
			})
	
context("Market order on open processing")
	
	test_that("Order takes no action if not related to instrument", {
				
				broker <- Mock()
				AMP <- loadStocks("AMP.AX")[[1]]
				bhp.order <- Order("BHP.AX")
				price.bar <- AMP[2]
				
				order <- notify(bhp.order, broker, price.bar)
				
				expect_that(status(order), equals("open"))
			})
	
	
	test_that("MarketOrder executes on Open", {
				
				AMP <- loadStocks("AMP.AX")[[1]]
				amp.order <- Order("AMP.AX")
				price.bar <- AMP[2]
				
				amp.order <- notify(amp.order, broker, price.bar)
				
				expect_that(status(amp.order), equals("closed"))
				expect_that(execution_price(amp.order), equals(Op(price.bar)))
				
			})
	
	test_that("MarketOrder takes no action if no market activity", {
				
				AMP <- loadStocks("AMP.AX")[[1]]
				price.bar <- AMP[1]
				missing.volume <- missing.open <- zero.volume <- zero.open <- price.bar
				missing.volume[, "AMP.AX.Volume"] <- NA
				missing.open[, "AMP.AX.Open"] <- NA
				zero.volume[, "AMP.AX.Volume"] <- 0
				zero.open[, "AMP.AX.Open"] <- 0
				
				order <- Order("AMP.AX")
				
				expect_that(active_market(missing.volume), is_false())
				expect_that(active_market(missing.open), is_false())
				expect_that(active_market(zero.volume), is_false())
				expect_that(active_market(zero.open), is_false())
				
				expect_that(status(notify(order, broker, missing.volume)), equals("open"))
				expect_that(status(notify(order, broker, missing.open)), equals("open"))
				expect_that(status(notify(order, broker, zero.volume)), equals("open"))
				expect_that(status(notify(order, broker, zero.open)), equals("open"))
				
			})

context("Stop loss order processing")

	test_that("Creating stop loss orders", {
				
				stop.loss <- Stop("AMP.AX", 0.01)
				expect_that(Stop("AMP.AX"), throws_error("Require limit for StopLoss"))
				expect_that(stop.loss@parent, is_a("MarketOrder"))
			})

	test_that("Stop loss determines stop price", {
				
				stop.order <- Stop("AMP.AX", limit = 0.01)
				expect_that(limit_price(stop.order, 10), equals(0.99 * 10))
			})
	
	test_that("Stop loss fires limit order when parent executed", {
				
				broker <- Mock()
				mockMethod(broker, "addOrder")
				
				stop.order <- Stop("AMP.AX", limit = 0.01)
				AMP <- loadStocks("AMP.AX")[[1]]
				expected.parent <- Order("AMP.AX")
				expected.parent@status <- "closed"
				expected.parent@execution.price <- Op(AMP[2])
				
				order <- notify(stop.order, broker, AMP[2])
				limit.price <- Limit("AMP.AX", Op(AMP[2]) * 0.99)
				
				expect_that(broker, called_once_with("addOrder", limit.price))
				expect_that(order, matchesObject(expected.parent))
			})
	
	
	
	
	
	
	

