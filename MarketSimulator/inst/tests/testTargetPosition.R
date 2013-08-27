#'
#'

context("__ Position Target __")

context("Basic information")

	test_that("Target position reports on instrument and size", {
				
				position <- Target("AMP", size = 0.5)
				
				expect_that(sizeOf(position), matchesObject(c(AMP = 0.5)))
				expect_that(instrumentOf(position), matchesObject("AMP"))
			})
	
	test_that("Target only accepts non-missing numeric values", {
				
				expect_that(Target("AMP", size = NA), throws_error())
				expect_that(Target("AMP", size = "1"), throws_error())
			})
	
	
context("Creating orders")

	test_that("Target with no stop point makes Market order", {
				
				target <- Target("AMP", size = 0.5)
				
				expect_that(makeOrder(target, 100), is_a("MarketOrder"))
			})
	
	test_that("Target with stop point makes MarketWithStop", {
				
				target <- Target("AMP", size = 0.5, stop.point = 0.01)
				quantity(target) <- 100
				
				expect_that(makeOrder(target, 100), is_a("LongMarketWithStop"))
			})
	
	test_that("Target makes Market Order for adjustment size", {
				
				target <- Target("AMP", size = 0.5, stop.point = 0.01)
				quantity(target) <- 500
				
				expect_that(makeOrder(target, 100), is_a("MarketOrder"))
			})
	
	
context("Setting target quantity")

	test_that("Target quantity set by Manager", {
				
				manager <- Manager(Mock("Strategy"))
				manager <- setupAccount(manager, 10000)
				latestPrices(manager) <- c("AMP" = 5)
				
				target <- Target("AMP", size = 0.5)
				
				target <- setTargetQuantity(manager, target)
				
				expect_that(quantity(target), matchesObject(1000))
			})






	