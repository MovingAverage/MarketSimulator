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

test_that("Target supports generic interface", {
			
			target <- Target("AMP", size = 0.5)
			
			expect_that(instrumentOf(target), matchesObject("AMP"))
			expect_that(sizeOf(target), matchesObject(c(AMP = 0.5)))
			expect_that(quantity(target), matchesObject(numeric()))
		})

test_that("Target supports setting quantity", {
			
			target <- Target("AMP", size = 0.5)
			quantity(target) <- 100
			
			expect_that(quantity(target), matchesObject(100))
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







