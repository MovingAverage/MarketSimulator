#'
#'

context("__ Trade based calculations __")

context("Calculating trade lengths")

test_that("Trade lengths calculated from xts object", {
			
			positions <- c(NA, 0, 0, 1, 1, 1, 0, 1, 0, 0)
			dates <- seq.Date(as.Date("2007-01-01"), by = 1, 
					length.out = length(positions))
			data <- xts(positions, order.by = dates)
			
			expect_that(trade_lengths(data), matchesObject(list(c(3, 1))))
		})

test_that("Trade starts returned", {
			
			positions <- c(NA, 0, 0, 1, 1, 1, 0, 1, 0, 0)
			dates <- seq.Date(as.Date("2007-01-01"), by = 1, 
					length.out = length(positions))
			data <- xts(positions, order.by = dates)
			
			expect_that(trade_start_indexes(data), matchesObject(list(c(4, 8))))
		})

test_that("Starts returned if open on first day", {
			
			positions <- c(1, 0, 0, 1, 1, 1, 0, 1, 0, 0)
			dates <- seq.Date(as.Date("2007-01-01"), by = 1, 
					length.out = length(positions))
			data <- xts(positions, order.by = dates)
			
			expect_that(trade_start_indexes(data), matchesObject(list(c(1, 4, 8))))
		})

test_that("Trade ends returned", {
			
			positions <- c(1, 0, 0, 1, 1, 1, 0, 1, 0, 0)
			dates <- seq.Date(as.Date("2007-01-01"), by = 1, 
					length.out = length(positions))
			data <- xts(positions, order.by = dates)
			
			expect_that(trade_end_indexes(data), matchesObject(c(1, 6, 8)))
		})

test_that("Median lengths calculated for multiple instruments", {
			
			data <- list(
					AMP = c(1, 0, 0, 1, 1, 1, 0, 1, 0, 0), 
					BHP = c(1, 0, 1, 1, 1, 1, 0, 1, 1, 0), 
					CBA = c(NA, 0, 0, 1, 0, 1, 0, 1, 1, 0))
			dates <- seq.Date(as.Date("2007-01-01"), by = 1, 
					length.out = length(data[[1]]))
			data <- xts(as.data.frame(data), order.by = dates)
			
			expected.medians <- c(1, 2, 1)
			names(expected.medians) <- names(data)
			
			expect_that(median_trade_length(data), matchesObject(expected.medians))
		})



























