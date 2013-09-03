#'
#'

context("__ Account __")

context("Account creation")

	test_that("Account starts with appropriate values", {
				
				starting.equity <- 10000
				account <- Account(starting.equity)
				
				expect_that(cashIn(account), matchesObject(starting.equity))
			})
	
	
context("Updating Account from transactions")

	test_that("Updates to existing holding", {
				
				starting.equity <- 10000 - 6
				starting.value <- 100
				starting.cash <- starting.equity - starting.value
				account <- Account(starting.cash)
				
				order <- Order("AMP", sell = 50)
				order@execution.price <- xts(0.8, order.by = as.Date("2010-04-20"))
				order@txn.cost.model <- function(order) 6
				account <- updateAccount(account, list(order))
				
				expected.cash <- starting.cash + 40 - 6
				
				expect_that(cashIn(account), matchesObject(expected.cash))
			})
	
	test_that("Addition of new holding", {
				
				account <- Account(10000)
				
				order <- Order("AMP", buy = 100)
				order@execution.price <- xts(1, order.by = as.Date("2010-04-20"))
				order@txn.cost.model <- function(order) 6
				account <- updateAccount(account, list(order))
				
				expect_that(cashIn(account), matchesObject(10000 - 106))
			})
	
	test_that("Account updated with multiple transactions", {
				
				starting.equity <- 10000
				starting.value <- 200
				starting.cash <- starting.equity - starting.value
				account <- Account(starting.cash)
				active_instruments <- c("AMP", "BHP")
				
				order1 <- Order("AMP", buy = 100)
				order1@execution.price <- xts(1, order.by = as.Date("2010-04-20"))
				order1@txn.cost.model <- function(order) 10
				
				order2 <- Order("BHP", sell = 100)
				order2@execution.price <- xts(1.1, order.by = as.Date("2010-04-20"))
				order2@txn.cost.model <- function(order) 10
				
				expected.cash <- starting.cash - 100 - 10 + 110 - 10
				
				account <- updateAccount(account, list(order1, order2))
				
				expect_that(cashIn(account), matchesObject(expected.cash))
			})
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	