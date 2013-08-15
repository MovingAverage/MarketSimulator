#'
#'

load_all("D:/Code/R/Development/Packages/mockR")

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
				
				transaction <- data.frame(size = -50, price = 0.8, costs = 6, 
						row.names = "AMP")
				account <- updateAccount(account, transaction)
				
				expected.cash <- starting.cash + 40 - 6
				
				expect_that(cashIn(account), matchesObject(expected.cash))
			})
	
	test_that("Addition of new holding", {
				
				account <- Account(10000)
				
				transaction <- data.frame(size = 100, price = 1, costs = 6, 
						row.names = "AMP")
				account <- updateAccount(account, transaction)
				
				expect_that(cashIn(account), matchesObject(10000 - 106))
			})
	
	test_that("Account updated with multiple transactions", {
				
				starting.equity <- 10000
				starting.value <- 200
				starting.cash <- starting.equity - starting.value
				account <- Account(starting.cash)
				active_instruments <- c("AMP", "BHP")
				
				transactions <- data.frame(
						size = c(100, -100), price = c(1, 1.1), costs = c(10, 10), 
						row.names = active_instruments)
				
				expected.cash <- starting.cash - 100 - 10 + 110 - 10
				
				account <- updateAccount(account, transactions)
				
				expect_that(cashIn(account), matchesObject(expected.cash))
			})
	
	test_that("Account updated with transactions with some new", {
				
				starting.equity <- 10000
				starting.value <- 200
				starting.cash <- starting.equity - starting.value
				account <- Account(starting.cash)
				active_instruments <- c("AMP", "BHP", "CBA")
				
				transactions <- data.frame(
						size = c(100, 100), price = c(1, 1.5), costs = c(10, 10), 
						row.names = active_instruments[c(1, 3)])
				
				expected.cash <- starting.cash - 100 - 10 - 150 - 10
				
				account <- updateAccount(account, transactions)
				
				expect_that(cashIn(account), matchesObject(expected.cash))
			})
	
	test_that("Account updated with transaction of different instrument", {
				
				starting.equity <- 10000
				starting.cash <- starting.equity
				account <- Account(starting.cash)
				active_instruments <- c("AMP", "BHP")
				
				transactions <- data.frame(
						size = 100, price = 1.5, costs = 10, 
						row.names = active_instruments[1])
				
				expected.cash <- starting.cash - 150 - 10
				expected.equity <- starting.equity - 10
				
				account <- updateAccount(account, transactions)
				
				expect_that(cashIn(account), matchesObject(expected.cash))
			})
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	