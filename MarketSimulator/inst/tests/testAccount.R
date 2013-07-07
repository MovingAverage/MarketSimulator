#'
#'

load_all("D:/Code/R/Development/Packages/mockR")

context("__ Account __")

context("Account creation")

	test_that("Account starts with appropriate values", {
				
				starting.equity <- 10000
				starting.holdings <- data.frame(
						size = numeric(), value = numeric(), costs = numeric())
				account <- Account(starting.equity)
				
				expect_that(cashIn(account), matchesObject(starting.equity))
				expect_that(totalCosts(account), matchesObject(0))
				expect_that(equity(account), matchesObject(starting.equity))
				expect_that(holdings(account), matchesObject(starting.holdings))
			})
	
context("Account queries")

	test_that("Account reports on held positions", {
				
				account <- Account(10000)
				activeInstruments <- c("AMP", "BHP")
				positions <- c(100, 50)
				account@holdings <- data.frame(
						size = positions, value = c(100, 100), costs = c(6, 6), 
						row.names = activeInstruments)
				expected.positions <- positions
				names(expected.positions) <- activeInstruments
				
				expect_that(heldInstruments(account), matchesObject(activeInstruments))
				expect_that(currentPositions(account), matchesObject(expected.positions))
			})
	
context("Updating Account from transactions")

	test_that("Updates to existing holding", {
				
				starting.equity <- 10000 - 6
				starting.value <- 100
				starting.cash <- starting.equity - starting.value
				account <- Account(starting.cash)
				account@holdings <- data.frame(size = 100, value = 100, costs = 6, 
						row.names = "AMP")
				
				transaction <- data.frame(size = -50, price = 0.8, costs = 6, 
						row.names = "AMP")
				account <- updateAccounts(account, transaction)
				
				expected.holdings <- data.frame(size = 50, value = 40, costs = 12, 
						row.names = "AMP")
				expected.equity <- starting.equity + (80 - 100) - 6
				expected.cash <- starting.cash + 40 - 6
				
				expect_that(holdings(account), matchesObject(expected.holdings))
				expect_that(cashIn(account), matchesObject(expected.cash))
				expect_that(equity(account), matchesObject(expected.equity))
			})
	
	test_that("Addition of new holding", {
				
				account <- Account(10000)
				
				transaction <- data.frame(size = 100, price = 1, costs = 6, 
						row.names = "AMP")
				expected.holdings <- data.frame(size = 100, value = 100, costs = 6, 
						row.names = "AMP")
				account <- updateAccounts(account, transaction)
				
				expect_that(holdings(account), matchesObject(expected.holdings))
				expect_that(cashIn(account), matchesObject(10000 - 106))
				expect_that(equity(account), matchesObject(10000 - 6))
			})
	
	test_that("Account updated with multiple transactions", {
				
				starting.equity <- 10000
				starting.value <- 200
				starting.cash <- starting.equity - starting.value
				account <- Account(starting.cash)
				activeInstruments <- c("AMP", "BHP")
				account@holdings <- data.frame(
						size = c(100, 50), value = c(100, 100), costs = c(0, 0), 
						row.names = activeInstruments)
				
				transactions <- data.frame(
						size = c(100, -100), price = c(1, 1.1), costs = c(10, 10), 
						row.names = activeInstruments)
				
				expected.holdings <- data.frame(
						size = c(200, -50), value = c(200, -55), costs = c(10, 10), 
						row.names = activeInstruments)
				expected.cash <- starting.cash - 100 - 10 + 110 - 10
				
				account <- updateAccounts(account, transactions)
				
				expect_that(holdings(account), matchesObject(expected.holdings))
				expect_that(cashIn(account), matchesObject(expected.cash))
			})
	
	test_that("Account updated with transactions with some new", {
				
				starting.equity <- 10000
				starting.value <- 200
				starting.cash <- starting.equity - starting.value
				account <- Account(starting.cash)
				activeInstruments <- c("AMP", "BHP", "CBA")
				account@holdings <- data.frame(
						size = c(100, 50), value = c(100, 100), costs = c(10, 0), 
						row.names = activeInstruments[1:2])
				
				transactions <- data.frame(
						size = c(100, 100), price = c(1, 1.5), costs = c(10, 10), 
						row.names = activeInstruments[c(1, 3)])
				
				expected.holdings <- data.frame(
						size = c(200, 50, 100), value = c(200, 100, 150), 
						costs = c(20, 0, 10), row.names = activeInstruments)
				expected.cash <- starting.cash - 100 - 10 - 150 - 10
				expected.equity <- starting.equity - 20
				
				account <- updateAccounts(account, transactions)
				
				expect_that(cashIn(account), matchesObject(expected.cash))
				expect_that(equity(account), matchesObject(expected.equity))
				expect_that(holdings(account), matchesObject(expected.holdings))
			})
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	