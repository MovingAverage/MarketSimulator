#'
#'

setClass("Account",
		representation(
			cash = "numeric", 
			costs = "numeric", 
			holdings = "data.frame"
		))
		
Account <- function(starting.equity) {
	
	account <- new("Account")
	account@cash <- starting.equity
	account@holdings <- data.frame(
			size = numeric(), 
			value = numeric(), 
			costs = numeric())
	return(account)
}

cashIn <- function(account) {
	return(account@cash)
}

holdings <- function(account) {
	return(account@holdings)
}

totalCosts <- function(account) {
	return(sum(holdings(account)$costs))
}

equity <- function(account) {
	return(cashIn(account) + sum(holdings(account)$value))
}

heldInstruments <- function(account) {
	return(row.names(holdings(account)))
}

currentPositions <- function(account) {
	return(holdings(account)$size)
}

updateAccounts <- function(account, transactions) {
	
	holdings <- holdings(account)
	holdings <- pad_with_empty_rows(holdings, transactions)
	transactions <- pad_with_empty_rows(transactions, holdings)
	transactions <- calculate_missing_prices(transactions, holdings)
	
	account@holdings <- updateHoldings(holdings, transactions)
	account@cash <- updateCash(account, transactions)
	
	return(account)
}

updateHoldings <- function(holdings, transactions) {
	holdings$size <- holdings$size + transactions$size
	holdings$costs <- holdings$costs + transactions$costs
	holdings$value <- holdings$size * transactions$price
	return(holdings)
}

updateCash <- function(account, transactions) {
	transaction.value <- sum(transactions$size * transactions$price)
	transaction.costs <- sum(transactions$costs)
	return(cashIn(account) - transaction.value - transaction.costs)
}

pad_with_empty_rows <- function(frame, target) {
	
	target.rows <- row.names(target)
	rows <- target.rows[!target.rows %in% row.names(frame)]
	new.rows <- data.frame(array(0, dim = c(length(rows), 3)), row.names = rows)
	colnames(new.rows) <- colnames(frame)
	frame <- rbind(frame, new.rows)
	frame <- frame[sort(row.names(frame)), ]
	return(frame)
}

calculate_missing_prices <- function(transactions, holdings) {
	rows <- transactions$size == 0
	transactions[rows, "price"] <- holdings[rows, "value"] / holdings[rows, "size"]
	return(transactions)
}


