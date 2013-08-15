#'
#'

setClass("Account",
		representation(
			cash = "numeric"
		))
		
Account <- function(starting.equity) {
	
	account <- new("Account")
	account@cash <- starting.equity
	return(account)
}

cashIn <- function(account) {
	return(account@cash)
}

setGeneric("updateAccount",
		function(object, transactions, ...) {
			standardGeneric("updateAccount")
		})

setMethod("updateAccount",
		signature("Account"),
		function(object, transactions) {
			if (length(transactions)) {
				object@cash <- updateCash(object, transactions)
			}
			return(object)
		})

updateCash <- function(account, transactions) {
	transaction.value <- sum(transactions$size * transactions$price)
	transaction.costs <- sum(transactions$costs)
	return(cashIn(account) - transaction.value - transaction.costs)
}



