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
	cash <- cashIn(account)
	for (txn in transactions) {
		transaction.value <- as.numeric(quantity(txn) * execution_price(txn))
		transaction.costs <- as.numeric(txnFees(txn))
		cash <- cash - transaction.value - transaction.costs
	}
	
	return(as.numeric(cash))
}



