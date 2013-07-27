#'
#'
setClass("Broker",
		representation(
			market = "Market", 
			open.orders = "environment", 
			closed.orders = "environment", 
			cost.model = "function", 
			transactions = "environment"
		))

Broker <- function() {
	new("Broker", 
			market = new("Market"), 
			open.orders = new.env(emptyenv()), 
			closed.orders = new.env(emptyenv()), 
			cost.model = default_cost_model, 
			transactions = as.environment(list(
							transactions = data.frame(),
							latest.prices = c())))
}

default_cost_model <- function(order) {
	return(0)
}

addTxnCostModel <- function(broker, model) {
	broker@cost.model <- model
	return(broker)
}

addMarket <- function(broker, market) {
			broker@market <- market
			return(broker)
		}

setMethod("getBar",
		signature("Broker"),
		function(object, instrument, timestamp) {
			getBar(object@market, instrument, timestamp)
		})

addOrder <- function(broker, order) {
	if (!inherits(order, "Order")) {
		stop("Only orders derived from class 'Order' may be added to Broker")
	}
	order <- setTxnCostModel(order, broker@cost.model)
	addOrderToBook(broker, order, order.book = "open.orders")
}

addOrderToBook <- function(broker, order, order.book) {
	order.list <- getOrders(broker, instrumentOf(order), order.book)
	order <- setID(order, as.integer(length(order.list) + 1))
	order.list <- c(order.list, list(order))
	assign(instrumentOf(order), order.list, slot(broker, order.book))
}

updateOrder <- function(broker, order) {
	if (status(order) == "closed") {
		removeFromOpenOrders(broker, order)
		addOrderToBook(broker, order, order.book = "closed.orders")
		addTransactionRecord(broker, order)
	} else {
		updateOpenOrdersBook(broker, order)
	}
}

addTransactionRecord <- function(broker, order) {
	transaction <- writeTransaction(order)
	transactions <- get("transactions", envir = broker@transactions)
	transactions <- rbind(transactions, transaction)
	setTransactions(broker, transactions)
}

setTransactions <- function(broker, transactions) {
	assign("transactions", transactions, broker@transactions)
}

updateOpenOrdersBook <- function(broker, order) {
	order.list <- openOrders(broker, instrumentOf(order))
	order.list[[getID(order)]] <- order
	assign(instrumentOf(order), order.list, broker@open.orders)
}

removeFromOpenOrders <- function(broker, order) {
	order.list <- openOrders(broker, instrumentOf(order))
	order.list[[getID(order)]] <- NULL
	assign(instrumentOf(order), order.list, broker@open.orders)
}

openOrders <- function(broker, instrument) {
	getOrders(broker, instrument, "open.orders")
}

closedOrders <- function(broker, instrument) {
	getOrders(broker, instrument, "closed.orders")
}

getOrders <- function(broker, instrument, type) {
	possible.types <- c("open.orders", "closed.orders")
	if (!type %in% possible.types) {
		stop(paste("order.type must be one of:", 
						paste(possible.types, collapse = ", ")))
	}
	order.list <- tryCatch(
			get(instrument, slot(broker, type)), 
			error = function(e) list())
	return(order.list)
}

activeInstruments <- function(broker) {
	return(ls(broker@open.orders))
}

tradedInstruments <- function(broker) {
	return(ls(broker@closed.orders))
}

activeAndTradedInstruments <- function(broker) {
	return(sort(unique(c(activeInstruments(broker), tradedInstruments(broker)))))
}

setMethod("tradeableInstruments",
		signature("Broker"),
		function(object) {
			return(tradeableInstruments(object@market))
		})

#' Notify all orders of market actions
marketActivity <- function(broker, timestamp) {
	
	for (instrument in tradeableInstruments(broker)) {
		price.bar <- getBar(broker, instrument, timestamp)
		recordLatestPrice(broker, Cl(price.bar))
		if (instrument %in% activeInstruments(broker)) {
			notifyOrders(broker, instrument, price.bar)
		}
	}
}

#' Notify each order of prices for day.
notifyOrders <- function(broker, instrument, price.bar) {
	for (order in openOrders(broker, instrument)) {
		notify(order, broker, price.bar)
	}
}

# TODO set up latest prices when adding market and clean up below function
recordLatestPrice <- function(broker, price) {
	if (length(price) == 0) {
		return()
	} else {
		price.names <- names(price)
		splits <- strsplit(price.names, "\\.")[[1]]
		instrument <- paste(splits[-length(splits)], collapse = ".")
		price <- as.numeric(price)
		names(price) <- instrument
	}
	if (length(latestPrices(broker))) {
		latest.prices <- latestPrices(broker)
		if (names(price) %in% names(latest.prices)) {
			latest.prices[names(price)] <- price
		} else {
			latest.prices <- c(latest.prices, price)
		}
		setLatestPrices(broker, latest.prices)
	} else {
		setLatestPrices(broker, price)
	}
}

#' Return transaction records
transactions <- function(broker) {
	get("transactions", broker@transactions)
}

#' Clear transaction records
clearTransactions <- function(broker) {
	assign("transactions", data.frame(), broker@transactions)	
}

setupAccount <- function(broker, starting.equity) {
	account <- Account(starting.equity)
	setAccount(broker, account)
}

setAccount <- function(broker, account) {
	assign("account", account, broker@transactions)
}

accountAt <- function(broker) {
	get("account", broker@transactions)
}

setMethod("updateAccounts",
		signature("Broker"),
		function(object) {
			account <- updateAccounts(accountAt(object), transactions(object))
			setAccount(object, account)
			clearTransactions(object)
		})

currentEquity <- function(broker) {
	equity(accountAt(broker))
}

setMethod("currentPositions",
		signature("Broker"),
		function(object) {
			positions <- currentPositions(accountAt(object))
			order.sizes <- orderSizes(object)
			return(positions + order.sizes)
		})

orderSizes <- function(broker) {
	
	active.instruments <- activeInstruments(broker)
	order.sizes <- numeric(length(active.instruments))
	names(order.sizes) <- active.instruments
	for (instrument in active.instruments) {
		orders <- openOrders(broker, instrument)
		if (length(orders)) {
			order.sizes[instrument] <- sum(sapply(orders, quantity))
		}
	}
	return(order.sizes)
}

latestPrices <- function(broker) {
	get("latest.prices", broker@transactions)
}

setLatestPrices <- function(broker, prices) {
	assign("latest.prices", prices, broker@transactions)
}

#' Print closed orders as quantstrat order_book object
printOrderBook <- function(broker, portfolio) {
	
	print_orders <- function(order.list) {

		timestamp <- as.POSIXct(initDate())
		order.book <- xts(t(c(
								"Order.Qty" = 0, 
								"Order.Price" = NA, 
								"Order.Type" = "init", 
								"Order.Side" = "long",
								"Order.Threshold" = 0, 
								"Order.Status" = "closed", 
								"Order.StatusTime" = as.character(timestamp), 
								"Prefer" = "", 
								"Order.Set" = "", 
								"Txn.Fees" = 0, 
								"Rule" = "")), 
				order.by = timestamp)
		
		for (order in order.list) {
			order.book <- rbind(order.book, bookEntry(order))
		}
		return(order.book)
	}
	
	
	order_book <- list()
	order_book[[portfolio]] <- lapply(as.list(broker@closed.orders), print_orders)
	class(order_book) <- "order_book"
	return(order_book)
}

#' Updates named portfolio with executed transactions
#' 
#' This function is used to integrate MarketSimulator with the blotter package.
#' The PortfolioTxns function adds transactions to the portfolio with blotter:addTxn, 
#' which then allows further analysis and reporting to be done with blotter functionality.
#' 
#' @param broker the Broker object which contains the closed orders
#' @param portfolio a character string naming the portfolio for recording transactions.
#' @param verbose logical indicating whether each transaction is to be printed to the 
#' console.
#' 
portfolioTxns <- function(broker, portfolio, verbose = TRUE) {
	
	for (instrument in tradedInstruments(broker)) {
		for (order in closedOrders(broker, instrument)) {
			
			addTxn(portfolio, instrument, 
					TxnDate = statusTime(order), 
					TxnQty = quantity(order), 
					TxnPrice = execution_price(order), 
					TxnFees = txnFees(order), 
					verbose = verbose)
			
		}
	}
	
	
}
















