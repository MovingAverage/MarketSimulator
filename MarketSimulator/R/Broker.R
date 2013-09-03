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
							latest.prices = c(), 
							todays.date = initDate(), 
							next.id = 1)))
}

default_cost_model <- function(order) {
	return(0)
}

InteractiveBrokers_cost_model <- function(order) {
	value <- quantity(order) * execution_price(order)
	return(max(value * 0.0008, 6))
}

addTxnCostModel <- function(broker, model) {
	broker@cost.model <- model
	return(broker)
}

addMarket <- function(broker, market) {
			broker@market <- market
			prices <- zero_named_vector(tradeableInstruments(market))
			setLatestPrices(broker, prices)
			return(broker)
		}

setMethod("getBar",
		signature("Broker"),
		function(object, instrument, timestamp) {
			bar <- getBar(object@market, instrument, timestamp)
			setTodaysDate(object, index(bar))
			return(bar)
		})

today <- function(broker) {
	get("todays.date", broker@transactions)
}

setTodaysDate <- function(broker, timestamp) {
	assign("todays.date", as.POSIXct(timestamp), broker@transactions)
}

addOrder <- function(broker, order) {
	if (!inherits(order, "Order")) {
		stop("Only orders derived from class 'Order' may be added to Broker")
	}
	order <- setOrderID(order, broker)
	order <- setTxnCostModel(order, broker@cost.model)
	submissionTime(order) <- today(broker)
	if (hasSimilarOrder(broker, order)) {
		existing.order <- getSimilarOrder(broker, order)
		order <- mergeOrders(existing.order, order)
		updateOrder(order, broker)
	} else {
		addOrderToBook(broker, order, order.book = "open.orders")
	}
}

addOrderToBook <- function(broker, order, order.book) {
	order.list <- getOrders(broker, instrumentOf(order), order.book)
	order.list[getID(order)] <- order
	assign(instrumentOf(order), order.list, slot(broker, order.book))
}

setOrderID <- function(order, broker) {
	order <- setID(order, nextID(broker))
	return(order)
}

nextID <- function(broker) {
	ID <- get("next.id", broker@transactions)
	assign("next.id", ID + 1, broker@transactions)
	return(paste0("o", ID))
}

closeOrder <- function(broker, order) {
	removeFromOpenOrders(broker, order)
	addOrderToBook(broker, order, order.book = "closed.orders")
	addTransactionRecord(broker, order)
}

cancelOrder <- function(broker, order) {
	removeFromOpenOrders(broker, order)
}

replaceOrder <- function(broker, order) {
	order.list <- openOrders(broker, instrumentOf(order))
	statusTime(order) <- today(broker)
	order.list[[getID(order)]] <- NULL
	order.list[[getID(order)]] <- order
	# Note this is so that order is replaced at end of open orders.
	assign(instrumentOf(order), order.list, broker@open.orders)
}

removeFromOpenOrders <- function(broker, order) {
	order.list <- openOrders(broker, instrumentOf(order))
	order.list[[getID(order)]] <- NULL
	assign(instrumentOf(order), order.list, broker@open.orders)
}

addTransactionRecord <- function(broker, order) {
	transactions <- c(getTransactions(broker), order)
	setTransactions(broker, transactions)
}

setTransactions <- function(broker, transactions) {
	assign("transactions", transactions, broker@transactions)
}

#' Return transaction records
getTransactions <- function(broker) {
	get("transactions", broker@transactions)
}

#' Clear transaction records
clearTransactions <- function(broker) {
	assign("transactions", list(), broker@transactions)	
}

hasSimilarOrder <- function(broker, order) {
	similar.order <- getSimilarOrder(broker, order)
	return(is(similar.order, "Order"))
}

getSimilarOrder <- function(broker, order) {
	open.orders <- openOrders(broker, instrumentOf(order))
	similar.orders <- open.orders[vapply(open.orders, areSimilar, logical(1), order)]
	if (length(similar.orders) > 1) stop("More than one similar order found")
	if (length(similar.orders) == 1) {
		return(similar.orders[[1]])
	} else {
		return(NULL)
	}
}

openOrders <- function(broker, instrument) {
	getOrders(broker, instrument, "open.orders")
}

closedOrders <- function(broker, instrument) {
	getOrders(broker, instrument, "closed.orders")
}

getOrders <- function(broker, instrument, type) {
	if (missing(instrument)) {
		stop("Must supply instrument name of orders")
	}
	possible.types <- c("open.orders", "closed.orders")
	if (!type %in% possible.types) {
		stop(paste("order.type must be one of:", 
						paste(possible.types, collapse = ", ")))
	}
	order.list <- tryCatch(
			get(instrument, slot(broker, type), inherits = FALSE), 
			error = function(e) list())
	return(order.list)
}

setMethod("activeInstruments",
		signature("Broker"),
		function(object) {
			return(ls(object@open.orders))
		})

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
	
	unchecked.orders <- openOrders(broker, instrument)
	
	while (length(unchecked.orders) > 0) {
		for (order in unchecked.orders) {
			notify(order, broker, price.bar)
		}
		open.orders <- openOrders(broker, instrument)
		unchecked.orders <- open.orders[!open.orders %in% unchecked.orders] 
	}
}

recordLatestPrice <- function(broker, price) {
	if (length(price) != 0) {
		price <- remove_price_OHLC_flag(price)
		latest.prices <- latestPrices(broker)
		latest.prices[names(price)] <- price
		setLatestPrices(broker, latest.prices)
	}
}

remove_price_OHLC_flag <- function(price) {
	splits <- strsplit(names(price), "\\.")[[1]]
	instrument <- paste(splits[-length(splits)], collapse = ".")
	price <- as.numeric(price)
	names(price) <- instrument
	return(price)
}

setMethod("latestPrices",
		signature(),
		function(object) {
			get("latest.prices", object@transactions)
		})

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
					TxnFees = -txnFees(order), 
					verbose = verbose)
		}
	}
}

setMethod("show",
		signature(object = "Broker"),
		function(object) {
			print("Market:")
			print(object@market)
			print("Open Orders:")
			open.orders <- data.frame()
			closed.orders <- numeric()
			for (instrument in activeInstruments(object)) {
				for (order in openOrders(object, instrument)) {
					open.orders <- rbind(open.orders, asDataFrame(order))
				}
			}
			for (instrument in tradedInstruments(broker)) {
				num.closed <- length(closedOrders(broker, instrument))
				names(num.closed) <- instrument
				closed.orders <- c(closed.orders, num.closed)
			}
			print(open.orders)
			print("Number of closed orders:")
			print(closed.orders)
			print("Last Transactions:")
			print(getTransactions(broker))
		})














