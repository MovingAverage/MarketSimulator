#'
#'
setClass("Manager",
		representation(
			strategy = "ANY"
		))
		
Manager <- function(strategy) {
	manager <- new("Manager")
	manager@strategy <- strategy
	return(manager)
}

setMethod("targetPositions",
		signature("Manager"),
		function(object, timestamp) {
			targetPositions(object@strategy, timestamp)
		})

balancePositions <- function(current.positions, ideal.positions) {
	
	percentage.commission <- 0.0008
	minimum.commission <- 6
	cost.threshold <- 0.002
	capital <- 8000
	
	current.positions <- pad_with_zeros(current.positions, ideal.positions)
	ideal.positions <- pad_with_zeros(ideal.positions, current.positions)
	
	target.changes <- ideal.positions - current.positions
	target.changes[is.na(target.changes)] <- 0
	size.of.changes <- abs(target.changes) * capital
	cost <- pmax(minimum.commission, size.of.changes * percentage.commission)
	relative.cost <- cost / size.of.changes
	effective <- relative.cost < cost.threshold
	target.changes[!effective] <- 0
	return(target.changes)
}

pad_with_zeros <- function(current, target) {
	
	current.names <- names(current)
	target.names <- names(target)
	
	missing.names <- !target.names %in% current.names
	current <- c(current, target[missing.names])
	current[missing.names] <- 0
	current <- current[sort(names(current))]
	return(current)
}

positionSizes <- function(broker, target.fraction) {
	current.fraction <- currentPositionFractions(broker)
	changes <- balancePositions(current.fraction, target.fraction)
	position.changes <- currentEquity(broker) * changes / latestPrices(broker)
	sell.all <- target.fraction == 0 & changes < 0
	position.changes[sell.all] <- -1 * currentPositions(broker)[sell.all]
	return(position.changes[position.changes != 0])
}

placeOrders <- function(manager, broker, timestamp) {
	
	target.fraction <- targetPositions(manager, timestamp)
	if (all(is.na(target.fraction))) {
		return(NULL)
	}
	position.size <- positionSizes(broker, target.fraction)
	records <- NULL
	for (instrument in names(position.size)) {
		position <- as.integer(position.size[instrument])
		if (!is.finite(position)) {
			records <- nonFiniteNotice(records, instrument)
			next
		}
		if (position > 0) {
			records <- buyNotice(records, position, instrument)
			order <- Order(instrument, buy = position)
		} else {
			if (isTRUE(as.numeric(target.fraction[instrument]) == 0)) {
				records <- sellAllNotice(records, position, instrument)
				order <- Order(instrument, sell = position)
			} else {
				records <- sellNotice(records, position, instrument)
				order <- Order(instrument, sell = position)
			}
		}
		addOrder(broker, order)
	}
	if (is.null(records)) {
		return(NULL)
	} else {
		return(records)
	}
}

nonFiniteNotice <- function(records, instrument) {
	appendNotice(records, paste(instrument, "non-finite position"))
}

buyNotice <- function(records, position, instrument) {
	appendNotice(records, paste("buy", position, instrument))
}

sellAllNotice <- function(records, sell.size, instrument) {
	appendNotice(records, paste0("sell all (", sell.size, ") ", instrument))
}

sellNotice <- function(records, position, instrument) {
	appendNotice(records, paste("sell", position, instrument))
}

appendNotice <- function(records, notice) {
	return(paste0(records, ifelse(length(records), ", ", ""), notice))
}

BackTest <- function(manager, broker, start.date, end.date) {
	
	setupAccount(broker, starting.equity = 10000)
	for (timestamp in as.character(seq.Date(start.date, end.date, by = 1))) {
		marketActivity(broker, timestamp)
		updateAccounts(broker)
		records <- placeOrders(manager, broker, timestamp)
		if (!is.null(records)) {
			message(paste0(timestamp, ": ", records))
		}
	}
}

setupBackTest <- function(instruments, name) {
	
	require("blotter")
	currency("AUD")
	for (instrument in instruments) {
		stock(instrument, currency = "AUD")
	}
	initPortf(name, instruments, initDate = initDate(), currency = "AUD", )
	initAcct(name, portfolios = name, initDate = initDate(), 
			initEq = 10000, currency = "AUD")
	
	market <- Market(loadStocks(instruments))
	broker <- Broker()
	broker <- addMarket(broker, market)
	
	strategy <- new("Strategy")
	strategy@instruments <- instruments
	strategy@indicator <- EMA
	strategy <- strategySetup(strategy, market)
	manager <- Manager(strategy)

	start.date <- as.Date(first(index(strategy@positions)))
	end.date <- as.Date(last(index(strategy@positions)))
	
	assign("market", market, globalenv())
	assign("broker", broker, globalenv())
	assign("manager", manager, globalenv())
	assign("start.date", start.date, globalenv())
	assign("end.date", end.date, globalenv())
}


restartBacktest <- function(name) {
	
	instruments <- names(getPortfolio(name)$symbols)
	try(rm(list = paste(c("account", "portfolio"), name, sep = "."), pos = .blotter), 
			silent = TRUE)
	initPortf(name, instruments, initDate = initDate(), currency = "AUD", )
	initAcct(name, portfolios = name, initDate = initDate(), 
			initEq = 10000, currency = "AUD")
	broker <- Broker()
	broker <- addMarket(broker, market)
	assign("broker", broker, globalenv())
}


updateBlotter <- function(name) {
	
	for (symbol in tradeableInstruments(market)) {
		assign(symbol, market@instruments[[symbol]], globalenv())
	}
	
	portfolioTxns(broker, name)
	updatePortf(name)
	updateAcct(name)
}












