#'
#'
BackTest <- function(manager, broker, start.date, end.date) {
	
	for (timestamp in as.character(seq.Date(start.date, end.date, by = 1))) {
		# During trading day
		marketActivity(broker, timestamp)
		# At close of trading
		manager <- updatePositions(manager, broker)
		# Before next days trading
		manager <- placeOrders(manager, broker, timestamp)
		manager <- sendNotices(manager, timestamp)
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
	manager <- setupAccount(manager, starting.equity = 10000)
	
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
	removeBlotterObjects(name, instruments)
	initPortf(name, instruments, initDate = initDate(), currency = "AUD", )
	initAcct(name, portfolios = name, initDate = initDate(), 
			initEq = 10000, currency = "AUD")
	broker <- Broker()
	broker <- addMarket(broker, market)
	assign("broker", broker, globalenv())
}

removeBlotterObjects <- function(name, instruments) {
	try(rm(list = paste(c("account", "portfolio"), name, sep = "."), pos = .blotter), 
			silent = TRUE)
}

updateBlotter <- function(name) {
	
	for (symbol in tradeableInstruments(market)) {
		assign(symbol, market@instruments[[symbol]], globalenv())
	}
	
	portfolioTxns(broker, name)
	updatePortf(name)
	updateAcct(name)
}





















