#'
#'
BackTest <- function(manager, broker, start.date, end.date) {
	
	for (timestamp in as.character(seq.Date(start.date, end.date, by = 1))) {
		# During trading day
		marketActivity(broker, timestamp)
		# At close of trading
		manager <- updateRecords(manager, broker)
		# Before next days trading
		manager <- placeOrders(manager, broker)
		manager <- sendNotices(manager, timestamp)
	}
}

setupBackTest <- function(instruments, name, strategy, initEq = 10000) {
	
	require("blotter")
	currency("AUD")
	for (instrument in instruments) {
		stock(instrument, currency = "AUD")
	}
	initPortf(name, instruments, initDate = initDate(), currency = "AUD", )
	initAcct(name, portfolios = name, initDate = initDate(), 
			initEq = initEq, currency = "AUD")
	
	market <- Market(loadStocks(instruments))
	broker <- Broker()
	broker <- addMarket(broker, market)
	
	strategy <- primeStrategy(strategy, market)
	manager <- Manager(strategy)
	manager <- setupAccount(manager, starting.equity = initEq)
	
	start.date <- as.Date(first(index(strategy@positions)))
	end.date <- as.Date(last(index(strategy@positions)))
	
	assign("market", market, globalenv())
	assign("broker", broker, globalenv())
	assign("manager", manager, globalenv())
	assign("start.date", start.date, globalenv())
	assign("end.date", end.date, globalenv())
}


restartBacktest <- function(name) {
	
	initEq <- attr(getAccount(name), "initEq")
	instruments <- names(getPortfolio(name)$symbols)
	removeBlotterObjects(name, instruments)
	initPortf(name, instruments, initDate = initDate(), currency = "AUD", )
	initAcct(name, portfolios = name, initDate = initDate(), 
			initEq = initEq, currency = "AUD")
	broker <- Broker()
	broker <- addMarket(broker, market)
	assign("broker", broker, globalenv())
}

removeBlotterObjects <- function(name) {
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

overallPerformance <- function(name) {
	
	initEq <- attr(getAccount(name), "initEq")
	PL <- getPortfolio(name)$summary$Net.Trading.PL
	charts.PerformanceSummary(PL / initEq)
}

byPositionPerformance <- function(name) {
	
	charts.PeformanceSummary(portfReturns(name), colorset = bluefocus)
}

tradeStatsComparison <- function(names, instrument) {
	
	stats <- data.frame()
	for (name in names) {
		stats <- rbind(stats, tradeStats(name, instrument))
	}
	row.names(stats) <- names
	t(stats)
}

stopPlacement <- function(name, instrument) {
	
	stats <- perTradeStats(name, instrument)
	MAE <- stats$Pct.MAE
	trade.returns <- stats$Pct.Net.Trading.PL
	
	stop.points <- c(0.5, 1, 2, 3, 4, 5, 7, 10, 15, 20)
	
	for (stop.point in stop.points) {
		stop.value <- -(stop.point / 100)
		stop.returns <- stats$Pct.Net.Trading.PL
		stop.returns[MAE <= stop.value] <- stop.value
		trade.returns <- cbind(trade.returns, stop.returns)
	}
	colnames(trade.returns) <- as.character(c(NA, stop.points))
	return(trade.returns)
}




























