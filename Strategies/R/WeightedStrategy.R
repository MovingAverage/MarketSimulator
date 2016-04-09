#'
#'

setClass("WeightedStrategy",
		representation(
				base = "Strategy", 
				weighting.fun = "function", 
				weights = "xts"
		),
		contains = "Strategy")

Weighted <- function(base.strategy, weighting.fun = rolling_mean()) {
	strategy <- new("WeightedStrategy")
	strategy@base <- base.strategy
	strategy@weighting.fun <- weighting.fun
	return(strategy)
}	

setMethod("primeStrategy",
		signature(strategy = "WeightedStrategy"),
		function(strategy, market) {
			strategy@base <- primeStrategy(strategy@base, market)
			strategy@instruments <- strategy@base@instruments
			strategy@weights <- strategy@weighting.fun(strategy@base, market)
			positions(strategy) <- positions(strategy@base) * strategy@weights
			return(strategy)
		})

create_weighting_fun <- function(modifier, by.trade) {
	
	force(modifier)
	force(by.trade)
	
	weighting.fun <- function(strategy, market) {
		returns <- lag(strat_returns(strategy, market))
		returns[is.na(returns)] <- 0
		weights <- modifier(returns)
		if (by.trade) {
			weights <- by_trade_weight(weights, strategy)
		}
		return(weights)
	}
	return(weighting.fun)
}

by_trade_weight <- function(weights, strategy) {
	trade.weights <- weights
	trade.weights[] <- NA
	starts <- trade_start_indexes(positions(strategy))
	for (inst in colnames(weights)) {
		trade.weights[starts[[inst]], inst] <- weights[starts[[inst]], inst]
	}
	return(na.locf(trade.weights))
}

rolling_mean <- function(weight.pd = 40, by.trade = TRUE) {
	
	force(weight.pd)
	
	modifier <- function(returns) {
		rolling.returns <- rollmeanr(returns, k = weight.pd, fill = 0)
		weights <- rolling.returns > 0
		return(weights * 1)
	}
	return(create_weighting_fun(modifier, by.trade))
}

rolling_G <- function(weight.pd = 40, threshold = 0.001, by.trade = TRUE) {
	
	force(weight.pd)
	force(threshold)
	
	modifier <- function(returns) {
		rolling.mu <- rollmeanr(returns, k = weight.pd, fill = 0)
		rolling.var <- rollapplyr(returns, width = weight.pd, FUN = var, fill = 0)
		rolling.G <- sqrt((1 + rolling.mu)^2 - rolling.var) - 1
		weights <- rolling.G > threshold
		return(weights * 1)
	}
	return(create_weighting_fun(modifier, by.trade))
}

ema_slope <- function(ema.pd = 40, by.trade = TRUE) {
	
	force(ema.pd)
	
	modifier <- function(returns) {
		cum.returns <- exp(cumsum(log(returns + 1)))
		ema.returns <- cum.returns
		for (i in 1:ncol(cum.returns)) {
			ema.returns[, i] <- EMA(cum.returns[, i], n = ema.pd)
		}
		ema.returns <- diff(ema.returns)
		ema.returns[is.na(ema.returns)] <- 0
		weights <- ema.returns > 0
		return(weights * 1)
	}
	return(create_weighting_fun(modifier, by.trade))
}

ema_pair <- function(fast = 40, slow = 120, by.trade = TRUE) {
	
	force(fast)
	force(slow)
	
	modifier <- function(returns) {
		cum.returns <- exp(cumsum(log(returns + 1)))
		fast.ema.returns <- slow.ema.returns <- cum.returns
		for (i in 1:ncol(cum.returns)) {
			fast.ema.returns[, i] <- EMA(cum.returns[, i], n = fast)
			slow.ema.returns[, i] <- EMA(cum.returns[, i], n = slow)
		}
		fast.ema.returns <- diff(fast.ema.returns)
		slow.ema.returns <- diff(slow.ema.returns)
		fast.ema.returns[is.na(fast.ema.returns)] <- 0
		slow.ema.returns[is.na(slow.ema.returns)] <- 0
		weights <- fast.ema.returns > 0 & slow.ema.returns > 0
		return(weights * 1)
	}
	return(create_weighting_fun(modifier, by.trade))
}

ema_cross <- function(fast = 40, slow = 120, by.trade = TRUE) {
	
	force(fast)
	force(slow)
	
	modifier <- function(returns) {
		cum.returns <- exp(cumsum(log(returns + 1)))
		fast.ema.returns <- slow.ema.returns <- cum.returns
		for (i in 1:ncol(cum.returns)) {
			fast.ema.returns[, i] <- EMA(cum.returns[, i], n = fast)
			slow.ema.returns[, i] <- EMA(cum.returns[, i], n = slow)
		}
		fast.ema.returns[is.na(fast.ema.returns)] <- 0
		slow.ema.returns[is.na(slow.ema.returns)] <- 0
		weights <- fast.ema.returns > slow.ema.returns
		return(weights * 1)
	}
	return(create_weighting_fun(modifier, by.trade))
}

G_bytrade <- function(weight.pd = 40, threshold = 0.001) {
	
	force(weight.pd)
	force(threshold)
	
	weighting.fun <- function(strategy, market) {
		trade.returns <- trade_daily_returns(strategy, market)
		cum.fun <- function(trade) exp(sum(log(trade + 1), na.rm = TRUE))
		cum.returns <- lapply(trade.returns, function(trades) sapply(trades, cum.fun))
		
		g.fun <- function(trades, n) {
			if (length(trades) > n) {
				G <- sqrt(EMA(trades, n) - EMA((trades - 1) ^ 2, n))
			} else {
				G <- trades
				G[] <- NA
			}
			G[is.na(G)] <- 1
			return(G)
		}
		
		G.trades <- lapply(cum.returns, g.fun, n = weight.pd)
		weights <- dailyReturns(market)
		weights[] <- NA
		G <- weights
		
		# TODO lagging by 2 may be too harsh. Returns on trade close known at day's Open.
		trade.ends <- trade_end_indexes(lag(positions(strategy), 2))
		
		for (i in seq_along(G.trades)) {
			G[trade.ends[[i]], i] <- G.trades[[i]]
		}
		
		# This lag puts the calculated G on the day after trade completes.
		weights <- na.locf(lag(G))
		weights <- (weights > (1 + threshold)) * 1
		weights[is.na(weights)] <- 0
		return(weights)
	}
	return(weighting.fun)
}

EMA_filter <- function(ema.pd = 40) {
	
	force(ema.pd)
	
	weighting.fun <- function(strategy, market) {
		
		dates <- index(dailyReturns(market))
		weights <- xts(, order.by = dates)
		
		for (inst in tradeableInstruments(market)) {
			ema <- lag(EMA(Cl(market[inst]), ema.pd))
			weights <- merge(weights, diff(ema) > 0)
		}
		colnames(weights) <- tradeableInstruments(market)
		weights <- by_trade_weight(weights, strategy)
		return(weights)
	}
	return(weighting.fun)
}

Volatility_filter <- function(vol.pd = 20, vol.threshold = 0.2) {
	
	force(vol.pd)
	force(vol.threshold)
	
	weighting.fun <- function(strategy, market) {
		
		dates <- index(dailyReturns(market))
		weights <- xts(, order.by = dates)
		
		for (instrument in tradeableInstruments(market)) {
			vol <- volatility(market[instrument], n = vol.pd)
			weights <- merge(weights, vol > vol.threshold)
		}
		colnames(weights) <- tradeableInstruments(market)
		weights <- by_trade_weight(weights, strategy)
		return(weights)
	}
	return(weighting.fun)
}


















