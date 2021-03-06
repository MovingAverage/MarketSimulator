#'
#'
#' Utility functions for examining strategy performance using daily returns.

strat_returns <- function(strategy, market, out.of.position = NA) {
	
	market.returns <- dailyReturns(market, type = "arithmetic")
	positions <- lag(positions(strategy), 2)
	strat.returns <- market.returns * positions
	strat.returns[positions == 0] <- out.of.position
	return(strat.returns)
}

net_strat_returns <- function(strategy, market, round.trip.cost = 0.002) {
	strat.returns <- strat_returns(strategy, market)
	cost.per.day <- round.trip.cost / median_trade_length(positions(strategy))
	cost.per.day <- rep(cost.per.day, each = nrow(strat.returns))
	return(strat.returns - cost.per.day)
}

median_trade_length <- function(positions) {
	sapply(trade_lengths(positions), median)
}

mean_intrade_returns <- function(strat.returns) {
	apply(strat.returns, 2, mean, na.rm = TRUE)
}

sd_intrade_returns <- function(strat.returns) {
	apply(strat.returns, 2, sd, na.rm = TRUE)
}

geom_intrade_returns <- function(strat.returns) {
	mean.returns <- mean_intrade_returns(strat.returns)
	sd.returns <- sd_intrade_returns(strat.returns)
	geometric_returns(mean.returns, sd.returns)
}

geometric_returns <- function(mean, sd) {
	sqrt((1 + mean) ^ 2 - sd ^ 2) - 1
}

mean_annual_returns <- function(strat.returns) {
	strat.returns[is.na(strat.returns)] <- 0
	returns <- apply(strat.returns, 2, mean)
	returns * 250
}

sd_annual_returns <- function(strat.returns) {
	strat.returns[is.na(strat.returns)] <- 0
	returns <- apply(strat.returns, 2, sd)
	returns * sqrt(250)
}

geom_annual_returns <- function(strat.returns) {
	mean.returns <- mean_annual_returns(strat.returns)
	sd.returns <- sd_annual_returns(strat.returns)
	geometric_returns(mean.returns, sd.returns)
}


strategy_comparison <- function(strategies, market, metric = mean_intrade_returns) {
	
	n.strats <- length(strategies)
	
	strat.names <- names(strategies)
	if (is.null(strat.names)) {
		strat.names <- sapply(strategies, class)
	}
	
	strat.returns <- lapply(strategies, strat_returns, market = market)
	strat.returns <- c(strat.returns, list(dailyReturns(market)))
	strat.metrics <- lapply(strat.returns, metric)
	
	summary.metric <- matrix(NA, nrow = n.strats + 1, ncol = length(strat.metrics[[1]]))
	colnames(summary.metric) <- tradeableInstruments(market)
	for (s in 1:nrow(summary.metric)) {
		summary.metric[s, ] <- strat.metrics[[s]]
	}
	
	rownames(summary.metric) <- c(strat.names, "Market")
	
	cols <- c(rainbow(n.strats, start = 0.6, end = 0.9, alpha = 0.4), "grey")
	bdrs <- c(rainbow(n.strats, start = 0.6, end = 0.9), "black")
	
	strategy.summary <- list()
	strategy.summary$data <- summary.metric
	strategy.summary$metric <- metric
	strategy.summary$colors <- cols
	strategy.summary$borders <- bdrs
	class(strategy.summary) <- "strategy_summary"
	
	return(strategy.summary)
}

barplot.strategy_summary <- function(height, legend.pos = "topright", ...) {
	
	barplot(height$data, beside = TRUE, cex.names = 0.8, 
			col = height$colors, border = height$borders, 
			names.arg = colnames(height$data), legend.text = rownames(height$data), 
			args.legend = list(x = legend.pos), las = 3)
}

boxplot.strategy_summary <- function(x, ...) {
	
	boxplot(t(x$data), col = x$colors, border = x$borders)
	abline(h = 0)
}

instrument_returns <- function(returns.list, instrument, time.pd = "2007::") {
	
	returns <- returns.list[["Market"]][, instrument][time.pd]
	
	for (strat.returns in returns.list[-1]) {
		returns <- cbind(returns, strat.returns[, instrument][time.pd])
	}
	
	names(returns) <- c(instrument, names(returns.list[-1]))
	return(returns)
}

strategy_returns <- function(strategies, market) {
	
	returns <- list()
	returns[["Market"]] <- dailyReturns(market)
	
	strat.names <- names(strategies)
	if (is.null(strat.names)) {
		strat.names <- sapply(strategies, class)
	}
	
	for (strategy in strat.names) {
		returns[[strategy]] <- strat_returns(strategies[[strategy]], market, 
				out.of.position = 0)
	}
	return(returns)
}

combined_returns <- function(strategy, market) {
	returns <- strat_returns(strategy, market)
	xts(rowMeans(returns, na.rm = TRUE), order.by = index(returns))
}

equity_returns <- function(returns) {
	returns[is.na(returns)] <- 0
	exp(cumsum(log(returns + 1)))
}

overall_returns <- function(strategies, market, exclude = NULL) {
	
	if (!is.null(exclude)) {
		exclude <- -1 * abs(exclude)
		market.returns <- dailyReturns(market)[, exclude]
	} else {
		market.returns <- dailyReturns(market)
	}
	
	rets <- xts(rowMeans(market.returns, na.rm = TRUE), order.by = index(market.returns))
	
	for (strat in strategies) {
		rets <- cbind(rets, combined_returns(strat, market))
	}
	names(rets) <- c("Mkt", names(strategies))
	return(rets)
}

weight_performance <- function(strategy, market) {
	
	base.returns <- as.numeric(strat_returns(strategy@base, market))
	groups <- as.factor(strategy@weights)
	mean <- tapply(base.returns, groups, mean, na.rm = TRUE) * 250
	sd <- tapply(base.returns, groups, sd, na.rm = TRUE) * sqrt(250)
	G.fun <- function(r) sqrt((1 + mean(r, na.rm = TRUE)) ^ 2 - var(r, na.rm = TRUE))
	G <- tapply(base.returns, groups, G.fun) ^ 250 - 1
	results <- rbind(mean, sd, G)
	results <- cbind(results, rep(0, 3))
	colnames(results)[ncol(results)] <- "Base"
	results[1, "Base"] <- mean(base.returns, na.rm = TRUE) * 250
	results[2, "Base"] <- sd(base.returns, na.rm = TRUE) * sqrt(250)
	results[3, "Base"] <- G.fun(base.returns) ^ 250 - 1
	rownames(results) <- c("mean", "sd", "G")
	results
}


























