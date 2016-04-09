#' 
#' 
#' Utility functions for examining performance by trade

trade_start_indexes <- function(positions) {
	sequence <- lapply(positions, as_character_seq)
	starts <- gregexpr("1+", sequence)
	starts <- lapply(starts, as.numeric)
	names(starts) <- names(positions)
	return(starts)
}

as_character_seq <- function(positions) {
	positions <- na.locf(positions)
	positions[is.na(positions)] <- 0
	positions <- (positions != 0) * 1
	sequence <- paste(as.character(positions), collapse = "")
	return(sequence)
}

trade_end_indexes <- function(positions) {
	starts <- trade_start_indexes(positions)
	lengths <- trade_lengths(positions)
	FUN <- function(start, length) start + length - 1
	return(mapply(FUN, starts, lengths))
}

trade_lengths <- function(positions) {
	sequence <- sapply(positions, as_character_seq)
	trades <- strsplit(sequence, "0")
	trade.lengths <- lapply(trades, nchar)
	trade.lengths <- lapply(trade.lengths, function(x) x[x != 0])
	names(trade.lengths) <- names(positions)
	return(trade.lengths)
}

trade_daily_returns <- function(strategy, market) {
	# TODO Profile this function to look for speed improvements
	
	returns <- dailyReturns(market, type = "arithmetic")
	positions <- lag(positions(strategy), 2)
	
	starts <- trade_start_indexes(positions)
	ends <- trade_end_indexes(positions)
	
	trade.returns <- list()
	for (i in seq_along(starts)) {
		indexes <- as.list(paste(starts[[i]], ends[[i]], sep = ":"))
		indexes <- lapply(indexes, function(x) eval(parse(text = x)))
		trade.returns[[i]] <- lapply(indexes, function(x, rets) rets[x], 
				rets = returns[, i])
	}
	names(trade.returns) <- names(positions)
	return(trade.returns)
}

broomstick_plot <- function(trade.returns, instrument) {
	
	trade.returns <- trade.returns[[instrument]]
	cum.returns <- lapply(trade.returns, function(trade) exp(cumsum(log(trade + 1))))
	max.length <- max(sapply(cum.returns, length))
	max.R <- max(sapply(cum.returns, max, na.rm = TRUE))
	min.R <- min(sapply(cum.returns, min, na.rm = TRUE))
	
	plot(1:max.length, 1:max.length, type = 'n', ylim = c(min.R * 0.95, max.R * 1.05), 
			main = instrument, xlab = "Days", ylab = "Return")
	
	cols <- rainbow(length(cum.returns), start = 0.5, end = 0.8)
	
	for (l in seq_along(cols)) {
		lines(as.numeric(cum.returns[[l]]), col = cols[l])
	}
	
}

trade_returns <- function(trade.daily.returns) {
	
	overall.returns <- list()
	sum.returns <- function(R) exp(sum(log(R + 1))) - 1
	
	for (instrument in names(trade.daily.returns)) {
		returns <- trade.daily.returns[[instrument]]
		overall.returns[[instrument]] <- sapply(returns, sum.returns)
	}
	return(overall.returns)
}

trade_entry_values <- function(positions, values, lag.n = 2) {
	
	positions <- lag(positions, lag.n)
	values <- lag(values, lag.n)
	start.indexes <- trade_start_indexes(positions)
	entry.values <- list()
	
	for (instrument in names(start.indexes)) {
		indexes <- start.indexes[[instrument]]
		entry.values[[instrument]] <- values[, instrument][indexes]
	}
	return(entry.values)
}

returns_vs_filter <- function(strategy, market, filter) {
	
	filter.values <- measures(filter)
	
	if (length(unique(filter.values)) > 10) {
		filter.breaks <- quantile(filter.values, probs = seq(0, 1, 0.1), na.rm = TRUE)
		filter.factor <- cut(filter.values, filter.breaks)
	} else {
		filter.factor <- factor(filter.values)
	}
	
	filter.levels <- levels(filter.factor)
	filter.values[] <- filter.factor
	
	trade.returns <- trade_returns(trade_daily_returns(strategy, market))
	filter.at.entry <- trade_entry_values(positions(strategy), filter.values, lag.n = 2)
	filter.at.entry <- lapply(filter.at.entry, as.factor)
	
	summary <- list()
	summary$returns <- trade.returns
	summary$filter <- filter.at.entry
	summary$levels <- filter.levels
	summary$mean <- filter_apply(summary, mean, na.rm = TRUE)
	summary$sd <- filter_apply(summary, sd, na.rm = TRUE)
	summary$G <- geometric_returns(summary$mean, summary$sd)
	class(summary) <- "filter_returns"
	return(summary)
}

filter_apply <- function(filter.summary, FUN, ...) {
	
	instruments <- names(filter.summary$returns)
	levels <- filter.summary$levels
	
	result <- array(NA, dim = c(length(instruments), length(levels)), 
			dimnames = list(instruments = instruments, levels = levels))
	
	for (instrument in instruments) {
		R <- filter.summary$returns[[instrument]]
		F <- filter.summary$filter[[instrument]]
		levels <- filter.summary$levels[as.numeric(levels(F))]
		result[instrument, levels] <- tapply(R, F, FUN, ...)
	}
	return(result)
}




























