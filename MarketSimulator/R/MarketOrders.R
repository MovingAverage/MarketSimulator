#' 
#' 
setClass("Order", 
		representation(
			instrument = "character",
			ID = "numeric"
		))

instrumentOf <- function(order) {
	return(order@instrument)
}

setID <- function(order, ID) {
	order@ID <- ID
	return(order)
} 

getID <- function(order) {
	return(order@ID)
}

#' Market Order
#' 
#' Opens position at first opportunity.
#'
setClass("MarketOrder",
		representation(
			status = "character",
			quantity = "integer",
			execution.price = "xts"
		), 
		contains = "Order")

#' Create market order
#' 
#' Creates an order to be submitted to the broker which is to be executed at market.
#' This order will effectively be executed at the next day's open price.
#' The order is to be specified with one of either 'buy' or 'sell' amounts. If both
#' are provided it will throw an error.
#' 
#' @param instrument character identifying the instrument the order is related to.
#' @param buy the number of shares to buy
#' @param sell the number of shares to sell
#' 
Order <- function(instrument, buy = NULL, sell = NULL) {
	
	if (missing(instrument)) {
		stop("Require an instrument identifier")
	}
	if (is.null(buy) && is.null(sell)) {
		stop("Require an order quantity")
	}
	if (is.numeric(buy) && is.numeric(sell)) {
		stop("Must have only one of buy or sell")
	}
	quantity <- ifelse(is.null(buy), -as.integer(sell), as.integer(buy))
	order <- new("MarketOrder", 
			instrument = instrument, 
			status = "open",
			quantity = quantity,
			execution.price = xts())
	return(order)
}

setGeneric("notify",
		function(order, broker, price.bar, ...) {
			if (are_related(order, price.bar) && active_market(price.bar)) {
				order@execution.price <- Op(price.bar)
				order@status <- "closed"
				updateOrder(broker, order)
			}
		})

status <- function(order) {
	return(order@status)
}

quantity <- function(order) {
	return(order@quantity)
}

execution_price <- function(order) {
	return(order@execution.price)
}

are_related <- function(order, price.bar) {
	return(any(grepl(instrumentOf(order), names(price.bar))))
}

active_market <- function(price.bar) {
	volume.ok <- not_NA_or_Zero(Vo(price.bar))
	open.ok <- not_NA_or_Zero(Op(price.bar))
	return(volume.ok & open.ok)
}

not_NA_or_Zero <- function(value) {
	!(is.na(value) || value == 0)
}


#' Limit Order
#' 
setClass("LimitOrder",
		representation(
			limit.price = "xts"
		),
		contains = "MarketOrder")
		

Limit <- function(instrument, buy = NULL, sell = NULL, at) {
	
	if (missing(instrument)) {
		stop("Require an instrument identifier")
	}
	if (missing(at)) {
		stop("Limit order must have a limit price")
	}
	if (is.null(buy) && is.null(sell)) {
		stop("Require an order quantity")
	}
	if (is.numeric(buy) && is.numeric(sell)) {
		stop("Must have only one of buy or sell")
	}
	quantity <- ifelse(is.null(buy), -as.integer(sell), as.integer(buy))
	order <- new("LimitOrder", 
			instrument = instrument, 
			status = "open",
			quantity = quantity,
			execution.price = xts(), 
			limit.price = at)
	return(order)
}

limit_price <- function(limit.order) {
	return(limit.order@limit.price)
}

setMethod("notify",
		signature(order = "LimitOrder"),
		function(order, broker, price.bar) {
			if (active_market(price.bar)) {
				if (Op(price.bar) < limit_price(order)) {
					order@execution.price <- Op(price.bar)
					order@status <- "closed"
					updateOrder(broker, order)
				}
				if (Lo(price.bar) < limit_price(order)) {
					order@execution.price <- limit_price(order)
					order@status <- "closed"
					updateOrder(broker, order)
				}
			}
		})


#' Stop Loss Order
#' 
setClass("StopLossOrder",
		contains = "LimitOrder")

Stop <- function(instrument, buy = NULL, sell = NULL, at) {
	
	if (missing(instrument)) {
		stop("Require an instrument identifier")
	}
	if (missing(at)) {
		stop("Limit order must have a limit price")
	}
	if (is.null(buy) && is.null(sell)) {
		stop("Require an order quantity")
	}
	if (is.numeric(buy) && is.numeric(sell)) {
		stop("Must have only one of buy or sell")
	}
	quantity <- ifelse(is.null(buy), -as.integer(sell), as.integer(buy))
	order <- new("StopLossOrder", 
			instrument = instrument, 
			status = "open",
			quantity = quantity,
			execution.price = xts(), 
			limit.price = at)
	return(order)
}













