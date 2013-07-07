#' 
#' 
setClass("Order", 
		representation(
			instrument = "character",
			ID = "integer", 
			status = "character",
			quantity = "integer",
			execution.price = "xts", 
			txn.cost.model = "function"
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

status <- function(order) {
	return(order@status)
}

quantity <- function(order) {
	return(order@quantity)
}

execution_price <- function(order) {
	return(order@execution.price)
}

setTxnCostModel <- function(order, model) {
	order@txn.cost.model <- model
	return(order)
}

txnFees <- function(order) {
	return(order@txn.cost.model(order))
}

bookEntry <- function(order) {
	
	ordertemplate <- xts(t(c(
							"Order.Qty" = quantity(order), 
							"Order.Price" = execution_price(order), 
							"Order.Type" = class(order), 
							"Order.Side" = "long",
							"Order.Threshold" = 0, 
							"Order.Status" = status(order), 
							"Order.StatusTime" = as.character(as.POSIXct(timestamp)), 
							"Prefer" = "", 
							"Order.Set" = "", 
							"Txn.Fees" = txnFees(order), 
							"Rule" = "")), 
			order.by = as.POSIXct(timestamp))
	
	return(ordertemplate)
}

#' notify order of market activity
#' 
#' \code{notify} will generally be called by the broker when a new price bar is available.
#' If the instrument of the price bar and order are the same, and provided that the price
#' bar has both sufficient volume and price information, then the order will check to see
#' if it was executed.
#' If executed the order will change its status with the broker.
#' 
#' @param order the order to be notified
#' @param broker the broker with which the order is held
#' @param price.bar the OHLCV information representing the day's activity.
#' 
setGeneric("notify",
		function(order, broker, price.bar, ...) {
			if (are_related(order, price.bar) && active_market(price.bar)) {
				standardGeneric("notify")
			}
		})

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

execute <- function(order, at, broker) {
	order@execution.price <- at
	order@status <- "closed"
	updateOrder(broker, order)
}


#' Market Order
#' 
#' Opens position at first opportunity.
#'
setClass("MarketOrder",
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
	
	check_order_parameters(instrument, buy, sell)
	quantity <- ifelse(is.null(buy), -as.integer(sell), as.integer(buy))
	order <- new("MarketOrder", 
			instrument = instrument, 
			status = "open",
			quantity = quantity,
			execution.price = xts())
	return(order)
}

check_order_parameters <- function(instrument, buy, sell) {
	if (missing(instrument)) {
		stop("Require an instrument identifier")
	}
	if (is.null(buy) && is.null(sell)) {
		stop("Require an order quantity")
	}
	if (is.numeric(buy) && is.numeric(sell)) {
		stop("Must have only one of buy or sell")
	}
}

setMethod("notify",
		signature(order = "MarketOrder"),
		function(order, broker, price.bar) {
			execute(order, at = Op(price.bar), broker)
		})


#' Limit Order
#' 
setClass("LimitOrder",
		representation(
			limit.price = "xts"
		),
		contains = "MarketOrder")

setClass("BuyLimitOrder",
		contains = "LimitOrder")
		
setClass("SellLimitOrder",
		contains = "LimitOrder")
		

Limit <- function(instrument, buy = NULL, sell = NULL, at) {
	
	check_order_parameters(instrument, buy, sell)
	if (missing(at)) {
		stop("Limit order must have a limit price")
	}
	if (is.null(buy)) {
		order <- new("SellLimitOrder", 
				instrument = instrument, 
				status = "open",
				quantity = -abs(as.integer(sell)),
				execution.price = xts(), 
				limit.price = at)
	} else {
		order <- new("BuyLimitOrder", 
				instrument = instrument, 
				status = "open",
				quantity = abs(as.integer(buy)),
				execution.price = xts(), 
				limit.price = at)
	}
	return(order)
}

limit_price <- function(limit.order) {
	return(limit.order@limit.price)
}

setMethod("notify",
		signature(order = "BuyLimitOrder"),
		function(order, broker, price.bar) {
			if (Op(price.bar) < limit_price(order)) {
				execute(order, at = Op(price.bar), broker)
			} else {
				if (Lo(price.bar) < limit_price(order)) {
					execute(order, at = limit_price(order), broker)
				}
			}
		})

setMethod("notify",
		signature(order = "SellLimitOrder"),
		function(order, broker, price.bar) {
			if (Op(price.bar) > limit_price(order)) {
				execute(order, at = Op(price.bar), broker)
			} else {
				if (Hi(price.bar) > limit_price(order)) {
					execute(order, at = limit_price(order), broker)
				}
			}
		})

#' Stop Loss Order
#' 
setClass("StopLossOrder",
		contains = "LimitOrder")

setClass("BuyStopLoss",
		contains = "StopLossOrder")
		
setClass("SellStopLoss",
		contains = "StopLossOrder")
		

Stop <- function(instrument, buy = NULL, sell = NULL, at) {
	
	check_order_parameters(instrument, buy, sell)
	if (missing(at)) {
		stop("Limit order must have a limit price")
	}
	if (is.null(buy)) {
		order <- new("SellStopLoss", 
				instrument = instrument, 
				status = "open",
				quantity = -abs(as.integer(sell)),
				execution.price = xts(), 
				limit.price = at)
	} else {
		order <- new("BuyStopLoss", 
				instrument = instrument, 
				status = "open",
				quantity = abs(as.integer(buy)),
				execution.price = xts(), 
				limit.price = at)
	}
	return(order)
}

setMethod("notify",
		signature(order = "BuyStopLoss"),
		function(order, broker, price.bar) {
			if (Op(price.bar) > limit_price(order)) {
				execute(order, at = Op(price.bar), broker)
			} else {
				if (Hi(price.bar) > limit_price(order)) {
					execute(order, at = limit_price(order), broker)
				}
			}
		})

setMethod("notify",
		signature(order = "SellStopLoss"),
		function(order, broker, price.bar) {
			if (Op(price.bar) < limit_price(order)) {
				execute(order, at = Op(price.bar), broker)
			} else {
				if (Lo(price.bar) < limit_price(order)) {
					execute(order, at = limit_price(order), broker)
				}
			}
		})








