#' Market Order
#' 
#' Opens position at first opportunity.
#'
setClass("MarketOrder",
		representation(
			instrument = "character", 
			status = "character", 
			execution.price = "xts"
		))
		

Order <- function(instrument) {
	
	if (missing(instrument)) {
		stop("Require an instrument identifier")
	}
	order <- new("MarketOrder", 
			instrument = instrument, 
			status = "open", 
			execution.price = xts())
	return(order)
}

setGeneric("notify",
		function(order, broker, price.bar, ...) {
			if (are_related(order, price.bar) && active_market(price.bar)) {
				order@execution.price <- Op(price.bar)
				order@status <- "closed"
			}
			return(order)
		})

status <- function(order) {
	return(order@status)
}

execution_price <- function(order) {
	return(order@execution.price)
}

are_related <- function(order, price.bar) {
	return(any(grepl(order@instrument, names(price.bar))))
}

active_market <- function(price.bar) {
	volume.ok <- not_NA_or_Zero(Vo(price.bar))
	open.ok <- not_NA_or_Zero(Op(price.bar))
	return(volume.ok & open.ok)
}

not_NA_or_Zero <- function(value) {
	!(is.na(value) || value == 0)
}


#' Stop Loss Order
#' 
setClass("StopLossOrder",
		representation(
			limit = "numeric", 
			parent = "MarketOrder"
		))
		
Stop <- function(instrument, limit) {
	
	if (missing(limit)) {
		stop("Require limit for StopLoss")
	}
	parent <- Order(instrument)
	order <- new("StopLossOrder", 
			parent = parent, 
			limit = limit)
	return(order)
}

limit_price <- function(stop.loss, price) {
	return(price * (1 - stop.loss@limit))
}

setMethod("notify",
		signature(order = "StopLossOrder"),
		function(order, broker, price.bar) {
			parent.order <- notify(order@parent, broker, price.bar)
			if (status(parent.order) == "closed") {
				addOrder(broker, Limit(parent.order@instrument, 
								limit_price(order, parent.order@execution.price)))
			}
			return(parent.order)
		})


#' Limit Order
#' 
setClass("LimitOrder",
		representation(
			limit.price = "xts"
		),
		contains = "MarketOrder")
		

Limit <- function(instrument, limit.price) {
	
	if (missing(instrument)) {
		stop("Require an instrument identifier")
	}
	if (missing(limit.price)) {
		stop("Limit order must have a limit price")
	}
	order <- new("LimitOrder", 
			instrument = instrument, 
			status = "open", 
			execution.price = xts(), 
			limit.price = limit.price)
	return(order)
}












