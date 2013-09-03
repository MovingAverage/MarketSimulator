#' 
#' Parent object from which different Order types may be derived.
#' 
setClass("Order", 
		representation(
			instrument = "character",
			ID = "character", 
			status = "OrderStatus",
			quantity = "integer",
			execution.price = "xts", 
			txn.cost.model = "function", 
			submission.time = "POSIXct", 
			status.time = "POSIXct"
		))

setID <- function(order, ID) {
	order@ID <- ID
	return(order)
} 

getID <- function(order) {
	return(order@ID)
}

setMethod("status",
		signature("Order"),
		function(object) {
			status(object@status)
		})

setMethod("status<-",
		signature("Order"),
		function(object, value) {
			object@status <- value
			return(object)
		})

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

"submissionTime<-" <- function(order, value) {
	order@submission.time <- as.POSIXct(value)
	return(order)
}

submissionTime <- function(order) {
	return(order@submission.time)
}

"statusTime<-" <- function(order, value) {
	order@status.time <- as.POSIXct(value)
	return(order)
}

statusTime <- function(order) {
	return(order@status.time)
}

setGeneric("areSimilar",
		function(order1, order2, ...) {
			return(identical(class(order1), class(order2)))
		})

setGeneric("mergeOrders",
		function(existing.order, new.order) {
			if (instrumentOf(existing.order) != instrumentOf(new.order)) {
				stop("Instruments differ between orders")
			}
			if (inherits(existing.order, "Order", which = TRUE) != 
					inherits(new.order, "Order", which = TRUE)) {
				stop("Attempt to merge different order types")
			}
			order <- standardGeneric("mergeOrders")
			order <- setTxnCostModel(order, existing.order@txn.cost.model)
			order <- setID(order, getID(existing.order))
			if (quantity(order) == 0) {
				order@status <- NullStatus()
			}
			return(order)
		})

writeTransaction <- function(order) {
	data.frame(
			size = quantity(order), 
			price = as.numeric(execution_price(order)), 
			costs = txnFees(order), 
			row.names = instrumentOf(order))
}

asDataFrame <- function(x) {
	
	order <- list(
			type = class(x), 
			ID = getID(x), 
			instrument = instrumentOf(x), 
			status = status(x), 
			quantity = quantity(x), 
			price = execution_price(x), 
			submitted = submissionTime(x), 
			updated = statusTime(x))
	
	order <- order[sapply(order, length) == 1 & sapply(order, class) != "S4"]
	
	as.data.frame(order)
}

bookEntry <- function(order) {
	
	timestamp <- as.Date("2010-04-20")
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
			order.by = submissionTime(order))
	
	return(ordertemplate)
}

setMethod("show",
		signature(object = "Order"),
		function(object) {
			show(asDataFrame(object))
		})

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

execute <- function(order, at, broker) {
	order@execution.price <- at
	order@status <- ClosedStatus()
	statusTime(order) <- today(broker)
	updateOrder(order, broker)
}

updateOrder <- function(order, broker) {
	callUpdateProcedure(order@status, order, broker)
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
	quantity <- ifelse(is.null(buy), -as.integer(abs(sell)), as.integer(abs(buy)))
	order <- new("MarketOrder", 
			instrument = instrument, 
			status = OpenStatus(),
			quantity = quantity,
			execution.price = xts(), 
			submission.time = initDate(), 
			status.time = initDate())
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

setMethod("mergeOrders",
		signature(existing.order = "MarketOrder", new.order = "MarketOrder"),
		function(existing.order, new.order) {
			quantity <- quantity(existing.order) + quantity(new.order)
			if (quantity > 0) {
				order <- Order(instrumentOf(existing.order), buy = quantity)
			} else {
				order <- Order(instrumentOf(existing.order), sell = quantity)
			}
			return(order)
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
				status = OpenStatus(),
				quantity = -abs(as.integer(sell)),
				execution.price = xts(), 
				limit.price = at, 
				submission.time = initDate(), 
				status.time = initDate())
	} else {
		order <- new("BuyLimitOrder", 
				instrument = instrument, 
				status = OpenStatus(),
				quantity = abs(as.integer(buy)),
				execution.price = xts(), 
				limit.price = at, 
				submission.time = initDate(), 
				status.time = initDate())
	}
	return(order)
}

limit_price <- function(limit.order) {
	return(limit.order@limit.price)
}

setMethod("notify",
		signature(order = "BuyLimitOrder"),
		function(order, broker, price.bar) {
			if (as.numeric(Op(price.bar)) < as.numeric(limit_price(order))) {
				execute(order, at = Op(price.bar), broker)
			} else {
				if (as.numeric(Lo(price.bar)) < as.numeric(limit_price(order))) {
					execute(order, at = limit_price(order), broker)
				}
			}
		})

setMethod("notify",
		signature(order = "SellLimitOrder"),
		function(order, broker, price.bar) {
			if (as.numeric(Op(price.bar)) > as.numeric(limit_price(order))) {
				execute(order, at = Op(price.bar), broker)
			} else {
				if (as.numeric(Hi(price.bar)) > as.numeric(limit_price(order))) {
					execute(order, at = limit_price(order), broker)
				}
			}
		})

setMethod("areSimilar",
		signature(order1 = "LimitOrder", order2 = "LimitOrder"),
		function(order1, order2) {
			limit.dist1 <- inherits(order1, "LimitOrder", which = TRUE)
			limit.dist2 <- inherits(order2, "LimitOrder", which = TRUE)
			
			same.type <- limit.dist1 == limit.dist2
			same.limit <- limit_price(order1) == limit_price(order2)
			
			return(same.type && same.limit)
		})

setMethod("mergeOrders",
		signature(existing.order = "LimitOrder", new.order = "LimitOrder"),
		function(existing.order, new.order) {
			stop("merge for Limit orders not defined")
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
				status = OpenStatus(),
				quantity = -abs(as.integer(sell)),
				execution.price = xts(), 
				limit.price = at, 
				submission.time = initDate(), 
				status.time = initDate())
	} else {
		order <- new("BuyStopLoss", 
				instrument = instrument, 
				status = OpenStatus(),
				quantity = abs(as.integer(buy)),
				execution.price = xts(), 
				limit.price = at, 
				submission.time = initDate(), 
				status.time = initDate())
	}
	return(order)
}

setMethod("notify",
		signature(order = "BuyStopLoss"),
		function(order, broker, price.bar) {
			if (as.numeric(Op(price.bar)) > as.numeric(limit_price(order))) {
				execute(order, at = Op(price.bar), broker)
			} else {
				if (as.numeric(Hi(price.bar)) > as.numeric(limit_price(order))) {
					execute(order, at = limit_price(order), broker)
				}
			}
		})

setMethod("notify",
		signature(order = "SellStopLoss"),
		function(order, broker, price.bar) {
			if (as.numeric(Op(price.bar)) < as.numeric(limit_price(order))) {
				execute(order, at = Op(price.bar), broker)
			} else {
				if (as.numeric(Lo(price.bar)) < as.numeric(limit_price(order))) {
					execute(order, at = limit_price(order), broker)
				}
			}
		})

setMethod("mergeOrders",
		signature(existing.order = "StopLossOrder", new.order = "StopLossOrder"),
		function(existing.order, new.order) {
			quantity <- quantity(existing.order) + quantity(new.order)
			limit <- limit_price(existing.order)
			if (quantity > 0) {
				order <- Stop(instrumentOf(existing.order), buy = quantity, at = limit)
			} else {
				order <- Stop(instrumentOf(existing.order), sell = quantity, at = limit)
			}
			return(order)
		})



#' Combination order - Market with Stop loss
#' 
setClass("MarketWithStop",
		representation(
				stop.point = "numeric"				
		), 
		contains = "MarketOrder")

setClass("LongMarketWithStop",
		contains = "MarketWithStop")

setClass("ShortMarketWithStop",
		contains = "MarketWithStop")
		
MarketWithStop <- function(instrument, buy = NULL, sell = NULL, stop.point) {
	
	check_order_parameters(instrumnet, buy, sell)
	if (missing(stop.point)) {
		stop("Stop point must be supplied")
	}
	if (is.null(buy)) {
		order <- new("ShortMarketWithStop", 
				instrument = instrument, 
				status = OpenStatus(),
				quantity = -abs(as.integer(sell)),
				execution.price = xts(), 
				stop.point = abs(stop.point), 
				submission.time = initDate(), 
				status.time = initDate())
	} else {
		order <- new("LongMarketWithStop", 
				instrument = instrument, 
				status = OpenStatus(),
				quantity = abs(as.integer(buy)),
				execution.price = xts(),
				stop.point = abs(stop.point), 
				submission.time = initDate(), 
				status.time = initDate())
	}
}

setMethod("notify",
		signature(order = "LongMarketWithStop"),
		function(order, broker, price.bar) {
			notify(asMarketOrder(order), broker, price.bar)
			stop.price <- Op(price.bar) * (1 - order@stop.point)
			addOrder(broker, 
					Stop(instrumentOf(order), sell = quantity(order), at = stop.price))
		})

setGeneric("asMarketOrder",
		function(order) {
			standardGeneric("asMarketOrder")
		})

setMethod("asMarketOrder",
		signature(order = "LongMarketWithStop"),
		function(order) {
			market.order <- Order(instrumentOf(order), buy = quantity(order))
			market.order <- setID(market.order, getID(order))
			market.order <- setTxnCostModel(market.order, order@txn.cost.model)
			submissionTime(market.order) <- submissionTime(order)
			return(market.order)
		})

setMethod("asMarketOrder",
		signature(order = "ShortMarketWithStop"),
		function(order) {
			market.order <- Order(instrumentOf(order), sell = quantity(order))
			market.order <- setID(market.order, getID(order))
			market.order <- setTxnCostModel(market.order, order@txn.cost.model)
			submissionTime(market.order) <- submissionTime(order)
			return(order)
		})












































