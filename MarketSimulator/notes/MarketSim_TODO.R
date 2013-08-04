#'
#'
#' Todo list for MarketSimulator package

# 	DONE - Add order objects to broker
# 	DONE - Orders are stored for each instrument
#	DONE - Distinguish between buy and sell orders
# 	DONE - Set order submission date
# 	DONE - Set order status date
# 	DONE - Set order transaction fees
#	DONE - Cancel open orders for instrument
# 	DONE - Execute at market orders
# 	DONE - Execute stop orders
# 	DONE - Execute limit orders
# 	DONE - Separate closed orders from open
# 	DONE - Report on closed orders for given timestamp
# 	DONE - Report on market information for given timestamp
# 	DONE - Broker reports on current equity and latest prices.
# 	DONE - Orders print a row of the order book
# 	DONE - Broker returns an order book object
# 	DONE - Order submission time should be set when added to Broker not when notified.

# 	Implement show method for orders
# 	Cancel open buy order, instead of placing equivalent sell order.
# 	Combine two separate buy orders.
# 	Handle multiple transactions on single instrument in day

# Example of duplicate transactions ("2008-06-09" from backtest triple: AMP, BHP, CBA)
#			size price costs
#	AMP.AX  -423  7.42     0
#	BHP.AX    42 43.78     0
#	BHP.AX1  109 43.78     0
#	CBA.AX    36 42.42     0
#	CBA.AX1 -110 42.42     0