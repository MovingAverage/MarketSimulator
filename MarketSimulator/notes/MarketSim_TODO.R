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
# 	DONE - Implement show method for orders
# 	DONE - Cancel open buy order, instead of placing equivalent sell order.
# 	DONE - Combine two separate buy orders.
# 	DONE - Add latest price to Position on creation
# 	DONE - Positions object to hold list of positions
# 	DONE - CurrentPosition should have size slot instead of holding Account
# 	DONE - Pull up Account into Positions instead of each Position
# 	DONE - Remove Account from Broker and have as stand-alone object
# 	DONE - Determine if target changes are cost effective
# 	DONE - Submit target change to Position for action
# 	DONE - Change Account to only have cash. Positions report on holdings value.
# 	DONE - Create positions at Manager and update from Broker

# 	Make sure position uses current size to sell if target is zero
# 	Update prices in Positions with latest.prices from Broker
# 	Store closed orders for day instead of data.frame of transactions
# 	Handle multiple transactions on single instrument in day
# 	Don't accept more orders if insufficient cash

# Higher level TODOs
# 	1. Get backtest happening with stop orders
#	2. Combining strategy outputs


# Object Interfaces
# Strategy
#		Sits outside the Market Simulator package
#		Communicates changes in target positions
# 		Combines changes from multiple sub-strategies
# Manager
#		Represents the person submitting the orders
#		Determines effective changes considering costs / cash etc.
# 		Handles portfolio of positions
# 		Applies risk settings and controls
# Position
#		Interface to Broker
# 		Facilitates changes to positions
# 		Submits orders to Broker
#		Receives notice of executed orders
# Broker 
#		Handles submitting orders to Market


