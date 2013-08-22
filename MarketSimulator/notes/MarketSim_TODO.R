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
# 	DONE - Consider removing PositionSet, and have Manager hold list of positions

# 	Test for merging stop orders
# 	Test for cancelling stop orders
# 	Close position instead of setting target of zero
# 	Make sure position uses current size to sell if target is zero
# 	Store closed orders for day instead of data.frame of transactions
# 	Make Transactions object.
# 	Handle multiple transactions on single instrument in day
# 	Don't accept more orders if insufficient cash


#	[1] "2007-09-11 08:00:00 AMP.AX 992 @ 10.32"
#	[1] "2007-09-11 08:00:00 AMP.AX -992 @ 10.1136"

#	[1] "2007-01-08 09:00:00 AMP.AX 999 @ 9.9"
#	[1] "2007-01-10 09:00:00 AMP.AX -999 @ 10.24"
#	[1] "2007-02-02 09:00:00 AMP.AX 989 @ 10.5"
#	[1] "2007-02-02 09:00:00 AMP.AX -989 @ 10.5"
#	[1] "2007-02-02 09:00:00 AMP.AX -999 @ 10.5"
#	[1] "2007-02-05 09:00:00 AMP.AX 999 @ 10.45"
#	[1] "2007-02-05 09:00:00 AMP.AX 996 @ 10.45"
#	[1] "2007-02-09 09:00:00 AMP.AX -996 @ 10.58"
#	[1] "2007-02-19 09:00:00 AMP.AX 991 @ 10.66"
#	[1] "2007-02-20 09:00:00 AMP.AX -990 @ 10.78"
#	[1] "2007-03-01 09:00:00 AMP.AX 1030 @ 10.42"
#	[1] "2007-03-01 09:00:00 AMP.AX -1030 @ 10.2116"
#	[1] "2007-03-05 09:00:00 AMP.AX 1019 @ 10.1"
#	[1] "2007-03-05 09:00:00 AMP.AX -1019 @ 9.898"
#	[1] "2007-03-06 09:00:00 AMP.AX 1036 @ 9.87"
#	[1] "2007-03-09 09:00:00 AMP.AX -1037 @ 10.04"
#	[1] "2007-03-12 09:00:00 AMP.AX 1041 @ 9.98"
#	[1] "2007-03-14 09:00:00 AMP.AX -1041 @ 10.05"
#	[1] "2007-03-15 09:00:00 AMP.AX 1046 @ 10.2"
#	[1] "2007-03-16 09:00:00 AMP.AX -1046 @ 10.24"
#	[1] "2007-03-19 09:00:00 AMP.AX 1043 @ 10.05"
#	[1] "2007-03-21 09:00:00 AMP.AX -1043 @ 10.31"
#	[1] "2007-04-03 08:00:00 AMP.AX 1059 @ 10.27"
#	[1] "2007-04-04 08:00:00 AMP.AX -1059 @ 10.55"
#	[1] "2007-05-01 08:00:00 AMP.AX 1030 @ 10.82"
#	[1] "2007-05-01 08:00:00 AMP.AX -1030 @ 10.82"
#	[1] "2007-05-01 08:00:00 AMP.AX -1059 @ 10.82"
#	[1] "2007-05-02 08:00:00 AMP.AX 1059 @ 10.84"
#	[1] "2007-05-02 08:00:00 AMP.AX 1031 @ 10.84"
#	[1] "2007-05-03 08:00:00 AMP.AX -1031 @ 10.89"
#	[1] "2007-05-07 08:00:00 AMP.AX 1028 @ 10.78"
#	[1] "2007-05-21 08:00:00 AMP.AX -1028 @ 10.38"
#	[1] "2007-05-22 08:00:00 AMP.AX 1028 @ 10.41"
#	[1] "2007-05-22 08:00:00 AMP.AX -1028 @ 10.2018"


#	   			AMP.AX.Open AMP.AX.High AMP.AX.Low AMP.AX.Close AMP.AX.Volume
#	2007-01-01       10.10       10.10      10.10        10.10             0
#	2007-01-02       10.18       10.20      10.09        10.09       1195500
#	2007-01-03       10.18       10.28      10.15        10.16       2235300
#	2007-01-04       10.19       10.25      10.07        10.07       4080500
#	2007-01-05       10.08       10.20       9.90        10.01       3926700
#	2007-01-08        9.90        9.99       9.90         9.90       5229400
#	2007-01-09       10.08       10.22      10.01        10.20       6057400
#	2007-01-10       10.24       10.32      10.07        10.07       4882100
#	2007-01-11       10.12       10.19       9.98        10.10       3835100
#	2007-01-12       10.13       10.22      10.08        10.21       2040900
#	2007-01-15       10.20       10.32      10.18        10.29       2503300
#	2007-01-16       10.30       10.48      10.26        10.37       5652900
#	2007-01-17       10.48       10.48      10.35        10.35       4694500
#	2007-01-18       10.42       10.43      10.28        10.31       3424800
#	2007-01-19       10.25       10.40      10.25        10.33       2783200
#	2007-01-22       10.37       10.47      10.37        10.41       2870300
#	2007-01-23       10.41       10.48      10.39        10.45       3293000
#	2007-01-24       10.48       10.60      10.46        10.58       5433100
#	2007-01-25       10.68       10.70      10.47        10.50       9106900
#	2007-01-26       10.50       10.50      10.50        10.50             0
#	2007-01-29       10.52       10.61      10.47        10.59       4186600
#	2007-01-30       10.63       10.68      10.56        10.68       4568800
#	2007-01-31       10.64       10.68      10.47        10.47       5962700
#	2007-02-01       10.52       10.56      10.38        10.45       4703100
#	2007-02-02       10.50       10.51      10.43        10.44       4987700
#	2007-02-05       10.45       10.47      10.35        10.37       4264900
#	2007-02-06       10.47       10.50      10.39        10.46       2372000
#	2007-02-07       10.55       10.56      10.46        10.47       3572000
#	2007-02-08       10.47       10.58      10.45        10.54       3057000
#	2007-02-09       10.58       10.67      10.50        10.63       4070700
#	2007-02-12       10.60       10.65      10.56        10.61       2580300
#	2007-02-13       10.63       10.80      10.62        10.78       5376700
#	2007-02-14       10.85       10.91      10.77        10.83       4588100
#	2007-02-15       10.80       10.82      10.62        10.70      11940700
#	2007-02-16       10.70       10.72      10.60        10.61       8005500
#	2007-02-19       10.66       10.74      10.64        10.69      10519100
#	2007-02-20       10.78       10.83      10.70        10.81       4431400
#	2007-02-21       10.78       10.81      10.67        10.67       7434300
#	2007-02-22       10.73       10.78      10.68        10.77       4550100
#	2007-02-23       10.78       10.78      10.71        10.72       8312500
#	2007-02-26       10.73       10.75      10.67        10.72       6591400
#	2007-02-27       10.71       10.80      10.70        10.74       5494900
#	2007-02-28       10.48       10.50      10.31        10.31      15831500
#	2007-03-01       10.42       10.47      10.22        10.23       9958600
#	2007-03-02       10.23       10.36      10.15        10.21       5643200
#	2007-03-05       10.10       10.15       9.84         9.85      11534900
#	2007-03-06        9.87       10.05       9.83        10.04      13360600
#	2007-03-07       10.20       10.38      10.16        10.38      18599700
#	2007-03-08       10.34       10.34      10.20        10.29      10575100
#	2007-03-09       10.04       10.17       9.98         9.98      15751200
#	2007-03-12        9.98       10.31       9.98        10.28       7651100
#	2007-03-13       10.32       10.35      10.13        10.21       4713300
#	2007-03-14       10.05       10.08       9.98        10.00       6307900
#	2007-03-15       10.20       10.25      10.08        10.25      10986200
#	2007-03-16       10.24       10.26      10.06        10.07       4120100
#	2007-03-19       10.05       10.18      10.03        10.12       3832900
#	2007-03-20       10.23       10.29      10.19        10.26       4512700
#	2007-03-21       10.31       10.36      10.16        10.16       4092100
#	2007-03-22       10.27       10.40      10.25        10.40       5489200
#	2007-03-23       10.37       10.39      10.29        10.37       3108600
#	2007-03-26       10.40       10.48      10.35        10.45       4732600
#	2007-03-27       10.43       10.45      10.37        10.43       3878600
#	2007-03-28       10.37       10.44      10.22        10.29       4909600
#	2007-03-29       10.29       10.35      10.20        10.35       3350200
#	2007-03-30       10.39       10.45      10.38        10.39       7426400
#	2007-04-02       10.39       10.40      10.17        10.17       2810700
#	2007-04-03       10.27       10.50      10.22        10.50       6293400
#	2007-04-04       10.55       10.61      10.50        10.60       8386000
#	2007-04-05       10.62       10.63      10.55        10.60       1986700
#	2007-04-06       10.63       10.63      10.63        10.63             0
#	2007-04-09       10.63       10.63      10.63        10.63             0
#	2007-04-10       10.70       10.76      10.63        10.74       5085700
#	2007-04-11       10.74       10.77      10.60        10.62       5422300
#	2007-04-12       10.61       10.68      10.56        10.64       5658400
#	2007-04-13       10.70       10.74      10.57        10.59       4637900
#	2007-04-16       10.63       10.79      10.61        10.75       3685600
#	2007-04-17       10.82       10.84      10.65        10.68       5451300
#	2007-04-18       10.69       10.89      10.68        10.83       5295500
#	2007-04-19       10.81       10.82      10.71        10.72       5537500
#	2007-04-20       10.77       10.89      10.72        10.84       4667200
#	2007-04-23       10.89       11.00      10.82        10.97       5087300
#	2007-04-24       10.97       11.00      10.78        10.89       4079000
#	2007-04-25       10.89       10.89      10.89        10.89             0
#	2007-04-26       10.91       11.02      10.91        10.95       4415900
#	2007-04-27       10.90       10.99      10.85        10.86       9716300
#	2007-04-30       10.87       10.94      10.75        10.75       5690900
#	2007-05-01       10.82       10.82      10.70        10.78       4686700
#	2007-05-02       10.84       10.89      10.76        10.89       4957900
#	2007-05-03       10.89       10.91      10.82        10.84       2948500
#	2007-05-04       10.80       10.90      10.75        10.80       4457200
#	2007-05-07       10.78       10.80      10.65        10.74       5906300
#	2007-05-08       10.78       10.78      10.66        10.68       9551400
#	2007-05-09       10.65       10.83      10.61        10.72       5695200
#	2007-05-10       10.70       10.72      10.59        10.62       7556000
#	2007-05-11       10.52       10.57      10.49        10.51       6930000
#	2007-05-14       10.65       10.76      10.64        10.71       5182300
#	2007-05-15       10.66       10.70      10.53        10.55       6103700
#	2007-05-16       10.58       10.64      10.47        10.55       6193400
#	2007-05-17       10.54       10.68      10.54        10.61      10104600
#	2007-05-18       10.62       10.68      10.58        10.66       5486800
#	2007-05-21       10.38       10.47      10.30        10.40       6738800
#	2007-05-22       10.41       10.44      10.23        10.27       5893400






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


