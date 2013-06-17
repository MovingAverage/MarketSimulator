#'
#'

setClass("Broker",
		representation(
			market = "list"
		))
		
addMarket <- function(broker, market) {
			broker@market <- market
			return(broker)
		}

getBar <- function(broker, instrument, timestamp) {
	instrument <- getMarketInstrument(broker@market, instrument)
	return(instrument[timestamp])
	
}
