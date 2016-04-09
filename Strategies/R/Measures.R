#'
#'

setClass("Measurement",
		representation(
			measures = "xts"
		))
		
Measure <- function(indicator, market, ...) {
	
	measure <- new("Measurement")
	measurements <- xts()
	for (instrument in tradeableInstruments(market)) {
		measurements <- merge(measurements, indicator(market[instrument], ...))
	}
	names(measurements) <- tradeableInstruments(market)
	measure@measures <- measurements
	return(measure)
}

measures <- function(measurement) {
	measurement@measures
}





















