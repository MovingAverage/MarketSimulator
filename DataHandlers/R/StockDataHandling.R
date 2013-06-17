#'
#' 

require(quantmod)
require(FinancialInstrument)


downloadStocks <- function(symbols) {
	
	name <- as.character(substitute(symbols))
	symbols <- paste0(symbols, ".AX")
	.stocks <- new.env()
	
	downloadWithRetry <- function(symbols, retried = "None", env) {
		
		suppressWarnings(try(getSymbols(symbols, env = env)))
		successful.downloads <- ls(pos = env)
		still.to.download <- !symbols %in% successful.downloads
		
		if (any(still.to.download)) {
			still.to.download <- symbols[still.to.download]
			next.stock <- still.to.download[1]
			if (next.stock != retried) {
				message(paste("Failed attempt on '", next.stock, 
								"', retrying in 5s...", sep = ""))
				Sys.sleep(5)
				downloadWithRetry(still.to.download, retried = next.stock, env = env)
			} else {
				message(paste("Second failed attempt on '", next.stock, 
								"'; removing from list", sep = ""))
				downloadWithRetry(still.to.download[-1], env = env)
			}
		}
	}
	
	downloadWithRetry(symbols, env = .stocks)
	assign(".stocks", .stocks, pos = globalenv())
}

saveStocks <- function(save.location = "D:/Data/MarketData/Stocks", env) {
	
	for (stock in ls(pos = env)) {
		save(list = stock, 
				file = file.path(save.location, paste0(stock, ".RData")), envir = env)
	}
}

loadStocks <- function(stocks, location = "D:/Data/MarketData/Stocks", env) {
	
	files <- list.files(location)
	existing.files <- files[grep(paste(stocks, collapse = "|"), files)]
	if (length(existing.files) != length(stocks)) {
		existing.stocks <- sub("\\..+", "", existing.files)
		missing.stocks <- stocks[-grep(paste(existing.stocks, collapse = "|"), stocks)]
		warning(paste("Stocks not found: ", paste(missing.stocks, collapse = ", ")))
	}
	
	if (missing(env)) {
		env <- new.env()
		for (file in existing.files) {
			load(file.path(location, file), envir = env)
		}
		return(as.list(env))
	} else {
		for (file in existing.files) {
			load(file.path(location, file), envir = env)
		}
	}
}





