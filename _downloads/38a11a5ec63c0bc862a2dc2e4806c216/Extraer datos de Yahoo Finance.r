library(quantmod)

getSymbols(c("NFLX","AAPL"), from = "2015-01-01", to = "2020-04-15")

head(NFLX)

head(AAPL)

getSymbols(c("NFLX","AAPL"), from="2015-01-01", to = Sys.Date())

precios = merge( NFLX[,6], AAPL[,6])

precios = ts(precios)

head(precios)

dim(precios)

plot(precios, main = "Precios")

rendimientos = diff(log(precios))

head(rendimientos)

dim(rendimientos)

head(rendimientos)

plot(rendimientos, main = "Rendimientos")

library(quantmod)

library(tseries)

NFLX = get.hist.quote(instrument = "NFLX", start = as.Date("2015-01-01"), end= as.Date("2020-04-15"), quote = "AdjClose")

head(NFLX)

AAPL = get.hist.quote(instrument = "AAPL", start = as.Date("2015-01-01"), end= as.Date("2020-04-15"), quote = "AdjClose", provider = c("yahoo"))

head(AAPL)

NFLX = get.hist.quote(instrument = "NFLX", start = as.Date("2015-01-01"), end = Sys.Date(), quote = "AdjClose")

AAPL = get.hist.quote(instrument = "AAPL", start = as.Date("2015-01-01"), end = Sys.Date(), quote = "AdjClose")

precios = merge( NFLX, AAPL)

precios = ts(precios)

head(precios)

dim(precios)

plot(precios, main = "Precios")

rendimientos = diff(log(precios))

head(rendimientos)

dim(rendimientos)

plot(rendimientos, main = "Rendimientos")
