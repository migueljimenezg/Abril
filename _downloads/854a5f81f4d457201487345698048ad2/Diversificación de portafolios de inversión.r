library(quantmod)
library(tseries)

NFLX = get.hist.quote(instrument = "NFLX", start = as.Date("2018-04-16"), end= as.Date("2020-04-15"), quote = "AdjClose")

AAPL = get.hist.quote(instrument = "AAPL", start = as.Date("2018-04-16"), end= as.Date("2020-04-15"), quote = "AdjClose", provider = c("yahoo"))

precios = merge(NFLX, AAPL)

precios = ts(precios)

dim(precios)

plot(precios, xlab = "Tiempo", main = "Precios")

rendimientos = diff(log(precios))

plot(rendimientos, xlab = "Tiempo", main = "Rendimientos")

rendimientos_esperados = apply(rendimientos, 2, mean)
rendimientos_esperados

volatilidades = apply(rendimientos, 2, sd)
volatilidades

correlacion = cor(rendimientos)
correlacion

covarianzas = cov(rendimientos)
covarianzas

proporciones_NFLX = seq(0, 1, 0.05)
proporciones_AAPL = seq(1, 0, -0.05)

proporciones = cbind(proporciones_NFLX, proporciones_AAPL)
proporciones

dim(proporciones)

dim(t(proporciones[2,]))

volatilidad_portafolio = vector()

for(i in 1: nrow(proporciones)){
    
    volatilidad_portafolio[i] = sqrt(sum(proporciones[i,]%*%covarianzas*t(proporciones[i,])))
}

volatilidad_portafolio

rendimientos_portafolio = matrix(, nrow(rendimientos), nrow(proporciones))

for(j in 1:nrow(proporciones)){
  
  for(i in 1:nrow(rendimientos)){
    
    rendimientos_portafolio[i,j] = sum(rendimientos[i,]*proporciones[j,])
    
}}

dim(rendimientos_portafolio)

rendimiento_esperado_portafolio = apply(rendimientos_portafolio, 2, mean)
rendimiento_esperado_portafolio

plot(volatilidad_portafolio, rendimiento_esperado_portafolio, xlab = "Volatilidad portafolio", ylab = "Rendimiento esperado portafolio")

plot(volatilidad_portafolio, rendimiento_esperado_portafolio, xlab = "Volatilidad portafolio", ylab = "Rendimiento esperado portafolio")
points(volatilidad_portafolio[1], rendimiento_esperado_portafolio[1], col = "darkgreen", lwd = 5)
points(volatilidad_portafolio[21], rendimiento_esperado_portafolio[21], col = "darkblue", lwd = 5)
points(volatilidad_portafolio[7], rendimiento_esperado_portafolio[7], col = "black", lwd = 5)

min(volatilidad_portafolio)
