library(quantmod)
library(tseries)

FB = get.hist.quote(instrument = "FB", start = as.Date("2018-04-01"), end = as.Date("2020-04-22"), quote = "AdjClose")

KO = get.hist.quote(instrument = "KO", start = as.Date("2018-04-01"), end = as.Date("2020-04-22"), quote = "AdjClose")

K = get.hist.quote(instrument = "K", start = as.Date("2018-04-01"), end = as.Date("2020-04-22"), quote = "AdjClose")

F = get.hist.quote(instrument = "F", start = as.Date("2018-04-01"), end = as.Date("2020-04-22"), quote = "AdjClose")

MCD = get.hist.quote(instrument = "MCD", start = as.Date("2018-04-01"), end = as.Date("2020-04-22"), quote = "AdjClose")

precios = merge(FB, KO, K, F, MCD)

precios = ts(precios)

head(precios)

dim(precios)

acciones = ncol(precios)
acciones

rendimientos = diff(log(precios))

dim(rendimientos)

s = tail(precios,1)
s = as.numeric(s)
s

numero_acciones = c(2000, 5000, 2000, 10000, 1000)
numero_acciones

valor_mercado_acciones = numero_acciones*s
valor_mercado_acciones

valor_portafolio = sum(valor_mercado_acciones)
valor_portafolio

proporciones = valor_mercado_acciones/valor_portafolio
proporciones

sum(proporciones)

rendimientos_esperados = apply(rendimientos,2,mean)
rendimientos_esperados

volatilidades = apply(rendimientos,2,sd)
volatilidades

covarianzas = cov(rendimientos)
covarianzas

correlaciones = cor(rendimientos)
correlaciones

rendimientos_portafolio=vector()

for(i in 1:nrow(rendimientos)){
    
  rendimientos_portafolio[i] = sum(rendimientos[i,]*proporciones)
  
}

rendimiento_esperado_portafolio = mean(rendimientos_portafolio)
rendimiento_esperado_portafolio

volatilidad_portafolio = sd(rendimientos_portafolio)
volatilidad_portafolio

volatilidad_portafolio = sqrt(sum(t(proporciones)%*%covarianzas*proporciones))
volatilidad_portafolio

NC = 0.95
t = 5

VaR_individuales_sin_promedios = valor_mercado_acciones*volatilidades*qnorm(NC)*sqrt(t)
VaR_individuales_sin_promedios[1]

NC = 0.95
t = 5

VaR_individuales_sin_promedios = valor_mercado_acciones*volatilidades*qnorm(NC)*sqrt(t)
VaR_individuales_sin_promedios[4]

NC = 0.99
t = 5

VaR_individuales_sin_promedios = valor_mercado_acciones*volatilidades*qnorm(NC)*sqrt(t)
VaR_individuales_sin_promedios[5]

NC = 0.99
t = 5

VaR_individuales_sin_promedios = valor_mercado_acciones*volatilidades*qnorm(NC)*sqrt(t)
VaR_individuales_sin_promedios[3]

NC = 0.99
t = 5

VaR_portafolio_sin_promedios = sqrt(sum(t(VaR_individuales_sin_promedios)%*%correlaciones*VaR_individuales_sin_promedios))
VaR_portafolio_sin_promedios

NC = 0.99
t = 1

VaR_individuales_sin_promedios = valor_mercado_acciones*volatilidades*qnorm(NC)*sqrt(t)
VaR_individuales_sin_promedios[1]

NC = 0.99
t = 1

VaR_individuales_sin_promedios = valor_mercado_acciones*volatilidades*qnorm(NC)*sqrt(t)
VaR_individuales_sin_promedios[2]

NC = 0.975
t = 1

VaR_portafolio_sin_promedios = sqrt(sum(t(VaR_individuales_sin_promedios)%*%correlaciones*VaR_individuales_sin_promedios))
VaR_portafolio_sin_promedios

NC = 0.99
t = 5

VaR_individuales_con_promedios = valor_mercado_acciones*abs(rendimientos_esperados*t+qnorm(1-NC,sd=volatilidades*sqrt(t)))
VaR_individuales_con_promedios[2]

NC = 0.95
t = 20

VaR_individuales_con_promedios = valor_mercado_acciones*abs(rendimientos_esperados*t+qnorm(1-NC,sd=volatilidades*sqrt(t)))
VaR_individuales_con_promedios[4]

NC = 0.99
t = 20

VaR_portafolio_con_promedios = sqrt(sum(t(VaR_individuales_con_promedios)%*%correlaciones*VaR_individuales_con_promedios))
VaR_portafolio_con_promedios

NC = 0.98
t = 20

VaR_portafolio_con_promedios = sqrt(sum(t(VaR_individuales_con_promedios)%*%correlaciones*VaR_individuales_con_promedios))
VaR_portafolio_con_promedios

NC = 0.99
t = 1

VaR_individuales_sin_promedios = valor_mercado_acciones*volatilidades*qnorm(NC)*sqrt(t)

VaR_portafolio_sin_promedios = sqrt(sum(t(VaR_individuales_sin_promedios)%*%correlaciones*VaR_individuales_sin_promedios))

suma_VaR_individuales_sin_promedios = sum(VaR_individuales_sin_promedios)

BD_sin_promedios=suma_VaR_individuales_sin_promedios-VaR_portafolio_sin_promedios
BD_sin_promedios

NC = 0.99
t = 1

VaR_individuales_con_promedios = valor_mercado_acciones*abs(rendimientos_esperados*t+qnorm(1-NC,sd=volatilidades*sqrt(t)))

VaR_portafolio_con_promedios = sqrt(sum(t(VaR_individuales_con_promedios)%*%correlaciones*VaR_individuales_con_promedios))

suma_VaR_individuales_con_promedios = sum(VaR_individuales_con_promedios)

BD_con_promedios = suma_VaR_individuales_con_promedios-VaR_portafolio_con_promedios
BD_con_promedios

plot(precios, col = "darkblue", lwd = 2, xlab = "Tiempo", main = "Precios")

plot(rendimientos, col = "darkblue", lwd = 2, xlab = "Tiempo", main = "Rendimientos")
