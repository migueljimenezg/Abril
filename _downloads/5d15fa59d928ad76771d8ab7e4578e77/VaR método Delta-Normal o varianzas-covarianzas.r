datos = read.csv("Cuatro acciones 2020.csv", sep = ";", dec = ",", header = T)

precios = datos[,-1]

head(precios)

precios = ts(precios)

dim(precios)

nombres = colnames(precios)
nombres

acciones = ncol(precios)
acciones

rendimientos = diff(log(precios))

dim(rendimientos)

s = tail(precios,1)
s = as.numeric(s)
s

numero_acciones = c(180000,5000,12000,9000)
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

correlacion = cor(rendimientos)
correlacion

rendimientos_portafolio = vector()

for(i in 1:nrow(rendimientos)){
    
  rendimientos_portafolio[i] = sum(rendimientos[i,]*proporciones)
  
}

rendimiento_esperado_portafolio = mean(rendimientos_portafolio)
rendimiento_esperado_portafolio

volatilidad_portafolio = sd(rendimientos_portafolio)
volatilidad_portafolio

volatilidad_portafolio = sqrt(sum(t(proporciones)%*%covarianzas*proporciones))
volatilidad_portafolio

NC = 0.99
t = 10

VaR_individuales_sin_promedios_porcentaje = volatilidades*qnorm(NC)*sqrt(t)
VaR_individuales_sin_promedios_porcentaje

VaR_individuales_sin_promedios = valor_mercado_acciones*volatilidades*qnorm(NC)*sqrt(t)
VaR_individuales_sin_promedios

VaR_portafolio_sin_promedios = sqrt(sum(t(VaR_individuales_sin_promedios)%*%correlacion*VaR_individuales_sin_promedios))
VaR_portafolio_sin_promedios

VaR_portafolio_sin_promedios_porcentaje = VaR_portafolio_sin_promedios/valor_portafolio
VaR_portafolio_sin_promedios_porcentaje

suma_VaR_individuales_sin_promedios = sum(VaR_individuales_sin_promedios)
suma_VaR_individuales_sin_promedios

BD_sin_promedios = suma_VaR_individuales_sin_promedios - VaR_portafolio_sin_promedios
BD_sin_promedios

NC = 0.99
t = 10

VaR_individuales_con_promedios_porcentaje = abs(rendimientos_esperados*t+qnorm(1-NC, sd = volatilidades*sqrt(t)))
VaR_individuales_con_promedios_porcentaje

VaR_individuales_con_promedios = valor_mercado_acciones*abs(rendimientos_esperados*t+qnorm(1-NC, sd = volatilidades*sqrt(t)))
VaR_individuales_con_promedios

VaR_portafolio_con_promedios = sqrt(sum(t(VaR_individuales_con_promedios)%*%correlacion*VaR_individuales_con_promedios))
VaR_portafolio_con_promedios

VaR_portafolio_con_promedios_porcentaje = VaR_portafolio_con_promedios/valor_portafolio
VaR_portafolio_con_promedios_porcentaje

suma_VaR_individuales_con_promedios = sum(VaR_individuales_con_promedios)
suma_VaR_individuales_con_promedios

BD_con_promedios = suma_VaR_individuales_con_promedios - VaR_portafolio_con_promedios
BD_con_promedios

vol_portafolio = sqrt(sum(t(proporciones)%*%covarianzas*proporciones))
vol_portafolio

VaR_portafolio_sin_promedios = valor_portafolio*vol_portafolio*qnorm(NC)*sqrt(t)
VaR_portafolio_sin_promedios

plot(precios, col = "darkblue", lwd = 2, main = "Precios")

plot(rendimientos, col = "darkblue", lwd = 2, main = "Rendimientos")

layout(matrix(c(1:4), nrow = 2, byrow = T))
# layout.show(4)  correr esta línea en RStudio.

for(i in 1:acciones){
  
  hist(rendimientos[,i], breaks = 60, col= "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = nombres[i], freq = F, xlim = c(-0.5, 0.5))
  curve(dnorm(x, mean = 0, sd = volatilidades[i]*sqrt(t)), add = T, lwd = 3)
  abline(v = - VaR_individuales_sin_promedios_porcentaje[i], col = "darkred", lwd = 2)
    
}

hist(rendimientos_portafolio, breaks = 60, col = "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = "Portafolio de inversión", freq=F, xlim = c(-0.3, 0.3))
curve(dnorm(x, mean = 0, sd = volatilidad_portafolio*sqrt(t)), add = T, lwd = 3)
abline(v = -VaR_portafolio_con_promedios_porcentaje, col = "darkgreen", lwd = 4)

hist(rendimientos_portafolio, breaks = 60, col = "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = "", freq=F, xlim = c(-0.3,0.1))
for(i in 1:acciones){
    
abline(v = - VaR_individuales_sin_promedios_porcentaje[i], col = i, lwd = 2)
    
}
abline(v = -VaR_portafolio_sin_promedios_porcentaje, col = "darkgreen", lwd = 4)
legend("topleft", nombres, lty = rep(1, times = acciones), lwd = rep(2, times = acciones), col = seq(1, acciones), bty = "n")

layout(matrix(c(1:4), nrow = 2, byrow = T))
# layout.show(4)  correr esta línea en RStudio.

for(i in 1:acciones){
  
  hist(rendimientos[,i], breaks = 60, col= "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = nombres[i], freq = F, xlim = c(-0.5, 0.5))
  curve(dnorm(x, mean = rendimientos_esperados[i]*t, sd = volatilidades[i]*sqrt(t)), add = T, lwd = 3)
  abline(v = - VaR_individuales_con_promedios_porcentaje[i], col = "darkred", lwd = 2)
    
}

hist(rendimientos_portafolio, breaks = 60, col = "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = "Portafolio de inversión", freq=F, xlim = c(-0.5, 0.5))
curve(dnorm(x, mean = rendimiento_esperado_portafolio*t, sd = volatilidad_portafolio*sqrt(t)), add = T, lwd = 3)
abline(v = -VaR_portafolio_con_promedios_porcentaje, col = "darkgreen", lwd = 4)

hist(rendimientos_portafolio, breaks = 60, col = "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = "", freq=F, xlim = c(-0.3,0.1))
for(i in 1:acciones){
    
abline(v = - VaR_individuales_con_promedios_porcentaje[i], col = i, lwd = 2)
    
}
abline(v = -VaR_portafolio_con_promedios_porcentaje, col = "darkgreen", lwd = 4)
legend("topleft", nombres, lty = rep(1, times = acciones), lwd = rep(2, times = acciones), col = seq(1, acciones), bty = "n")
