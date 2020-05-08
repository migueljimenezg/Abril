datos = read.csv("TRM.csv", sep = ";", dec = ",", header = T)

precios = datos[,-1]

precios = ts(precios)

s = tail(precios,1)
s = as.numeric(s)
s

rendimientos = diff(log(precios))

rendimiento_esperado = mean(rendimientos)
rendimiento_esperado

volatilidad = sd(rendimientos)
volatilidad

plot(precios, col = "darkblue", lwd = 2, main = "Precios")

plot(rendimientos, col = "darkblue", lwd = 2, main = "Rendimientos")

hist(rendimientos, breaks = 40, col= "gray", border = "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = "TRM", freq = F)
curve(dnorm(x, mean = 0, sd = volatilidad), add = T, lwd = 4)

NC = 0.99
t = 10

VaR_sin_promedios_porcentaje = volatilidad*qnorm(NC)*sqrt(t)
VaR_sin_promedios_porcentaje

VaR_sin_promedios = s*volatilidad*qnorm(NC)*sqrt(t)
VaR_sin_promedios

NC = 0.99
t = 10

VaR_con_promedios_porcentaje = abs(rendimiento_esperado*t+qnorm(1-NC,sd=volatilidad*sqrt(t)))
VaR_con_promedios_porcentaje

VaR_con_promedios = s*abs(rendimiento_esperado*t+qnorm(1-NC,sd=volatilidad*sqrt(t)))
VaR_con_promedios

hist(rendimientos, breaks = 40, col= "gray", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = "TRM", freq = F, xlim = c(-0.055, 0.055))
curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(t)), add = T, lwd = 3)
abline(v = - VaR_sin_promedios_porcentaje, col = "darkblue", lwd = 3)

hist(rendimientos, breaks = 40, col= "gray", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = "TRM", freq = F, xlim = c(-0.055, 0.055))
curve(dnorm(x, mean = rendimiento_esperado*t, sd = volatilidad*sqrt(t)), add = T, lwd = 3)
abline(v = - VaR_con_promedios_porcentaje, col = "darkgreen", lwd = 3)

hist(rendimientos, breaks = 40, col= "gray", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = "TRM", freq = F, xlim = c(-0.055, 0.055))
curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(t)), add = T, lwd = 3, col = "darkred")
curve(dnorm(x, mean = rendimiento_esperado*t, sd = volatilidad*sqrt(t)), add = T, lwd = 3)
abline(v = - VaR_sin_promedios_porcentaje, col = "darkblue", lwd = 3)
abline(v = - VaR_con_promedios_porcentaje, col = "darkgreen", lwd = 3)
legend("topright", c("VaR sin promedios", "VaR con promedios", "Normal media cero", "Normal"), lty = c(1,1,1,1), lwd = 3, col = c("darkblue", "darkgreen", "darkred", "black"), bty = "n")
