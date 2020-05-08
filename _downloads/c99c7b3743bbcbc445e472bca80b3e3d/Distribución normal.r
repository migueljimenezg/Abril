z = qnorm(0.01)
z

z = qnorm(0.99)
z

hist(1,col= "white", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = NULL, freq = T, ylim = c(0, 0.4), xlim = c(-5,5))
curve(dnorm(x, mean = 0, sd = 1), add = T, lwd = 3, col = "#3b5998")
abline(v = 0, lwd = 2, lty = 1)
abline(v = qnorm(0.01), lwd = 2, lty = 1)
abline(v = qnorm(0.99), lwd = 2, lty = 1)

z = qnorm(0.05)
z

z = qnorm(0.95)
z

hist(1,col= "white", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = NULL, freq = T, ylim = c(0, 0.4), xlim = c(-5,5))
curve(dnorm(x, mean = 0, sd = 1), add = T, lwd = 3, col = "#3b5998")
abline(v = 0, lwd = 2, lty = 1)
abline(v = qnorm(0.05), lwd = 2, lty = 1)
abline(v = qnorm(0.95), lwd = 2, lty = 1)

mu = 0.01
volatilidad = 0.05

x = qnorm(0.01, mean = mu, sd = volatilidad)
x

x = mu + qnorm(0.01, sd = volatilidad)
x

x = qnorm(0.99, mean = mu, sd = volatilidad)
x

x = mu + qnorm(0.99, sd = volatilidad)
x

hist(1,col= "white", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = NULL, freq = T, ylim = c(0, 8), xlim = c(-0.3,0.3))
curve(dnorm(x, mean = mu, sd = volatilidad), add = T, lwd = 3, col = "#3b5998")
abline(v = 0, lwd = 2, lty = 1)
abline(v = qnorm(0.01, mean = mu, sd = volatilidad), lwd = 2, lty = 1)
abline(v = qnorm(0.99, mean = mu, sd = volatilidad), lwd = 2, lty = 1)

mu = -0.01
volatilidad = 0.05

x = qnorm(0.01, mean = mu, sd = volatilidad)
x

x = mu + qnorm(0.01, sd = volatilidad)
x

x = qnorm(0.99, mean = mu, sd = volatilidad)
x

x = mu + qnorm(0.99, sd = volatilidad)
x

hist(1,col= "white", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = NULL, freq = T, ylim = c(0, 8), xlim = c(-0.3,0.3))
curve(dnorm(x, mean = mu, sd = volatilidad), add = T, lwd = 3, col = "#3b5998")
abline(v = 0, lwd = 2, lty = 1)
abline(v = qnorm(0.01, mean = mu, sd = volatilidad), lwd = 2, lty = 1)
abline(v = qnorm(0.99, mean = mu, sd = volatilidad), lwd = 2, lty = 1)

datos = read.csv("COLCAP.csv", sep = ";", dec = ",", header = T)

precios = datos[,-1]

precios = ts(precios)

rendimientos = diff(log(precios))

rendimiento_esperado = mean(rendimientos)
rendimiento_esperado

volatilidad = sd(rendimientos)
volatilidad

plot(precios, xlab = "Tiempo", ylab = "Puntos", col = "#3b5998", lwd = 3)

plot(rendimientos, t = "h", xlab = "Tiempo", ylab = "Rendimientos", col = "#3b5998", lwd = 2)

hist(rendimientos, breaks = 60, col= "#3b5998", xlab = "Rendimientos", ylab = "Frecuencia", main = "Índice COLCAP", freq = F)

hist(rendimientos, breaks = 60, col= "gray", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = "Índice COLCAP", freq = F)
curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(1)), add = T, lwd = 3, col = "#3b5998")

hist(rendimientos,col= "white", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = NULL, freq = F, ylim = c(0, 30))
curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(1)), add = T, lwd = 3, col = "#3b5998")
curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(5)), add = T, lwd = 3, col = "firebrick3")
curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(10)), add = T, lwd = 3, col = "forestgreen")
curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(20)), add = T, lwd = 3)
legend("topright", c("Diaria", "Semanal", "15 días", "Mensual"), col = c("#3b5998", "firebrick3", "forestgreen", "black"), lty = c(1,1,1,1), lwd = 3, bty = "n")

hist(rendimientos,col= "white", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = NULL, freq = F, ylim = c(0, 30))
curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(1)), add = T, lwd = 3, col = "#3b5998")
curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(5)), add = T, lwd = 3, col = "firebrick3")
curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(10)), add = T, lwd = 3, col = "forestgreen")
curve(dnorm(x, mean = 0, sd = volatilidad*sqrt(20)), add = T, lwd = 3)
abline(v = -volatilidad*qnorm(0.99)*sqrt(1), lwd = 3, col = "#3b5998")
abline(v = -volatilidad*qnorm(0.99)*sqrt(5), lwd = 3, col = "firebrick3")
abline(v = -volatilidad*qnorm(0.99)*sqrt(10), lwd = 3, col = "forestgreen")
abline(v = -volatilidad*qnorm(0.99)*sqrt(20), lwd = 3)
legend("topright", c("Diaria", "Semanal", "15 días", "Mensual"), col = c("#3b5998", "firebrick3", "forestgreen", "black"), lty = c(1,1,1,1), lwd = 3, bty = "n")

hist(rendimientos,col= "white", border = "white", xlab = "Rendimientos", ylab = "Frecuencia", main = NULL, freq = F, ylim = c(0, 30))
curve(dnorm(x, mean = rendimiento_esperado*1, sd = volatilidad*sqrt(1)), add = T, lwd = 3, col = "#3b5998")
curve(dnorm(x, mean = rendimiento_esperado*5, sd = volatilidad*sqrt(5)), add = T, lwd = 3, col = "firebrick3")
curve(dnorm(x, mean = rendimiento_esperado*10, sd = volatilidad*sqrt(10)), add = T, lwd = 3, col = "forestgreen")
curve(dnorm(x, mean = rendimiento_esperado*20, sd = volatilidad*sqrt(20)), add = T, lwd = 3)
abline(v = rendimiento_esperado*20, lwd = 3, lty = 1)
legend("topright", c("Diaria", "Semanal", "15 días", "Mensual"), col = c("#3b5998", "firebrick3", "forestgreen", "black"), lty = c(1,1,1,1), lwd = 3, bty = "n")
