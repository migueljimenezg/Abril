datos = read.csv("Cuatro acciones 2020.csv", sep = ";", dec = ",", header = T)

head(datos)

precios = datos[,-1]

head(precios)

nombres = colnames(precios)
nombres

acciones = ncol(precios)
acciones

precios = ts(precios)

plot(precios, t = "l", xlab = "Tiempo")

rendimientos <- matrix(0, nrow(precios) -1, acciones)

for(i in 1:acciones){
    
  rendimientos[,i] = diff(log(precios[,i]))
    
}

rendimientos = diff(log(precios))

head(rendimientos)

dim(precios)

dim(rendimientos)

plot(rendimientos[,1], t = "h", xlab = "Tiempo", ylab = "Rendimientos", main = nombres[1])

plot(rendimientos[,2], t = "h", xlab = "Tiempo", ylab = "Rendimientos", main = nombres[2])

plot(rendimientos[,3], t = "h", xlab = "Tiempo", ylab = "Rendimientos", main = nombres[3])

plot(rendimientos[,4], t = "h", xlab = "Tiempo", ylab = "Rendimientos", main = nombres[4])

layout(matrix(c(1:4),nrow=2,byrow=F))
layout.show(4)

layout(matrix(c(1:4), nrow = 2, byro w= F))

for(i in 1:acciones){
    
    plot(rendimientos[,i], t = "h", xlab = "Tiempo", ylab = "Rendimientos", main = nombres[i])
}

summary(rendimientos)

library(fBasics)

basicStats(rendimientos)

rendimientos_esperados = vector()

for(i in 1:acciones){
    
    rendimientos_esperados[i] = mean(rendimientos[,i])
    
}
rendimientos_esperados

rendimientos_esperados = apply(rendimientos, 2, mean)
rendimientos_esperados

volatilidades = vector()

for(i in 1:acciones){
    
    volatilidades[i ]= sd(rendimientos[,i])
    
}
volatilidades

volatilidades = apply(rendimientos, 2, sd)
volatilidades

layout(matrix(c(1:4), nrow = 2, byrow = F))

for(i in 1:acciones){
    
    hist(rendimientos[,i], breaks = 60, col = "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = nombres[i], freq = F)
}

layout(matrix(c(1:4), nrow = 2, byrow = F))

for(i in 1:acciones){
    
    hist(rendimientos[,i], breaks = 60, col = "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = nombres[i], freq = F)
    curve(dnorm(x, mean = rendimientos_esperados[i], sd = volatilidades[i]), add = T, lwd = 3)
}

layout(matrix(c(1:4), nrow = 2, byrow = F))

for(i in 1:acciones){
    
    hist(rendimientos[,i], breaks = 60, col = "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = nombres[i], freq = F)
    lines(density(rendimientos[,i]), lwd = 3, col = "darkgreen")
}

layout(matrix(c(1:4), nrow = 2, byrow = F))

for(i in 1:acciones){
    
    hist(rendimientos[,i], breaks = 60, col = "gray", xlab = "Rendimientos", ylab = "Frecuencia", main = nombres[i], freq = F)
    curve(dnorm(x, mean = rendimientos_esperados[i], sd = volatilidades[i]), add = T, lwd = 3)
    lines(density(rendimientos[,i]), lwd = 3, col = "darkgreen")
    legend("topleft", c("Distribución Normal", "Distribución empírica"), lty = c(1,1), lwd = c(3,3), col = c("black", "darkgreen"), bty = "n")
}

layout(matrix(c(1:4), nrow = 2, byrow = F))

for(i in 1:acciones){
    
    qqnorm(rendimientos[,i], main = nombres[i])
    qqline(rendimientos[,i])
}

covarianza = cov(rendimientos)
covarianza

correlacion = cor(rendimientos)
correlacion
