volatilidad_portafolio = sqrt(0.7^2*0.02^2 + 0.3^2*0.05^2 + 2*0.7*0.3*0.02*0.05*0.67)
volatilidad_portafolio

volatilidad_portafolio = sqrt(0.30^2*0.02^2 + 0.70^2*0.05^2 + 2*0.30*0.70*0.02*0.05*0.67)
volatilidad_portafolio

volatilidad_portafolio = sqrt(0.20^2*0.02^2 + 0.50^2*0.04^2 + 0.30^2*0.032^2 + 2*0.20*0.50*0.02*0.04*0.52 + 2*0.20*0.30*0.02*0.032*0.42 + 2*0.50*0.30*0.04*0.032)
volatilidad_portafolio

proporciones = c(0.70, 0.30)
proporciones

vector_1 = c(0.0010196809, 0.0005939468)

vector_2 = c(0.0005939468, 0.0008155434)

covarianzas = cbind(vector_1, vector_2)
covarianzas

volatilidad_portafolio = sqrt(sum(proporciones*covarianzas*t(proporciones)))
volatilidad_portafolio

datos = read.csv("Cuatro acciones 2020.csv", sep = ";", dec = ",", header = T)

precios = datos[,-1]

precios = ts(precios)

nombres = colnames(precios)
nombres

rendimientos = diff(log(precios))

volatilidades = apply(rendimientos, 2, sd)
volatilidades

covarianzas = cov(rendimientos)
covarianzas

proporciones = c(0.20, 0.30, 0.40, 0.10)
proporciones

volatilidad_portafolio = sqrt(sum(proporciones*covarianzas*t(proporciones)))
volatilidad_portafolio

volatilidad_portafolio = sqrt(sum(proporciones%*%covarianzas*t(proporciones)))
volatilidad_portafolio

rendimientos_portafolio = vector()

for(i in 1:nrow(rendimientos)){
    
  rendimientos_portafolio[i] = sum(rendimientos[i,]*proporciones)
}

volatilidad_portafolio = sd(rendimientos_portafolio)
volatilidad_portafolio
