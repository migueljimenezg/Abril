datos = read.csv("Cuatro acciones 2020.csv", sep=";", dec=",", header = T)

precios = datos[,-1]

nombres = colnames(precios)
nombres

acciones = ncol(precios)
acciones

precios = ts(precios)

rendimientos = diff(log(precios))

correlacion = cor(rendimientos)
correlacion

rendimientos_esperados = apply(rendimientos, 2, mean)
rendimientos_esperados

barplot(rendimientos_esperados, horiz = T, main="Rendimientos esperados de las acciones")

volatilidades = apply(rendimientos, 2, sd)
volatilidades

barplot(volatilidades,horiz = T, main="Volatilidades de las acciones")

s = tail(precios, 1)
s = as.numeric(s)
s

numero_acciones = c(150000,300000,40000,70000)
numero_acciones

valor_mercado_acciones = numero_acciones*s
valor_mercado_acciones

valor_portafolio = sum(valor_mercado_acciones)
valor_portafolio

proporciones = valor_mercado_acciones/valor_portafolio
proporciones

sum(proporciones)

pie(proporciones, nombres, main="Proporciones de inversión portafolio N° 1", col=c("darkgreen","darkblue","darkgray","darkred"))

rendimientos_portafolio = vector()

for(i in 1:nrow(rendimientos)){
    
  rendimientos_portafolio[i] = sum(rendimientos[i,]*proporciones)
  
}

head(rendimientos_portafolio, 10)

rendimiento_esperado_portafolio = mean(rendimientos_portafolio)
rendimiento_esperado_portafolio

volatilidad_portafolio = sd(rendimientos_portafolio)
volatilidad_portafolio

barplot(c(rendimientos_esperados, rendimiento_esperado_portafolio), horiz = T, main = "Rendimientos esperados de las acciones\n y del portafolio de inversión N° 1")

barplot(c(volatilidades,volatilidad_portafolio), horiz = T, main = "Volatilidades de las acciones\n y del portafolio de inversión N° 1")

proporciones = c(0.20,0.30,0.15,0.35)
proporciones

sum(proporciones)

pie(proporciones, nombres, main="Proporciones de inversión", col = c("darkgreen","darkblue","darkgray","darkred"))

rendimientos_portafolio=vector()

for(i in 1:nrow(rendimientos)){
    
  rendimientos_portafolio[i] = sum(rendimientos[i,]*proporciones)
  
}

rendimiento_esperado_portafolio = mean(rendimientos_portafolio)
rendimiento_esperado_portafolio

volatilidad_portafolio = sd(rendimientos_portafolio)
volatilidad_portafolio

barplot(c(rendimientos_esperados,rendimiento_esperado_portafolio), horiz = T, main="Rendimientos esperados de las acciones\n y del portafolio de inversión N° 2")

barplot(c(volatilidades,volatilidad_portafolio), horiz = T, main="Volatilidades de las acciones\n y del portafolio de inversión N° 2")
