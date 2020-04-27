vector <- c(1,2,3,4,5,6,7,8)
vector
View(vector)

vector <- c(1:8)
vector

vector[8]

vector <- c(1:8)*2
vector

a <- vector

b <- vector[2]

matriz <- matrix(1:6, nrow = 3)
matriz

matriz <- matrix(1:6, ncol = 3)
matriz

View(matriz)

ncol(matriz)

nrow(matriz)

dim(matriz)

length(matriz)

View(matriz)

matriz[2,2]

matriz[1,2]

# matriz[filas,columnas]

matriz[,2]

matriz[2,]

matriz <- matrix(1:20, ncol = 5)
View(matriz)

matriz[1,3:5]

matriz[2:3,]

matriz[-1,]

matriz <- matriz[-1,]

c <- matriz[,-3]
c

head(vector,1)

tail(vector)

tail(vector,4)

tail(vector,1)

matriz <- matrix(1:100, nrow = 20)

head(matriz)

head(matriz,2)

head(matriz[,1])

head(matriz[,3:5])

tail(matriz)

tail(matriz[,4])

max(vector)

min(vector)

max(matriz)

min(matriz)

max(matriz[5,])

sum(vector)

sum(matriz)

sum(tail(matriz))

mean(vector)

mean(matriz)

mean(matriz[,1:4])

apply(matriz,2,mean)

apply(matriz,2,sum)

sort(matriz, decreasing = T)

View(sort(matriz, decreasing = T))

sort(matriz[,1], decreasing = T)


ifelse(vector[1] < 3, 1, 0)

ifelse(vector[1] < 3, "Si", "No")

ifelse(mean(matriz[,5]) < 15, 123, 4*5)


vector_2=vector()

for(i in 1:20){
  
  vector_2[i] = i*2

  }
vector_2


vector_3 = vector()

for(i in 1:20){
  
  vector_3[i] = vector_2[i]^3
}
vector_3

matriz_2 = matrix(, nrow(matriz), ncol(matriz))

for(i in 1:ncol(matriz)){
  
  matriz_2[,i] = matriz[,i]-50
}
matriz_2


matriz_3 = matrix(, 6, 5)

for(j in 1:5){
  
  for(i in 1:6){
    
    matriz_3[i,j] = i*j
  }
}
matriz_3

matriz_4 = matrix(, 7, 10)

matriz_4[,1] = 1

for(j in 2:10){
  for(i in 1:7){
    matriz_4[i,j]=matriz_4[i,j-1]*2
  }
}
matriz_4


array= array(dim= c(5, 8, 3))

for(k in 1:3){
  for(j in 1:8){
    for(i in 1:5){
      
      array[i,j,k]= mean(i*j*k)
      
    }
  }
}
array

array[5,6,3]










