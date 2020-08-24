# Recordemos 
############### Simular el ingreso  #######################
?rexp()
# Tasa que dura un virus 1 /10.5
# Valor esperado de duración del virus es de 10.5 (mu)
x <- rexp(10000, rate = 1 / 10.5)
hist(x)
mean(x)

############### Simular una muestra de tamaño  1, 2, ... 10, 20, 30,.. 100,500, 1000 
nsimulaciones <- 10000
tam_mue <- 1000
datos <- matrix(NA, nrow = nsimulaciones, ncol =  tam_mue)
for(j in 1:tam_mue){  
  datos[,j] <- rexp(1000, rate = 1 / 10.5)
}

# Cada fila representa una muestra de tamaño n = 30

Xbarra <- rowMeans(datos)
hist(Xbarra)
summary(Xbarra)
mean(Xbarra) # aproxima el valor esperado (E(Xbarra))

# Varianza teorica de xbarra
((10.5)^2) / 30 # La teorica con n = 30,  110.25 / 30 = 3.675
#var(Xbarra) # Aproxima V(Xbarra) con n = 30


((10.5)^2) / 100 # La teorica con n = 100,  110.25 / 30 =  1.1025
#var(Xbarra) # Aproxima V(Xbarra) con n = 100

((10.5)^2) / 1000 # La teorica con n = 100,  110.25 / 30 =  1.1025
#var(Xbarra) # Aproxima V(Xbarra) con n = 1000


# Ejercicio: suponga que X se distribuye exponencial con lamba = 1 / 10.5.
# ¿Cuál es la probabilidad de que Xbarra sea mayor a 14 días?
# SUponer: n = 1200 (selecc0na una muestra de 400)
var_xbarra <- (10.5^2) / 1200 # 0.091875
1 - pnorm(q = 14, mean = 10.5, sd = sqrt(var_xbarra))


# Ejercicio 2: suponga que la distibución de X es Bernoulli con parámetro p = 0.08
# X: 1 si el hogar tiene inversiones en bolsa de valores.
# SUponer: n = 400 (selecc0na una muestra de 400)
# Cual es la probabilidad de que Xbarra sea mayor al 10%. Que el promedio en que invierte la 
# gente sea mayor al 10%