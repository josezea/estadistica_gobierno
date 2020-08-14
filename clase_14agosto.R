library(TeachingSampling)

data(Lucy)
summary(Lucy)
# población finita: unas empresas de USA (ingresos, impuestos en miles de dolare,..)

table(duplicated(Lucy$ID))
# No hay duplicados: Es un ideentificador adecuado


# Muestra aleatoria
# Ejercicio: seleccionar aleatoriamente una muestra de tamaño 100
?sample
N <- 2396
n <- 100
indica_muestra <- sample(N, n)
# Siempre me da diferente cada vez que corrar indica_muestra

# Para que a todos nos de la misma muestra y esta sea REPRODUCIBLE
set.seed(140820) # Cualquier semilla, tome en particular la fecha
indica_muestra <- sample(N, n)
indica_muestra 
head(indica_muestra) # Seis primeros valores

# Parentesis
Lucy[4, ] # Fila 4
Lucy[c(1,7), ] # Fila 1 y 7 
Lucy[c(1,7), c(1,2)] # Fila 1 y 7, columna 1 y 2
# Lucy[, c(1,2)] # Fila 1 y 7, columna 1 y 2

muestra <- Lucy[indica_muestra,]
muestra

# Resumnen
set.seed(140820) # Cualquier semilla, tome en particular la fecha
indica_muestra <- sample(N, n)
muestra <- Lucy[indica_muestra,]




# Ejercicio: seleccionar una muestra de tamaño 20 con Lucy, conserve sólo el ID y Ubication
# Colocar semilla 12345
set.seed(12345) # Cualquier semilla, tome en particular la fecha
muestra <- Lucy[sample(2396, 20),c(1,2)]
# Todas las columnas
muestra <- Lucy[sample(2396, 20),]



########################## Concepto de población infinita ##################################

############### Simular el ingreso  #######################
# Simular la distribución del ingreso de los hogares bogotanos
# https://math.stackexchange.com/questions/3104688/method-of-moments-with-a-gamma-distribution
# alpha: forma
# beta: escala
Promedio <- 3333780 # Estimación del Promedio del ingreso de los hogares bogotanos
S <- 4239029 # Estimación de La desviación estándar del ingreso de los hgoares bogotanos
4239029 / 3333780 * 100 # Coeficiente de variación
N_gorro <- 3209379 # El número de hogares ESTIMADO en Bogotá

# No tenemos un listado de los hogares bogotanos :(
# El tamaño poblacional exacto no lo conozco (sino una estimación)

# Asumiré basado en datos histórico un modelo de probabilidad (distribución continua de probabilidad)
# No preocuparse de donde salen los parámetros alpha y beta por el momento
alpha <- (Promedio ^ 2) / (S ^ 2)
beta <- (S ^ 2) / Promedio 

set.seed(12345)
ingreso <- rgamma(n = N_gorro, shape = alpha, scale = beta)
summary(ingreso)
mean(ingreso)
sd(ingreso) / mean(ingreso)
hist(ingreso, breaks = 100)
hist(ingreso, breaks = 10000, probability = T)
hist(ingreso, breaks = 10000, probability = T, xlim = c(0, 15000000))

# Se selecciona una muestra de tamaño 1200 (más adelante se aprendera a estimar) y asumo mi modelo 
# Y ~ Gamma(alpha =  0.618502, beta =  5390088)
 # Realizar con mi muestra de tamaño 1200, estimar el ingreso intervalos de confianza.
# NO puedo acceder a la población, asumo un modelo probabilistico razonable y selecciono
# una muestra para trabajar


# Ejemplo 2: Estaturas 
# No conozco N, no conozco los adultos mayores de 18 y menores de 25 (no hay un marco poblacional)
# SI tengo estudios anteriores
# Y: estaturas de las mujeres bogotanas
# Supongo (evidencia empirica basada en Inferencia)   que
# Y ~ N(mu = ???, sigma = ????)

# Selecciono una muestra de tamaño 400
#promedio = 162
#sigma = 6

# Supongo (evidencia empirica basada en Inferencia)   que
# Y ~ N(mu = 162, sigma = 6)

curve(dnorm(x, mean = 162, sd =  6), xlim = c(140, 190))
