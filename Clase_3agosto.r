
# Ejemplo 1: Simular una distribución Bernpulli
# Simular la proporción de masculino y femenino
# Población de tamaño 1000000
# P = 0.52 # mujeres
# 1- P = 0.48 # Hombres
# Simular 0 si es hombe, 1 si es mujer 
# r: random: aleatorio
set.seed(3072020)
x <- rbinom(n = 1000000, size = 1, prob = 0.52) # Bernoulli caso particular Binom (n = 1)
table(x) # Frecuencia absolutas
prop.table(table(x))

# table(datos$sexo) # Frecuencias absolutas
# Similar a 0.52
mean(x)

0.52 * 0.48 # Teórico: \sigma^2 = p (1-p)
# Similar a esto: Aproximamos
var(x)


# Ejemplo 2: Distribución binomila , p = 0.1, n = 100
# Calcular la probabilidad de que X = 9
# Probabilidades puntuales (con el prefijo d)
dbinom(x = 8, size = 100, prob = 0.1)

dbinom(x = 9, size = 100, prob = 0.1)
choose(100,9) * (0.1 ^9) * 0.9 ^ 91 # Usando directamente la fórmula

dbinom(x = 10, size = 100, prob = 0.1)
dbinom(x = 20, size = 100, prob = 0.1)


plot(x = 0:100, y = dbinom(x = 0:100, size = 100, prob = 0.1), type = "h")
# Probabilidades acumuladas (prefijo p)

# Clacular la probabilidad de que haya más de 9 personas morosoas (10 o más)
#  dbinom(x = 10, size = 100, prob = 0.1) + dbinom(x = 11, size = 100, prob = 0.1) + --
#   dbinom(x = 100, size = 100, prob = 0.1)
sum(dbinom(x = 10:100, size = 100, prob = 0.1)) # P(X >= 10)
1 - sum(dbinom(x = 0:9, size = 100, prob = 0.1)) # 1 - P(X <= 9))
1 - pbinom(q = 9, size = 100, prob = 0.1) # 1 - P(X <= 9)
pbinom(q = 9, size = 100, prob = 0.1, lower.tail = F) # P(X > 9) = P(X >= 10)


# P(X >= 20) = P(X <= 19)
1 - pbinom(q = 19, size = 100, prob = 0.1) # 1 - P(X <= 9)



# Estaturas de los hombres  colombianos: 170.6 (cm)
# Las estaturas se ha probado que sigue una normal


estaturas <- rnorm(4000000, mean = 170.6, sd = 6)
hist(estaturas, breaks = 10000)
hist(estaturas, breaks = 10000, probability = T)

# En la vida real mu = 170.6 y sigma = 6 se deben estimar con datos seleccionados de una muestra
hist(estaturas)
6 /  170.6 * 100

# P (X < 160)
pnorm(q = 160, mean = 170.6, sd = 6)

# P(X > 180) = P(X >= 180)
1 - pnorm(q = 180, mean = 170.6, sd = 6)

# P (170 < X < 180) = P(X > 170 & X < 187 )
# P(X < 180) - P(X < 170)
pnorm(q = 180, mean = 170.6, sd = 6) - pnorm(q = 170, mean = 170.6, sd = 6) 

# P (180 < X < 185: ejercicio, colocar que mu = 182 (Holandeses), mu = 7
# Comparar P (180 < X < 185) con los datos colombianos mu = 170.6, sigma = 6


############### Distribución estandar ###################################
# P (X < 160)
pnorm(q = 160, mean = 170.6, sd = 6)
# (160 - 170.6) / 6 = -1.7667

# P(Z < 1.7666)
pnorm(q = (160 - 170.6) / 6 , mean = 0, sd = 1)
pnorm(q = (160 - 170.6) / 6)


#  P (170 < X < 180)
# P(X < 180) - P(X < 170)
pnorm(q = 180, mean = 170.6, sd = 6) - pnorm(q = 170, mean = 170.6, sd = 6) 

# P(Z < 1.567) - P(Z < -0.1)
pnorm(q = (180 - 170.6) / 6) - pnorm(q = (170 - 170.6) / 6) 

# Uniforme
# X ~ U (a = 7, b = 15)
# P(8 < X < 10 )
# punif(10, min = 7, max = 15) - punif(8, min = 7, max = 15)  

