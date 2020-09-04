# Cuando la Y es mayuscula es una v.a y no se que resultado voy a obtener, tampoco se a quie
# voy a seleccionar
# Y1 ?, Y2=, pero se que cada Yi es una Bernoulli.

# Hay independencia por que el resultado de Y3 no afecta a Y5, mas aun Yi no afecta a Yj, para i defierente j
N <- 15
n <- 7
set.seed(40920)
indica_mue <- sample(N,n)
indica_mue
# Angela bolaños, Gabriela gutierrez, Juliana Sanabria, Manuela Martinez,  
# Marco Prada, María paula Bautista, Wayna
# 1 si tiene mascota, 0 si no tiene mascota
mascota <- c(1, 0, 1, 1, 0, 1,  1) # y1, ..y7 (y aca es minuscula)

# Ejercicio
# Calcular la funcion de verosimilitud

# funcione_ejemplo <- function(x){
#   x*sin(x)
# }
# 
# funcione_ejemplo(pi/2)
# (pi/2 ) * sin(pi/2)

y <- mascota
suma_y <- sum(y) # 5
n <- length(y) # 7

logL <- function(p){
suma_y * log(p) + (n - suma_y) * log(1-p)  
}

logL <- function(p){
  5 * log(p) + 2 * log(1-p)  
}

curve(logL, from = 0, to = 1, xlab = "p", ylab = "log(L(p))")

logL(5/7) # Se maxima la verfosimilitu acá
logL(6/7)

# Calculemos la proabilidad de que que los 15 estudiantes más de 10 tenga mascotas
# 1 - P(Y < = 9)
1 - pbinom(q = 9, size = 15, prob = 5/7)


# Estimar mu y sigma cuadrado para la muestra seleccionada

# Angela bolaños, Gabriela gutierrez, Juliana Sanabria, Manuela Martinez,  
# Marco Prada, María paula Bautista, Wayna
# Estatura
y <- c(167, 173, 164, 169, 174, 160, 178)

# Estimar mu
mean(y) # 169.2857

# Estimar sigma ^2
var(y)

# Estimar sigma
sd(y) # 6.21059
sqrt(var(y))

# Calcular la probabiliad de  que alguien en el curso mida más de 1.80
1 - pnorm(q = 180, mean = 169.2857, sd = 6.21059)

# Mas o menos 
15 *  0.04224876
