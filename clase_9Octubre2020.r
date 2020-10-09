# n1 <- 50
# n2 <- 48
# xbarra1 <- 3000000
# xbarra2 <- 2800000
# sigma1 <- 120000
# sigma2 <- 100000
# confiabilidad <- 0.95
# 
# D <- xbarra1 - xbarra2 # Estimación puntual (resta de los promedios)
# ee <- sqrt( ((sigma1^2) / n1) + ((sigma2^2) / n2)) # V(xbarra1-xbarr2): error estándar
# z <- qnorm(1 -(1-confiabilidad) / 2)  
# LI <- D - z * ee
# LS <- D + z * ee
# LI 
# LS

ic_difMedias_sigmasConoc <- function(n1, n2, xbarra1, xbarra2,
                                     sigma1, sigma2, confiabilidad){
  
  D <- xbarra1 - xbarra2 # Estimación puntual (resta de los promedios)
  ee <- sqrt( ((sigma1^2) / n1) + ((sigma2^2) / n2)) # V(xbarra1-xbarr2): error estándar
  z <- qnorm(1 -(1-confiabilidad) / 2)  
  LI <- D - z * ee
  LS <- D + z * ee
  salida <- c(LI, LS)
  return(salida)
}

ic_difMedias_sigmasConoc(n1 = 50, n2 = 48, xbarra1 = 3000000,
                         xbarra2 = 2800000, sigma1 =  120000,
                         sigma2 = 100000, confiabilidad = 0.95)

y1 <- c(170, 180, 190, 178, 165, 162)
y2 <- c(162, 165, 170, 158, 161, 164, 160, 159)
sigma1 <- 7 # poblacional poco realista (ejemplo no dado en la vida real)
sigma2 <- 11 # poblacional poco realista (ejemplo no dado en la vida real)
# conf del 90%
ic_difMedias_sigmasConoc(n1 = length(y1), n2 = length(y2),
                         xbarra1 = mean(y1),
                         xbarra2 = mean(y2), sigma1 =  7,
                         sigma2 = 11, confiabilidad = 0.90)

?t.test
library(TeachingSampling)
data(Lucy)
dim(Lucy)
# Pago de impuestos para empresas pequeñas y medianas
y1 <- Lucy$Taxes[Lucy$Level == "Small"] # y1 <- c(3, 4,..)
y2 <- Lucy$Taxes[Lucy$Level == "Medium"]
# Promedio de pago impuestoas empresas pequeñas - Promedio de pago de impuestos empresas medianas
t.test(y1, y2) # por defecto el nivel de confianza del 95%
t.test(y1, y2, conf.level = 0.95) # lo mismo

# Promedio de pago impuestoas empresas medianas - Promedio de pago de impuestos empresas pequeñas
t.test(y2, y1)
# COnfianza al 99%
t.test(y2, y1, conf.level = 0.99)

# Ejemplo 2
y1 <- c(170, 180, 190, 178, 165, 162)
y2 <- c(162, 165, 170, 158, 161, 164, 160, 159)
# Ejercicio: calcular intervalo de confianza del 90%
t.test(y1, y2, conf.level = 0.90)

sd(y1)
sd(y2)
# Coeficientes de variacion
sd(y1) / mean(y1) * 100 # M´s dispersion de la estaturas de los hombres
sd(y2) / mean(y2) * 100


# Supuesto de varianzanas iguales
# Ejemplo 2
y1 <- c(170, 180, 190, 178, 165, 162, 174, 172)
y2 <- c(162, 165, 170, 158, 161, 164, 160, 159, 180, 185)
t.test(y1, y2, conf.level = 0.90, var.equal = T)


# Intervalo de co fianza para la proporción

ic_prop <- function(x, confianza = 0.95) {
  p_gorro <- mean(x)
  alfa <- 1- confianza
  n <- length(x)
  z <- qnorm(1-(alfa/2))
  LI <- p_gorro - (z * sqrt((p_gorro * (1-p_gorro))  / n))
  LS <- p_gorro + (z * sqrt((p_gorro * (1-p_gorro))  / n))
  return(c(LI, LS))
}

y <- c(rep(1, 10), rep(0, 16))
ic_prop(y)

# Ejemplo 2
n = 300
#Toman o fuman el 40%
300 * 0.4
y <- c(rep(1, 120), rep(0, 180))
# confianza del 99%
ic_prop(y, confianza = 0.99)

# confianza del 95%
ic_prop(y, confianza = 0.95)
ic_prop(y)
