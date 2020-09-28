# Intervalo para mu (asumiendo sigma conocido)
# Ejemplo 1:
n <- 450
xbarra <- 900000


confianza <- 0.94
error <- 1 - confianza
z <- qnorm(1 - error / 2)

900000 - z * 200000 / sqrt(450)
900000 + z * 200000 / sqrt(450)

# Interpretación:  
# Ejemplo 2

confianza <- 0.98
alpha <- 1- confianza
qnorm(1 - alpha / 2)
(qnorm(0.99)^2) * (200000 / 50000) ^2


qnorm(1 - alpha / 2)^2 * (200000/10000)^2


qnorm(0.975)
qt(0.975, 9)
qt(0.975, 399)
qt(0.975, 999)
qt(0.975, 4999)

tstudent <- function(x) dt(x, 9)
curve(dnorm, xlim = c(-5,5))
curve(tstudent, xlim = c(-5,5))
