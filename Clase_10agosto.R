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

