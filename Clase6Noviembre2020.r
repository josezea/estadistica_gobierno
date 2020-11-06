
# 1. Prpeuba de Hipotesis para una media
#H0: mu >= 1 SMLMV: 877.803
#H1: mu < 1 SMLMV
#1 - alpha = 0.9
# Población trabajadores
set.seed(6112020)
y <- rnorm(n = 200,  mean = 800000, sd = 800000)
summary(y)
t.test(y, alternative = "less", mu =  877803, 
       conf.level = 0.9)
# Conclusión coo pvalor es menor a 0.1, rechazo H0: 
# Los trabajadores ganan menos de un salario mínimo


# 2. Prpeuba de Hipotesis para una proporción
n <- 200
# y: es desempleado (1 SI, 2 NO)
# alpha = 0.05
set.seed(6112020)
y <- rbinom(n = n, size = 1, prob = 0.12)
table(y)
prop.table(table(y))

#H0: p <= 0.1
#H1: p > 0.1 = p_0
# zc = pgorro - p0 / raiz(p0*(1-p0) / n)
prop.test(x = 26, n = 200, p = 0.1, # p es p_0
          alternative = "greater",
          correct = FALSE) # El FALSE es para pruebas de proporciones para que haga la prueba de hipótesis vista en clase, en otro caso hará otra prueba de hiótesis pero no la vista en clase
zc <- sqrt(2)
pvalor <-  0.07865
# No alcanzao a rechazar al 5% por que el pvalor (0.07865) no es menor a 0.05
# No tengo evidencia para  reachzar H0 ( el desempleo en la poblacion
#sea mayor al 10% (rechazar)
# a un nivel de signifincancia del 95%
# Me quedo con H0: el desempleo es menor o igual al 10%.

# Nota si alpha hubiera sido 0.1 si hubiera podido rechazar H0,
# ya qye 0.07865 < 0.1 (alpha)
# es decir hubiera concluido que el desempleo es mayor al 10% con un nivle de significancia del 90%

# La forma vista en clase sin ayuda de funciones de R
# ver 26 / (26 + 174)
zc <-  (0.13 - 0.1) / sqrt(((0.1 * 0.9) / 200))
1 - pnorm(zc)



# Diferencias de medias
# Población 1 es área urbana
# Población 2 es área rural
# y1: Ingreso de trabajadores área urbana
# y2: ingreso de trbaadores área rural
# 1 - alpha = 0.95
# H0: mu1 - mu2 = 100000
# H1: mu1 - mu2 diferente  100000
set.seed(6112020)
y1 <- rnorm(n = 350, mean = 940000, sd = 250000)
set.seed(6112020)
y2 <- rnorm(n = 300, mean = 910000, sd = 220000)

summary(y1)
summary(y2)

t.test(x = y1, y = y2, alternative = "two.sided", mu = 100000,
       conf.level = 0.95)
# El pvalor es menor a alpha (0.05), por  lo tanto
# rechazo H0, es decir;
# los ingresos de lo urbano y rural son diferentes!



# Diferencia de proporciones
# Población 1 es área urbano
# Población 2 es área rural
# p1: proporción de desempleados (mayores 25 años y PEA) en el área urbana
# p2: proporción de desempleados (mayores 25 años y PEA) en el área rural
# alpha = 0.1

set.seed(6112020)
y1 <- rbinom( n = 200, size = 1, prob = 0.12)
set.seed(6112020)
y2 <- rbinom( n = 150, size = 1, prob = 0.08)

table(y1)
prop.table(table(y1))
# p1: 0.13, desmeploe del 13%
table(y2)
prop.table(table(y2))
# p2 = 0.08, desempleo del 8%
# H0: p1 - p2 <= 0
# H1: p1 - p2 > 0
n1 <- length(y1)
n2 <- length(y2)
prop.test(x = c(26, 12),  n = c(200, 150), 
          alternative = "greater",
          correct = FALSE) # El FALSE es para pruebas de proporciones para que haga la prueba de hipótesis vista en clase, en otro caso hará otra prueba de hiótesis pero no la vista en clase
zc <- sqrt(2.2141)
pvalor <-  0.06838
# COnclusiói: a un nivel de signifcancia del 90%, rechazo H0 es decir
# El desempelo en la zona urbana es mayor al desempelo en la zona rural

# Difiere un poquito en el zc y en el pvalor, por un cambio ligero de metodología en el test de R con respecto al del libro
num <- (0.13 - 0.08) - 0
denom <- sqrt( ((0.13 * 0.87) / 200) + (0.08 * 0.92) / 150)
zc <- num / denom
1-  pnorm(zc)

# Conclusión: rechazan la hiótesis nula (con ambos métodos)
# El desempelo en la zona urbana es mayor al desempelo en la zona rural

# Otra forma: 
num <- (0.13 - 0.08) - 0
pgorro <- (26 + 12) / (200 + 150) 
denom <- sqrt(pgorro * (1 - pgorr0) * ((1/n1) + (1 / n2)))
zc <- num / denom
1-  pnorm(zc)
# Nos da igual que la forma a pedal anterior


# Pruebas pareadas 
# Población 1: brinda un subsidio (Ingreso solidario)
# Población 2: (no le da subsidio)

# y1: Indidice de seguridad para poblacion  antes del subisido (0 y 100 , donde 0 es malo y 100 es que está nutrida)
# y2: Indidice de seguridad para poblacion después del subsidio

# D = Y2 - Y1: el valor del indice despues - valor del indice antes

# ID Y1  Y2   D
# 1  60  80  20
# 2  65  90  25
# 3  70  72  2


H0: Dbarra <= 20
H1: Dbarra > 20 
set.seed(6102020)
antes <- rnorm(n = 150, mean = 70, sd = 20)
set.seed(6102020)
despues <- rnorm(n = 150, mean = 93, sd = 12)

# Forma 1
d <- despues - antes
t.test(d, alternative = "greater", conf.level = 0.95)
# Conclusión, rechzao H0 es decir dbarra > 20

# Forma 2
t.test(despues, antes, alternative = "greater", paired = T, 
       conf.level = 0.95)




# Intervalo de confianza

# Diferencias de medias
# Población 1 es área urbana
# Población 2 es área rural
# y1: Ingreso de trabajadores área urbana
# y2: ingreso de trbaadores área rural
set.seed(6112020)
y1 <- rnorm(n = 350, mean = 940000, sd = 250000)
set.seed(6112020)
y2 <- rnorm(n = 300, mean = 910000, sd = 220000)

summary(y1)
summary(y2)

t.test(x = y1, y = y2, alternative = "two.sided",
       conf.level = 0.95)

t.test(x = y1, y = y2,
       conf.level = 0.95)





# Intervalo de confianza
# Población trabajadores
set.seed(6112020)
y <- rnorm(n = 200,  mean = 800000, sd = 800000)
summary(y)
t.test(y,  conf.level = 0.9)
# Conclusión coo pvalor es menor a 0.1, rechazo H0: 
# Los trabajadores ganan menos de un salario mínimo


# Más de intervalos:

#https://github.com/josezea/inferencia
# proproció
# diferencia de proporciones


# diferencia de medias: t.test(x1, x2, conf.level = )
