options(scipen = 9999)

# Población
mu <- 160 # Promedio real
set.seed(12345)
Y <- rnorm(n = 9000, mean = 160, sd = 4)
summary(y)
# 159.9672
hist(Y)
# Lo anterior no se conoce

# Se sospecha que las mujeres miden más que 165
# n = 50
ID <- 1:9000
set.seed(12345)
indica_mue <- sample(9000, 50)
y <- round(Y[indica_mue],1)
mean(y)


(160.598 - 165) / ( 4/ sqrt(50))

z_c <- (160.598 - 165) / ( 4/ sqrt(50))
1-pnorm(z_c)



z_c <- (160.598 - 160) / ( 4/ sqrt(50))
1-pnorm(z_c)



z_c <- (160.598 - 163) / ( 4/ sqrt(50))
pnorm(z_c)


# Con sigma desconocido
sd(y)
sqrt(var(y))
t_c <- (160.598 - 163) / ( 3.806734/ sqrt(50))

t.test(x = y, alternative = "less", mu = 163, conf.level = 0.99)
