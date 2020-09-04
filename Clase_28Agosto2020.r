n = 7
set.seed(12345)
id <- sample(18, 7)
id
# 8, 10, 11, 12, 14, 16 , 17
# Muestra de n = 6
#  X: gasto en almentos del hogar
gastoalim <- c(1000000, 800000,1500000, 8000000, 1200000, 2500000 )

# Asumir un modeo gamma, vamos a estimar alpha y beta

alpha = (mean(gastoalim)^2) / var(gastoalim)
beta = mean(gastoalim) / var(gastoalim)

set.seed(12345)
x <- rgamma(n = 18, shape = alpha, rate = beta)
hist(x)

# Cual es la probabildad de que alguien gaste más de un millon
1 - pgamma(q = 1000000, shape = alpha, rate = beta)

# Modelo probabilistico
curve(gamma, shape = alpha, rate = beta)
