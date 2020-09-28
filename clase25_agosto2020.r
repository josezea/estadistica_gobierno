

# Intro IC

qnorm(0.025)
qnorm(0.975)

# Intervalo de confinaza de  mu asumiendo sigma conocido
set.seed(25092020)
y <- rnorm(n = 1000, mean = 3000000, sd = 2000000)
hist(y)
set.seed(25092020)
y_muestra <- y[sample(1000, 100)]
n <- length(y_muestra)
ybarra <- mean(y_muestra)
confian <- 0.95
alpha <- 1 - confian
z <- qnorm(1 - alpha / 2)

# Intervalo de confinaza de  mu asumiendo sigma conocido
set.seed(25092020)
y <- rnorm(n = 1000, mean = 3000000, sd = 2000000)
hist(y)

set.seed(25092020)
y_muestra <- y[sample(1000, 100)]
n <- length(y_muestra)
ybarra <- mean(y_muestra)
confian <- 0.95
alpha <- 1 - confian
z <- qnorm(1 - alpha / 2)

LI <- ybarra - z * sqrt(2000000 ^2 / n)  # sigma / raiz(n)  es lo mismo raiz(sigma ^2 / n)
LS <- ybarra + z * sqrt(2000000 ^2/ n) 
c(LI, LS)
