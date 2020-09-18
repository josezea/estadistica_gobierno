

library(TeachingSampling)
Support(N = 5, n = 3, ID = paste0("e", 1:5))

muestras <- Support(N = 5, n = 3, ID = c(121, 144, 36, 64, 49))
rowMeans(muestras)
hist(rowMeans(muestras))

library(gtools)
Elementos= c(121, 144, 36, 64, 49)
combinations(5,3,Elementos)

Y <- c(121, 144, 36, 64, 49)

rowMeans(muestras)
apply(muestras, 1, mean)

mean(rowMeans(muestras))
mean(Y)
# El promedio muestral es insesgado para el promedio poblacional

apply(muestras, 1, var)
mean(apply(muestras, 1, var))
var(Y)

