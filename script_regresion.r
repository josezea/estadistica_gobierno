

# Ir al menu session -> set wordking directory -> choose directory,  Buscar carpeta y darle open
dir()

datos <- read.csv("houses_portland.csv")
head(datos)

# Ejercicio: convertir los pies cuadrados a metros cuadrados
datos$area <- datos$area * 0.092903

# Realizar exploración de los datos
summary(datos$area)
summary(datos$price)

plot(datos$area, datos$price, pch = 20, xlab = "Área (mts2)",
     ylab = "Precio (US$)")
cor(datos$area, datos$price)

# Análitico (Minimización de MSE, en R para la regresión)

# Y es modelado por X: y ~ X 
modelo <- lm(price ~ area, data = datos)
summary(modelo)
plot(datos$area, datos$price, pch = 20, xlab = "Área (mts2)",
     ylab = "Precio (US$)")
abline(modelo, col = "red")

71270.5 +  1448.0  * 200
