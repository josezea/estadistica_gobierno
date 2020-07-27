library(moments)
library(writexl) # Para exportar a excel un data.frame
#  Repaso
ID <- 1:5
edad <- c(19, 18, 19, 19, 21)
estat <- c(1.65, 1.60, 1.72, 1.66, 1.58)
peso <- c(56, 54, 68, 55, 52)
sexo <- c("F", "F", "M", "F", "F")
tieneHermanos <- c(T, T, T, T, F)
# Longitud
length(edad)
# Promedio (media)
mean(estat)
# Desviación estándar
sd(estat)
# Coeficiente de variación
sd(estat) / mean(estat)
# hist
hist(estat)
boxplot(estat)
moments:::skewness(estat)
skewness(estat) # Coeficiente de asimetría
summary(estat)

# Dataframe
datos <- data.frame(ID, edad, estat, peso, sexo, tieneHermanos)

class(tieneHermanos)
class(edad)
class(sexo)
class(datos)
write_xlsx(datos, "datos clase 1.xlsx")
rm(estat) # borro el vector estatura
mean(datos$estat)
mean(datos[,3])
datos[,3]
datos[1,]
datos[4,]
datos[4,3] # datos[filas, columnas]
datos[c(1,4),]

# Frecuencia absulta
table(datos$sexo)

####################### Leer la encuesta multiproposito de Bogotá (EMB) ####
# Cambiar la carpeta 
setwd("~/Laboral2020/Universidad Externando/gobierno20202/clases")
setwd("C:/Users/Home/Documents/Laboral2020/Universidad Externando/gobierno20202/clases")
dir()

emb <- read.csv2("variables_adicionales_hogar_v3.txt")
names(emb)
