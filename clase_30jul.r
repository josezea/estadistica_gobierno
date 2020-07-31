# https://github.com/josezea/estadistica_gobierno
library(dplyr)
library(moments)
library(writexl) # Para exportar a excel un data.frame
####################### Leer la encuesta multiproposito de Bogotá (EMB) ####
# Cambiar la carpeta  en Session Working Directory Choose 
setwd("~/Laboral2020/Universidad Externando/gobierno20202/clases")
# También así
#setwd("C:/Users/Home/Documents/Laboral2020/Universidad Externando/gobierno20202/clases")
dir()
#emb <- read.csv("~/Laboral2020/Universidad Externando/gobierno20202/clases/variables_adicionales_hogar_v3.txt", sep=";")
#emb <- read.csv("variables_adicionales_hogar_v3.txt", sep=";")

emb <- read.csv2("variables_adicionales_hogar_v3.txt")
names(emb)
class(emb$IPM)
emb$IPM <- as.numeric(emb$IPM)
emb$INGRESOS_HOG <- as.numeric(emb$INGRESOS_HOG)

summary(emb$IPM)
# 1: Absoluto privado muyy muy pobre
# 0: Hogar tiene buenas condiciones  
############ Agregaciones de las estadiísticas localidad ###################
plot(emb$INGRESOS_HOG, emb$IPM, xlim = c(0, 20000000))
cor(emb$INGRESOS_HOG, emb$IPM)

# Consulta 1 : Cual es el promedio, la desviación estándar, cv,  la asimetría, 
# la curtosis del ingreso y la correlación entre ingreso e IPM 
# pipe %>%
consulta <- emb %>% summarise(prom_ing = mean(INGRESOS_HOG),
                              desvest_ing = sd(INGRESOS_HOG),
                              cv_ing = desvest_ing / prom_ing * 100,
                              asim_ing = skewness(INGRESOS_HOG),
                              curtosis_ing = kurtosis(INGRESOS_HOG),
                              cor_ingIPM = cor(INGRESOS_HOG, IPM))

hist(emb$INGRESOS_HOG, xlim = c(0, 20000000), breaks = 300)

# Consulta 2: Cual es el promedio, la desviación estándar,
# la asimetría y la curtosis del ingreso por localidad
# http://www.sdp.gov.co/gestion-estudios-estrat%C3%A9gicos/estudios-macro/encuesta-multiproposito/encuesta-multiproposito-2017
# Bajar tabla de viviendas (este tiene las localidades)
viv <- read.csv("viviendas_2017_v2_03092018.txt")

intersect(names(viv), names(emb))
viv <- viv %>% select(DIRECTORIO, LOCALIDAD_TEX)
emb2 <- inner_join(viv, emb, by = "DIRECTORIO")

consulta2 <- emb2 %>% group_by(LOCALIDAD_TEX) %>%
  summarise(prom_ing = mean(INGRESOS_HOG),
                              desvest_ing = sd(INGRESOS_HOG),
                              cv_ing = desvest_ing / prom_ing * 100,
                              asim_ing = skewness(INGRESOS_HOG),
                              curtosis_ing = kurtosis(INGRESOS_HOG),
                              cor_ingIPM = cor(INGRESOS_HOG, IPM))


#################### Parentesis: recordar curtosis : ######################
# Ejemplo de platicurtico 
a <- runif(1000000, 0,1)
hist(a)
kurtosis(a)
# Ejemplo de distrib mesocurtica
b <- rnorm(1000000, 0,1)
hist(b)
kurtosis(b)


