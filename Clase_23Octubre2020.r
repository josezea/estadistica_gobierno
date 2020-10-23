# Ejercicio 34, cáp 9.4
x <- c(1.7, 1.5, 2.6, 2.2, 2.4, 2.3, 2.6, 3, 1.4, 2.3)
alpha <- 0.05
mu_0 <- 2

xbarra <- mean(x)
s <- sd(x) # sqrt(var(x))
n <- length(x)

t_alphaMedios <- qt(alpha / 2, n-1)
t_0975 <- qt(1-alpha / 2, n-1)

ee <- s / sqrt(n) # error estandar : raiz cuadrada de V(Ybarra)

t_c <- (xbarra - mu_0) / ee

# Enfoque de la zona de rechazo
t_c < t_alphaMedios | t_c > t_0975
# COnclusión: t calculado no cae en la zona rechazo por 
# tanto no rechazo H0 (Acepto H0, es decir mu = 2)

# Enfoque del p valor
pvalor <- (1 - pt(t_c, n-1)) + pt(-t_c, n-1)
# pvalor <- 2 * (1 - pt(t_c, n-1))
# pvalor <- 2 * pt(-t_c, n-1)
 
# MÁS FACIL SI NOS DA PEREZA :) :
t.test(x, mu = 2, conf.level = 0.95)


# pruba para diferencia de medias

num <- 0.1 # mu1 - mu2 - 0.1
denom <- sqrt( ((0.4^2) / 50) + ((0.35^2) / 51))
t_c <- 0.1 / denom


num_gl <- (((0.4^2) / 50) + ((0.35^2) / 51))^2 
denom_gl_1 <- (1 / 49) * ((((0.4^2) / 50)) ^2 )
denom_gl_2 <- (1 / 50) * ((((0.35^2) / 51)) ^2 )
gl <- floor(num_gl / (denom_gl_1 + denom_gl_1))
pvalor <- 1- pt(t_c, gl)
pvalor
# COnclusión no hay evidencia estadística para rechazar H0
# No hay evidencia de que lo hombres ganen más de 100 mil pesos que las mujeres


# Hay disciminación (H1. mu1 - mu2 > 0)

num <- 0.2 # 3-2.8 - 0
denom <- sqrt( ((0.4^2) / 50) + ((0.35^2) / 51))
t_c <- 0.2 / denom


num_gl <- (((0.4^2) / 50) + ((0.35^2) / 51))^2 
denom_gl_1 <- (1 / 49) * ((((0.4^2) / 50)) ^2 )
denom_gl_2 <- (1 / 50) * ((((0.35^2) / 51)) ^2 )
gl <- floor(num_gl / (denom_gl_1 + denom_gl_1))
pvalor <- 1- pt(t_c, gl)
pvalor
# COnclusión si hay evidencia de discrimiación (
# hombres ganan más que las mujeres)
)





# En windows
# Seleccionar la tabla de excel con los titulos de las columnas (encabezados)
# Darle ctrl C (copiar)
# Correr el comando de abajo
#datos <- read.delim("clipboard")
# En Mac:
#datos <- read.delim(pipe("pbpaste"))
#dput(datos)
datos <- data.frame(y1 = c(300L, 280L, 344L, 385L, 372L, 360L, 288L, 
                           321L, 376L, 290L, 301L, 283L), 
                    y2 = c(274L, 220L, 308L, 336L, 198L, 300L, 
                           315L, 258L, 318L, 310L, 332L, 263L),
                    stringsAsFactors = F)
y1 <- datos$y1
y2 <- datos$y2
t.test(y1, y2, alternative = "greater", mu = 0 , conf.level = 0.95)
