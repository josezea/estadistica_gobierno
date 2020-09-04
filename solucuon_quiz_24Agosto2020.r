

# 1. El precio de la acción de Grupo Bolivar fluctúa semanalmente entre 45000 y 70000 de forma aleatoria
# con distribución  uniforme.
# Hoy el precio de la acción se encuentra en 50.000, y usted decide comprar 100 acciones. 
# Suponga que esas fluctuaciones son totalmente aleatorioas. La siguiente semana usted aspira a vender
# a más de 60.000 mil pesos y así ganar más de un millón de pesos.
# 
# ¿Es una buena decisión la que usted tomó, es decir,  lograría usted los objetivos de ganar más de un
# millón de pesos?
#   
#   Pista: ¿Calcule cuál es la probabilidad que la siguiente semana el precio de la acción sea mayor
# a $60.000 (y así ganar en un semana más de un millón de pesos).

# Retroalimentación
# R/
1-punif(60000, min = 45000, max = 70000)

# a. Sí, es una buena decisión porque la probabilidad de que las acciones estén a más de 60.000 es muy alta (más del 90%) y así garantizo tener una ganancia de más de un millón de pesos con toda seguridad.
# b. Si, Es una buena decisión porque la semana entrante sube con toda seguridad las acciones.
# c. No, Es una mala decisión porque la probabilidad de que las acciones suban a $60.000 o más es del 5%, con lo cual usted seguro no va a lograr la meta.
# d. No, es una mala decisión porque la probabilidad de que las acciones suban a $60.000 o más es 
# del 40%, con lo cual usted no va a lograr la meta.
# e. No se puede conocer si la decisión es buena o mala.


# 2. Suponga que la distribución de los ingresos de las empresas de tecnología (PYME) 
# en un año en Colombia sigue una distribución con media de 4000 millones de pesos 
# con una desviación estándar de 1000 millones de pesos. 
# Suponga que usted selecciona aleatoriamente una de estas empresas.
# ¿Cuál es la probabilidad de que esta empresa genera utilidades entre 3000 y 5000 millones?
pnorm(5000, 4000, 1000) - pnorm(3000, 4000, 1000)  
# 0.6826895

# 3. Suponga que históricamente un banco ha observado los datos de sus clientes y ha calculado que
# la probabilidad de que un cliente entre en mora en tarjeta de crédito en más de 120 días 
# (cliente de difícil recuperación de su deuda) es del 2%.
# Si el banco tiene 400.000 clientes, cuál es la probabilidad de que más de 8000 clientes
# entren en mora más de 90 días.
# Suponga que el comportamiento de los clientes es independiente (es decir, no hay muchos grupos familiares que toman la tarjeta)
1 - pbinom(8000, size = 400000, prob = 0.02)
# 0.4970264