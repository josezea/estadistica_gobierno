

N <- 27

set.seed(14092020)
y <- rnorm(27, mean = 170, sd = 8)
hist(y)


choose(27,10)

ybarra <- numeric(100000)
for(i in 1:100000){
  
  ybarra[i] <- mean(y[sample(27,10)])
}

ybarra
hist(ybarra)
mean(ybarra)

# seccion 4
# Ejerjcio 1
# Pr (0.2 < pgorr0 < 0.4)
pnorm(q = 0.4, mean = 0.3, sd = sqrt((0.3*0.7)/ 100)) - 
    pnorm(q = 0.2, mean = 0.3, sd = sqrt((0.3*0.7)/ 100))  


pbinom(q = 40, size = 100, prob = 0.3) -  pbinom(q = 20, size = 100, prob = 0.3)


# EJercicio 3

pbinom(q = 280, size = 1000, prob = 0.25) - 
  pbinom(q = 220, size = 1000, prob = 0.25) 

pnorm(q = 0.28, mean = 0.25, sd = sqrt((0.25*0.75)/1000)) - 
    pnorm(q = 0.22, mean = 0.25, sd = sqrt((0.25*0.75)/1000))  
  