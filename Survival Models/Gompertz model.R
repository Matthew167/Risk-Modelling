
# i)
B = 0.00000729
c = 1.128

mux = function(x){
  B * c ^ (x - 25)
}

plot(25:100, mux(25:100), type = "l", col = "blue", main = "Graph of mu_x against age",
     xlab = "age", ylab = "Force of mortality", lwd = 3)

# ii)
library(flexsurv)
shape = log(c)
rate = B

plot(dgompertz(0:100, shape, rate * c ^ 20), col = "blue", type = "l", 
     main = "Density of the future lifetime of a 45-year-old",
     xlab = "t", ylab = "f(t)", lwd = 3)


# iii)
integrate(pgompertz, 0, Inf, shape = shape, rate = rate * c ^ 25, lower.tail = FALSE)

#Or
tpx2 = function(x, t){
  g = exp(-B/ log(c))
  g ^ (c ^ (x - 25) * (c ^ t - 1))  
}

integrate(tpx2, 0, Inf, x = 50)


# iv)a)
set.seed(50)
sims = rgompertz(100000, shape, rate * c ^ 25)

# b)
mean(sims)