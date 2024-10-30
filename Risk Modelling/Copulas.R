

# i)


rClayton = function(n, a){
  u = runif(n)
  w = runif(n)
  v = ((w ^ (-a / (a + 1)) - 1) * u ^ (-a) + 1) ^ (-1/a)
  data.frame(u = u, v = v)
}


# ii)


# a)

set.seed(1729)
clay.sims = rClayton(1000, 2)

# b)

head(clay.sims, 5)


# iii)


# a)

plot(clay.sims,
     main = "Simulation of Clayton copula with alpha = 2")


# iv)


cor(clay.sims, method = "pearson")
cor(clay.sims, method = "spearman")
cor(clay.sims, method = "kendall")


# v)


# a)

set.seed(1337)

x = rnorm(1000, 100, 20)
y = rgamma(1000, 3, 0.1)

# b)

head(x, 5)
head(y, 5)

# c)

plot(x,y, main="Simulated N(100,400) (x) and Ga(3, 0.1) (y) values 
     connected by the product copula")


# vi)


# a)

x.clay = qnorm(clay.sims[,1], 100, 20)
y.clay = qgamma(clay.sims[,2], 3, 0.1)

# b)

head(x.clay)
head(y.clay)

# c)

plot(x.clay,y.clay, main="Simulated N(100,400) (x) and Ga(3, 0.1) (y) values 
     connected by Clayton copula with alpha = 2")