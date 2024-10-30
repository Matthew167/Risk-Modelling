

# Using probability density functions

dexp(30, 0.05)
dexp(30, 0.05, log = TRUE)

x <- c(18.9, 16.2, 51.5, 65.2, 23.8, 4.2, 21.1, 0.5, 15.2, 3.3)
l <- dexp(x,0.05)
prod(l)
lnL <- log(prod(l)); lnL
lnL <- sum(dexp(x, 0.05, log = TRUE)); lnL

x <- 50
dgamma(x, 6, 0.1)

y <- 1000
dlnorm(y, 5, 2) 

set.seed(300)
y <- rweibull(20, 4, 0.00002^(-1/4))
c <- 0.00002; g <- 4
sum(dweibull(y, g, c^(-1/g), log = TRUE))

# Using the cumulative distribution function

pexp(800, 0.001)
pexp(800, 0.001, lower = FALSE)

y <- 1000
plnorm(y, 5, 2)

pgamma(50, 6, 0.1)

c <- 0.00002; g <- 4
pweibull(12, g, c^(-1/g)) - pweibull(8, g, c^(-1/g))

# Using the quantile function

qexp(0.5, 0.02)

qlnorm(0.995, 5, 2)
qlnorm(99.5, 5, 2)

qgamma(0.75, 6, 0.1) - qgamma(0.25, 6, 0.1)

c <- 0.00002; g <- 4
qweibull(0.7, g, c^(-1/g))

qweibull2 <- function(p, c, g){
  (-log(1 - p)/c)^(1/g)
}

qweibull2(0.7, c, g)

# Using the random number generator functions

set.seed(100)
rexp(1, 0.02)

set.seed(100)
rexp(5, 0.02)

set.seed(50)
rlnorm(10, 5, 2)

set.seed(1)
rgamma(5, 6, rexp(5, 10))

set.seed(10)
c <- 0.00002; g <- 4
y <- rweibull(3, g, c^(-1/g))
x <- rexp(3, 0.1)
z <- y/x; z

rweibull2 <- function(n, c, g){
  (-log(1 - runif(n))/c)^(1/g)
}

set.seed(10)
c <- 0.00002
g <- 4
y <- rweibull2(3, c, g)
x <- rexp(3, 0.1)
z <- y/x; z

# Distributions not built in to R

# The two-parameter Pareto distribution
rpareto <- function(n, a, lambda ){
  lambda * ((1 - runif(n))^(-1/a)-1)
}

dpareto <- function(x, a, lambda){
  a * lambda^(a)/((lambda + x)^(a+1))
}

ppareto <- function(q, a, lambda){
  1 - (lambda/(lambda + q))^a
}

qpareto <- function(p, a, lambda){
  lambda * ((1 - p)^(-1/a)-1)
}

# The Burr distribution
rburr <- function(n, a, lambda, g){
  (lambda*((1 - runif(n))^(-1/a) - 1))^(1/g)
}

dburr <- function(x, a, lambda, g){
  a * g * lambda^(a)*x^(g - 1)/((lambda + x^g)^(a+1))
}

pburr <- function(q, a, lambda, g){
  1 - (lambda/(lambda + q^g))^a
}

qburr <- function(p, a, lambda, g){
  (lambda*((1 - p)^(-1/a) - 1))^(1/g)
}


# Plotting the CDF of a Burr distribution
pburr <- function(q, a, lambda, g){
  1 - (lambda/(lambda + q^g))^a
}

alpha <- 10; lambda <- 500; gamma <- 0.5
x <- seq(0, 100, by = 0.1)
Fx <- pburr(x, alpha, lambda, gamma)
plot(x, Fx, xlab = "x",ylab = "F(x)",
     main = "F(x) for Burr(10,500,0.5)", type = "l")

# The three-parameter Pareto distribution

d3pareto = function(x, a, lambda, k){
  gamma(a + k) * lambda^a / (gamma(a) * gamma(k)) *
    x^(k-1) / (lambda + x)^(a + k)
}

q <- 10
integrate(d3pareto, 0, q, a = 2, lambda = 4, k = 5)
integrate(d3pareto, 0, q, a = 2, lambda = 4, k = 5)$value



d3pareto = function(x, a, lambda, k){
  gamma(a + k) * lambda^a / (gamma(a) * gamma(k)) *
    x^(k-1) / (lambda + x)^(a + k)
}

1 - integrate(d3pareto, 0, 10000, a = 2, lambda = 200, k = 7)$value

# The Weibull distribution
rweibull2 <- function(n, c, g){
  (-log(1 - runif(n))/c)^(1/g)
}

dweibull2 <- function(x, c, g){
  c * g * x^(g-1) * exp(-c * x^g)
}

pweibull2 <- function(q, c, g){
  1 - exp(-c * q^g)
}

qweibull2 <- function(p, c, g){
  (-log(1 - p)/c)^(1/g)
}

# Question 15.1

# Part (i)
set.seed(80)
rpareto <- function(n, a, lambda ){
  lambda*((1 - runif(n))^(-1/a) - 1)
}
x <- rpareto(500, 3, 10000)
head(x)

# Part (i) alternative solution
set.seed(80)
u <- runif(500, 0, 1)
a <- 3; lambda <- 10000
sim <- lambda*(1 - u)^(-1/a) - lambda

head(x)
head(sim)

# Part (ii)
skewness <- sum((x - mean(x))^3)/length(x)
coeff.of.skew <- skewness/var(x)^(3/2)
coeff.of.skew

# Question 15.2

pbeta(0.75, 0.02, 0.4, lower = FALSE)
pbeta(0.75, shape1 = 0.02, shape2 = 0.4, lower.tail = FALSE)

# Question 15.3

a <- 3; lambda <- 5; k <- 4
f <- function(x) {
  gamma(a + k)*(lambda^a)*x^(k - 1)/
    (gamma(a)*gamma(k)*(lambda + x)^(a + k))
}

f(2)

Func <-function(M) {
  abs(integrate(f, 0, M)$value - 0.5)
}
k*lambda/(a - 1)
M.start <- 10
nlm(Func, M.start)

# Reasonableness check
M <- nlm(Func, M.start)$estimate
integrate(f, 0, M)

# Question 15.4

# Part (i)
m <- 0
s2 <- 0.25
s <- s2^0.5
exp(m + 0.5*s^2)
x <- seq(0.000001, 2, by = 0.000001)
fx <- dlnorm(x, m, s)
max <- max(fx); max
i <- which(fx == max); i
match(max(fx), fx)
x[i]

# Part (i) check
exp(m - s2)

# Part (i) alternative solution
x[which(dlnorm(x, 0, 0.5) == max(dlnorm(x, 0, 0.5)))]

# Part (ii)
plot(x, fx, type = "l", main = "Density function for LogN(0,0.25)")