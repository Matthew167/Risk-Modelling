

set.seed(2030)
x <- round(rlnorm(5000, 7, 1), 2)

# Excess of loss reinsurance 
# Example
y <- pmin(x, 1000)
z <- pmax(0, x - 1000)
x[20]; y[20]; z[20]

x.involvingRI <- x[x > 1000]
length(x.involvingRI)
length(x[x > 1000])
length(x[x > 1000]) / length(x)

# Excess of loss reinsurance question

rpareto <- function(n, a, lambda ){
  lambda*((1 - runif(n))^(-1/a) - 1)
}

set.seed(5)
x.pareto <- rpareto(10000, 2, 800)

z2 <- pmax(0, x.pareto - 1500)
length(z2[z2 > 0]) / length(x.pareto)

# Analysing the effect of excess of loss reinsurance

var(x)
var(y)

1 - mean(y) / mean(x)

quantile(y, 0.5)

length(y[y < 500]) / length(y)

z.nonzero <- z[z > 0]
mean(z.nonzero)

ascending <- sort(z)
last.3 <- tail(ascending, 3)
sum(last.3)

# Designing a reinsurance arrangement - example
z.profit <- function(premium) {
  z <- pmax(0, x - 1000)
  total.reinsurance.payments <- sum(z)
  profit <- premium - total.reinsurance.payments
  abs(profit)
}

nlm(z.profit, 20000)
sum(z)

# Designing a reinsurance arrangement - question
premium <- 2000000
z.profit <- function(M) {
  z <- pmax(0, x - M)
  total.reinsurance.payments <- sum(z)
  profit <- premium - total.reinsurance.payments
  abs(profit)
}
nlm(z.profit, 20000)

#Proportional reinsurance - example
a <- 0.8
y <- a*x
z <- (1 - a)*x

mean(y); var(y)
mean(z); var(z)

# Inflation of claims amounts - example 1
a <- 0.85
k <- 1.03
z <- (1 - a)*k*x
mean(z)

y1 <- var(a*x)
y2 <- var(a*k*x)
y2/y1 - 1

# Inflation of claims amounts - question
quantile((1 - a)*k^10*x, 0.9)

# Inflation of claims amounts - example 2
set.seed(2357)
x <- rexp(10000, 0.001)

y <- pmax(0, 1.05*x - 100)
kx <- 1.05*x; y <- pmax(0, kx - 100)
mean(y)

mean(y[y > 0])

n <- length(y[y > 0]) 
n / length(x)

# Estimation - example
set.seed(1955)
zclaims <- pmax(0, rgamma(10000, floor(runif(1, 10, 15)),
                          round(runif(1, 2.2, 2.3), 2)) - 7)

n1 <- length(zclaims[zclaims == 0])
n1

zcensored <- zclaims[zclaims > 0]
M <- 7

flnL <- function(parameters) {
  cont1 <- n1 * pgamma(M, shape = parameters[1], 
                       rate = parameters[2], log.p = TRUE)
  
  cont2 <- sum(dgamma(zcensored + M, shape = parameters[1], 
                      rate = parameters[2], log = TRUE))
  
  - cont1 - cont2
}

p <- c(10, 10)
flnL(p)
nlm(flnL, p)

# Estimation - question
set.seed(10249)
yclaims <- pmin(rexp(10000, rgamma(1, 3, runif(1, 580, 620))), 1000)

M <- 1000
n2 <- length(yclaims[yclaims == 1000]); n2

yclaims.excl1000 <- yclaims[yclaims < 1000]
yclaims.excl1000 <- yclaims[yclaims != 1000]

flnL <- function(parameter) {
  cont1 <- n2 * pexp(M, rate = parameter, lower.tail = FALSE, 
                     log.p = TRUE)   
  
  cont2 <- sum(dexp(yclaims.excl1000, rate = parameter,
                    log = TRUE))
  
  - cont1 - cont2
}


p <- 1; flnL(p)

nlm(flnL,p)


n1 <- length(yclaims[yclaims < 1000])

n1 / (1000 * n2 + sum(yclaims[yclaims < 1000]))


lambda <- nlm(flnL, p)$estimate
MLE <- nlm(flnL, p)
lambda <- MLE$estimate
plot(density(yclaims, from = 100, to = 1000), xlab="Amounts paid by insurer",main="Empirical density function
     vs
     Exp(0.00272)", col = "blue")

x <- seq(100, 1000)
y <- dexp(x, rate = lambda)
lines(x, y, col = "red")
legend("topright", legend = c("Empirical density", "Fitted Exp distn"),
       col = c("blue", "red"), lty = 1)

# Question 18.1

pgamma(200, shape = 5, rate = 0.04, lower.tail = FALSE)

# Question 18.2

set.seed(1); x <- sort(rpois(365, rgamma(365, 20, 0.01)))
f <- 0.01*x
f2 <- 1.04*f
nonf <- (1 - 0.01)*x
nonf2 <- x - f2
nonf2 <- x * (1 - 1.04*0.01)
skew.nonf2 <- sum((nonf2 - mean(nonf2))^3) / length(nonf2); skew.nonf2
skew.nonf2^(1/3)

# Question 18.3

# Part (i)
set.seed(16550)
M <- 2500
n <- 10000
x <- rexp(n, rate = 0.0005)
z <- pmax(x - M, 0)
y <- x - z
mean(y)
y <- pmin(x, M); mean(y)

# Part (ii)
f <- function(M) {
  var(x - pmin(pmax(x - M, 0), 1000))
}
nlm(f, 1000)

# Question 18.4

# Part (i)
data <- read.table("Reinsurance payments.txt", header = TRUE)
head(data)
data <- read.table("Reinsurance payments.txt", header = FALSE)
head(data)
data[1:20, ]
Payments <- data[, 1]

# Part (ii)
M <- 45000
RI.claims <- Payments[Payments > 0]
head(RI.claims) 
nm <- length(Payments) - length(RI.claims); nm
flogN <- function(parameters) {
  - nm*plnorm(M, meanlog = parameters[1], sdlog = 
                parameters[2], log.p = TRUE)-
    sum(dlnorm(RI.claims + M, meanlog = parameters[1],
               sdlog = parameters[2], log = TRUE))
} 
mu <- mean(log(RI.claims))
sigma <- sd(log(RI.claims))
p <- c(mu, sigma)
flogN(p)
nlm(flogN, p)

# Part (iii)
x <- Payments + M
max(x)
breakpoints <- c(20000, 45000, seq(46000, 86000, 1000))
hist(x, freq = FALSE, breaks = breakpoints, xlim = c(0, 86000), ylim = c(0, 0.00005), ylab = "Density", main = "Truncated data
     versus
     logN(10.475,0.265)", col = "grey") 
m <- nlm(flogN, p)$estimate[1]
s <- nlm(flogN, p)$estimate[2]
lines(seq(0, 86000), dlnorm(seq(0:86000), meanlog = m, sdlog = s),
      col = "blue")

# Part (iv)
max <- max(RI.claims); max
which(RI.claims == max)
which(Payments == max)

# Part (v)
x <- M + max
plnorm(x, m, s,lower.tail = FALSE)
m <- nlm(flogN, p)$estimate[1]
s <- nlm(flogN, p)$estimate[2]
plnorm(x, m, s, lower.tail = FALSE)
