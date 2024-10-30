

# Fitting a distribution

# Data requirements
Losses <- read.table("Losses.txt", header = TRUE)
head(Losses)

# Overview of data
head(Losses$Losses, 10)
summary(Losses$Losses)
min(Losses$Losses); max(Losses$Losses)
quantile(Losses$Losses, seq(0, 0.5, by = 0.1))

# Empirical probabilities
length(Losses$Losses)
large.Losses <- Losses$Losses[Losses$Losses > 5000]
length(large.Losses)
length(large.Losses) / length(Losses$Losses)

# Locating specific observations
which(Losses$Losses == min(Losses$Losses))
Losses$Losses[837]

# Key metrics
mean(Losses$Losses)
var(Losses$Losses)
quantile(Losses$Losses, 0.5)

Losses$b <- (Losses$Losses %/% 1)
head(Losses)

data <- aggregate(Losses ~ b, data = Losses, length)
head(data)
max(data[,2])
row <- which(data[,2] == max(data[,2]))
row
data[row,]

# Maximum likelihood estimates
# package
#install.packages("MASS")
library(MASS)

# Example
set.seed(1066)
x <- rlnorm(1000, 5, 2)
head(x)

n <- length(x)
(m <- mean(log(x)))
(s <- sqrt((n-1) / n * var(log(x))))


# lnL <- sum(dlnorm(x, mu, sigma, log = TRUE))
# lnL <- sum(log(dlnorm(x, mu, sigma)))

f <- function(params) {
  -sum(dlnorm(x, params[1], params[2],log = TRUE))
}

f1 <- function(data, params) {
  -sum(dlnorm(data, params[1], params[2],log = TRUE))
}

p <- c(100, 100)
f(p)
f1(data = x, p)
MLE <- nlm(f, p); MLE
MLE <- nlm(f1, p, data = x); MLE
head(warnings())

p <- c(1, 1)
MLE <- nlm(f, p); MLE

fitdistr(x, "lognormal")

# Question part (i)
mu <- log(mean(x)) - 0.5*log(1 + var(x)/mean(x)^2)
sigma <- (log(1 + var(x)/mean(x)^2))^0.5
mu; sigma

# Question part (ii)
p <- c(mu, sigma)
MLE <- nlm(f, p); MLE

# Question
set.seed(1812)
x <- rgamma(1000, rpois(1, 100),rexp(1, 5))

f <- function(params) {
  -sum(dgamma(x, params[1], params[2], log = TRUE))
}
p <- c(111, 0.189)
f(p)
MLE <- nlm(f, p); MLE

p <- c(1, 1)
MLE <- nlm(f, p); MLE

fitdistr(x, "gamma", list(shape = 111, rate = 0.189))
fitdistr(x, "gamma", list(shape = 111, rate = 0.189), lower = 0)


# Question
dpareto <- function(x, a, lambda) {
  a*(lambda^a)/((lambda + x)^(a + 1))
}

sum(dpareto(x, params[1], params[2], log = TRUE))
f <- function(params) {
  -sum(log(dpareto(Losses, params[1], params[2])))
}
p <-c(5, 1500)
MLE <- nlm(f, p); MLE

# Analysing goodness of fit
# Histogram example
hist(Losses, freq = FALSE)
x <- seq(0, 6500)
a <- MLE$estimate[1]
l <- MLE$estimate[2]
y <- dpareto(x, a, l)
lines(x, y, col = "red")
curve(dpareto(x, a, l), add = TRUE, col = "red")

legend("topright", 
       legend = c("histogram of losses", "PDF of fitted Pareto distn"),
       col = c("black", "red"), lty = 1)


# Plotting the empirical density function - example
density(Losses, from = 0, to = 3000)
lines(density(Losses, from = 0, to = 3000), col = "blue") 

hist(Losses, freq = FALSE, ylim = c(0, 0.002))
curve(dpareto(x, a, l), add = TRUE, col = "red")
lines(density(Losses, from = 0, to = 3000), col = "blue") 
legend("topright", 
       legend = c("histogram of losses", "PDF of fitted Pareto distn", "Empirical density"),
       col = c("black", "red", "blue"), lty = 1)

plot(density(Losses, from = 0,to = 3000), xlab = "Losses", main =
       "Empirical density function
versus
Pa(3.416,1047)",col = "blue")

# Plotting the empirical density function - question
x <- seq(0, 3000)
lines(x, y)

# Plotting the empirical density function - example (continued)
y <- dpareto(x, a, l)
lines(x, y, col = "red")
curve(dpareto(x, a, l), add = TRUE, col = "red")

legend("topright", 
       legend = c("empirical density", "PDF of fitted Pareto distn"),
       col = c("blue", "red"), lty = 1)

# Q-Q plots
head(airquality)
ozone <- airquality$Ozone
summary(ozone)
ozone <- ozone[!is.na(ozone)]
mean.ozone <- mean(ozone)
sd.ozone <- sd(ozone)

# Fitting a normal distribution
mean.ozone; sd.ozone

n <- length(ozone)
comparison.qs <- qnorm(ppoints(n), mean.ozone, sd.ozone)

qqplot(comparison.qs, ozone,
       xlab = "Quantiles from fitted normal distribution",
       ylab = "Sample quantiles", main = "Q-Q plot of ozone data")
abline(0, 1, col = "red")

qqnorm(ozone, xlab = "Quantiles from fitted normal distribution",
       ylab = "Sample quantiles", main = "Q-Q plot of ozone data")
qqline(ozone, col = "red")

# Fitting a gamma distribution
shape <- mean.ozone^2/sd.ozone^2
rate <- mean.ozone/sd.ozone^2
shape; rate

comparison.qs <- qgamma(ppoints(n), shape, rate)

qqplot(comparison.qs, ozone,
       xlab = "Quantiles from fitted gamma distribution",
       ylab = "Sample quantiles", main = "Q-Q plot of ozone data")
abline(0, 1, col = "blue")

# Q-Q plot question
# import data
Losses <- read.table("Losses.txt", header = TRUE)
Losses <- Losses[,1]

# write function for the PDF of the Pareto distribution
dpareto <- function(x,a,lambda) {
  a*(lambda^a)/((lambda + x)^(a + 1))
}

# write the negative log-likelihood function
f <- function(params) {
  -sum(log(dpareto(Losses, params[1], params[2])))
}

# set initial parameter values and find MLEs
p <- c(5, 1500)
MLE <- nlm(f, p); MLE
a <- MLE$estimate[1]
l <- MLE$estimate[2]

qpareto <- function(p, a, lambda) {
  lambda * ((1 - p)^(-1/a) - 1)
}

n <- length(Losses)
comparison.qs <- qpareto(ppoints(n), a, l)

qqplot(comparison.qs, Losses,
       xlab = "Quantiles from Pa(3.416,1047) distribution",
       ylab = "Sample quantiles", main = "Q-Q plot of Losses")
abline(0, 1, col = "blue")

# Question 15.5

# Part (i)
data <- read.table("ClaimSize.csv", header = T)
datavector <- data[,1]
alpha <- seq(1.7, 2, 0.1)
beta <- seq(0.0015, 0.002, 0.0001)
result <- numeric(0)
for (i in 1:length(alpha)) {
  for (j in 1:length(beta)) {
    logL <- dgamma(datavector, shape = alpha[i], rate = beta[j], log = TRUE)
    sumL <- sum(logL)
    result<-rbind(result, c(alpha[i], beta[j], sumL))
  }
}
result
result[result[,3] == max(result[,3]),]

# Part (ii)
summary(datavector)
hist(datavector, freq = FALSE, breaks = 20, xlim = c(0, 5000),
     ylim = c(0, 0.001), ylab = "density")
lines(seq(0:5000), dgamma(seq(0:5000), shape = 2.0, rate = 0.002),
      col="blue")
legend("topright", legend = c("Histogram of data", "Fitted gamma distn"),
       col = c("black", "blue"), lty = 1)

# Question 15.6

# Part (i)
claims <- read.table("exp.txt", header = T)
head(claims)
claims <- claims[,1]
f <- function(param) {
  -sum(dexp(claims, param, log = TRUE))
}
p <- 0.002
f(p)
MLE <- nlm(f, p); MLE

library(MASS)
fitdistr(claims, "exponential")
1/mean(claims)

# Part (ii)
plot(density(claims, from = 0, to = max(claims)),
     xlab = "claims", main = "Empirical density function and
     fitted PDF", col = "blue")
max(claims)
x <- seq(0, 5613)
y <- dexp(x, MLE$estimate)
lines(x, y, col = "red")

legend("topright", legend = c("empirical density", "fitted exp PDF"),
       col = c("blue", "red"), lty = 1)

# Question 15.7

x <- read.table("MotorClaims.txt", header = T)
head(x)
x <- x[,1]

n <- length(x)
comparison.qs = qexp(ppoints(n), 0.003)

qqplot(comparison.qs, x, xlab = "Sample quantiles",
       ylab = "Quantiles of exp(0.003)", main = "Q-Q plot")

abline(0,1,col="red")