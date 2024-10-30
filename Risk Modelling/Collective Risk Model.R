# Chapter 19 Course Notes
# Collective risk model

# Core Reading example
set.seed(123)

sims = 10000

n = rpois(sims, 1000)
s = rep(NA, sims)

x = rgamma(n[1], shape = 750, rate = 0.25)
sum(x)
s[1] = sum(x)

x <- rgamma(n[2], shape = 750, rate = 0.25)

s[2] = sum(x)

x = rgamma(n[3], shape = 750, rate = 0.25)

s[3] = sum(x)

for(i in 1:sims) {
  x = rgamma(n[i], shape = 750, rate = 0.25)
  s[i] = sum(x) 
}

# Summary
set.seed(123)

sims = 10000

n = rpois(sims, 1000)
s = rep(NA, sims)

for(i in 1:sims) {
  x = rgamma(n[i], shape = 750, rate = 0.25)
  s[i] = sum(x) 
}

# Question
set.seed(8143)
sims = 1000

n = rpois(sims,100)
s = rep(NA, sims)

for(i in 1:sims) {
  x = rexp(n[i], rate = 0.002)
  s[i] = sum(x) 
}

s[98]

# Using simulated data
set.seed(123)

sims = 10000

n = rpois(sims, 800)
s = rep(NA, sims)

for(i in 1:sims) {
  x = rgamma(n[i], shape = 900, rate = 2)
  s[i] = sum(x) 
}


length(s[s > 350000])/length(s)

# Question
mean(s)
var(s)
quantile(s, 0.5)

# Question
skewness = sum((s - mean(s))^3)/length(s)
skewness / var(s)^(3/2)

# Fitting a distribution to compound Poisson claims data
# Example
set.seed(123)

sims = 10000

n = rpois(sims, 1000)
s = rep(NA, sims)

for(i in 1:sims) {
  x = rgamma(n[i], shape = 750, rate = 0.25)
  s[i] = sum(x) 
}

(mu = mean(s))
(sigma = sd(s) * sqrt((sims - 1) / sims))

library(MASS)
fitdistr(s, "normal")

# Analysing gooness of fit
hist(s, main = "Simulated aggregate claims data", 
     xlab = "aggregate claims",
     ylab = "density",
     prob = TRUE)

lines(density(s), col = "blue")
curve(dnorm(x, mu, sigma), add = TRUE, col = "red")

legend("topright", legend = c("histogram of sample",
                              "empirical density", 
                              "fitted normal distn"),
       col = c("black", "blue", "red"), lty = 1)

qqnorm(s, main = "Q-Q plot comparing quantiles of simulated 
compound distribution values against normal quantiles")
qqline(s, col = "red")



# The compound binomial distribution
# Example
set.seed(123)

sims = 10000

n = rpois(sims, 1000)
s = rep(NA, sims)

for(i in 1:sims) {
  x = rgamma(n[i], shape = 750, rate = 0.25)
  s[i] = sum(x) 
}

set.seed(123)

sims = 2000

n = rbinom(sims, size = 500, prob = 0.7)
s = rep(NA, sims)

for(i in 1:sims) {
  x <- rnorm(n[i], mean = 1000, sd = 32)
  s[i] <- sum(x) 
}

summary(s)

# Question
set.seed(123)

sims = 10000

n = rbinom(sims, size = 80, prob = 0.3)
t = rep(NA, sims)

rpareto <- function(n, a, lambda){
  lambda * ((1 - runif(n))^(-1/a) - 1)
}

for(i in 1:sims) {
  x = rpareto(n[i], a = 4, lambda = 5)
  t[i] <- sum(x) 
}

tail(t, 5)

# Using simulated data
# Question 
quantile(t, 0.95)

# Fitting a distribution to  compound binomial claims data
# Question
f <- function(params) {
  lnL <- dgamma(t, shape = params[1], rate = params[2], log = TRUE)
  sum(-lnL)
}

p <- c(10, 10)
nlm<-nlm(f, p); nlm

library(MASS)
fitdistr(t, "gamma")

# Analysing goodness of fit
a <- nlm$estimate[1]
l <- nlm$estimate[2]

comparison.qs = qgamma(ppoints(length(t)), a, l)
qqplot(comparison.qs, t, main = "Q-Q plot comparing quantiles of simulated 
       compound distribution values against gamma quantiles",
       ylab = "quantiles of sample",
       xlab = "quantiles of fitted gamma distribution")

abline(0, 1, col = "red")

# Question
plot(density(t), xlim = c(90, 350), ylim = c(0, 0.001),
     main = "Simulated data
     versus
     fitted distribution", xlab = "Aggregate claim size", col = "blue")
agg <- seq(from = 80, to = 350)
y <- dgamma(agg, shape = a, rate = l)
lines(agg, y, col = "red")
legend("topright", legend = c("empirical density", "fitted gamma density"),
       col = c("blue", "red"), lty = 1)

plot(density(t), main =
       "Simulated data
     versus
     fitted distribution", xlab = "Aggregate claim size", col = "blue")
agg <- seq(from = 0, to = 150)
y <- dgamma(agg, shape = a, rate = l)
lines(agg, y, col = "red")
legend("topright", legend = c("empirical density", "fitted gamma density"),
       col = c("blue", "red"), lty = 1)

# The compound negative binomial distribution
# Example
set.seed(123)

sims = 10000

n = rpois(sims, 1000)
s = rep(NA, sims)

for(i in 1:sims) {
  x = rgamma(n[i], shape = 750, rate = 0.25)
  s[i] = sum(x) 
}


set.seed(123)

sims = 1000

n = rnbinom(sims, size = 700, prob = 0.2)
s = rep(NA, sims)

for(i in 1:sims) {
  x = rlnorm(n[i], meanlog = 4, sdlog = 3)
  s[i] <- sum(x) 
}

summary(s)

# Question
set.seed(123)

sims = 10
n = rnbinom(sims, size = 5, prob = 0.8)
w = rep(NA, sims)

rburr <- function(n, a, lambda, g){
  (lambda*((1 - runif(n))^(-1/a) - 1))^(1/g)
}

for(i in 1:sims) {
  x <- rburr(n[i], a = 0.3, lambda = 10, g = 1.2)
  w[i] <- sum(x) 
}

w

# Using simulated data
# Question
length(w[w > 2000])/length(w)

# Fitting a distribution to compound negative binomial claims data
# Question
set.seed(19)

sims = 100

n = rnbinom(sims, size = 20, prob = 0.1)
s = rep(NA, sims)

for(i in 1:sims) {
  x = rlnorm(n[i], meanlog = 3, sdlog = 0.8)
  s[i] = sum(x) 
}

(mu = mean(s))
(sigma = sd(s) * sqrt((sims - 1)/ sims))

# Analysing goodness of fit
# Question

comparison.qs = qnorm(ppoints(length(s)), mu, sigma)

qqplot(comparison.qs, s, main = "Q-Q plot comparing quantiles of simulated 
compound distribution values against fitted normal quantiles",
       xlab = "Fitted distn quantiles",
       ylab = "Sample quantiles")
abline(0, 1, col = "red")

# Question
plot(density(s),main="Simulated data
     versus
     fitted distribution",xlab="Aggregate claim size",col="blue")
agg <- seq(from = 2000, to = 8500)
y <- dnorm(agg, mean = mu, sd = sigma)
lines(agg, y, col = "red")
legend("topright", legend = c("empirical density", "fitted normal density"),
       col = c("blue", "red"), lty = 1)

# Excess of loss reinsurance
# Example
set.seed(123)

sims = 10000

n = rpois(sims, 1000)
sI = rep(NA, sims)

M = 2500

for(i in 1:sims) {
  x = rgamma(n[i], shape = 750, rate = 0.25)
  y = pmin(x, M)
  sI[i] <- sum(y) 
}

mean(sI)

# Question
# part (i)
set.seed(123)

sims = 10000

n = rpois(sims, 1000)
sR = rep(NA, sims)

M = 2500

for(i in 1:sims) {
  x = rgamma(n[i], shape = 750, rate = 0.25)
  z = pmax(0, x - M)
  sR[i] <- sum(z) 
}


# part (ii)
mean(sR)

# Aggregate excess of loss reinsurance
# Question
set.seed(123)

sims = 10000

n = rpois(sims, 1000)
s = rep(NA, sims)

for(i in 1:sims) {
  x = rgamma(n[i], shape = 750, rate = 0.25)
  s[i] <- sum(x) 
}

SR <- pmax(0, s - 3000000)

# Proportional reinsurance
# Example
set.seed(123)

sims = 1000

n = rnbinom(sims, size = 700, prob = 0.2)
s = rep(NA, sims)
for(i in 1:sims) {
  x <- rlnorm(n[i], meanlog = 4, sdlog = 3)
  s[i] <- sum(x) 
}

# Question
# Part (i)
set.seed(123)

sims = 1000

n = rnbinom(sims, size = 700, prob = 0.2)
sI = rep(NA, sims)

for(i in 1:sims) {
  x = rlnorm(n[i], meanlog = 4, sdlog = 3)
  y = 0.9*x
  sI[i] <- sum(y) 
}
sd(sI)

# Part (ii)
set.seed(123)

sims = 1000

n = rnbinom(sims, size = 700, prob = 0.2)
sR = rep(NA, sims)

for(i in 1:sims) {
  x = rlnorm(n[i], meanlog = 4, sdlog = 3)
  z = 0.1*x
  sR[i] <- sum(z) 
}
length(sR[sR > 1500000])/length(sR)

# Question 19-20.1

# Part (i)
k = 10; p = 0.1
mu = 10; sigma = 5^0.5

set.seed(16)

sims = 10000

n = rnbinom(sims, size = k, prob = p)
s = rep(NA, sims)

for(i in 1:sims) {
  x = rlnorm(n[i], meanlog = mu, sdlog = sigma)
  s[i] = sum(x) 
}

head(s, 5)

# Part (ii)
sample.mean = mean(s)
sample.variance = var(s)

meanN = k*(1 - p)/p
varN = k*(1 - p)/p^2

meanX = exp(mu + 0.5*sigma^2)
varX = exp(2*mu + sigma^2)*(exp(sigma^2) - 1)

meanS = meanN*meanX
varS = meanN*varX + varN*meanX^2

c(meanS, sample.mean); c(varS,sample.variance)

meanS/sample.mean; varS/sample.variance

# Part (iii)
set.seed(16)

k = 10; p = 0.1
mu = 10; sigma = 5^0.5

M = 2000000

sims = 10000

n = rnbinom(sims, size = k, prob = p)
sI = rep(NA, sims)

for(i in 1:sims) {
  x = rlnorm(n[i], meanlog = mu, sdlog = sigma)
  y = pmin(x, M)
  sI[i] = sum(y) 
}

head(sI, 5)

# Question 19-20.2

# Part (i)
sims = 1000
set.seed(1975)
n = rpois(sims, 500)
s = rep(NA, sims)

for(i in 1:sims) {
  x = rexp(n[i], rate = 0.001)
  s[i] = sum(x) 
}

tail(s, 5)

# Part (ii)
(mu = mean(s))
(sigma = sd(s) * sqrt((sims - 1) / sims))

# Part (iii)
qqnorm(s, main = "Q-Q plot comparing quantiles of simulated 
compound distribution values against normal quantiles")
qqline(s, col = "red")


plot(density(s), main = "Simulated data
     versus
     fitted distribution", xlab = "Aggregate claim size", col = "blue")
agg <- seq(from = min(s), to = max(s))
y <- dnorm(agg, mean = mu, sd = sigma)
lines(agg, y, col = "red")


# Question 19-20.3

# setwd("<file directory>")

claims = read.csv("claims.csv", header = T, sep = ",")
claims$claim.date = as.Date(claims$claim.date, format("%d/%m/%Y"))
head(claims)


#------------------------------------------------------------------------
# (i)
#------------------------------------------------------------------------


claims$wait = c(NA, diff(claims$claim.date))

head(claims)

#------------------------------------------------------------------------
# (ii)
#------------------------------------------------------------------------

(L = 1 / mean(claims$wait[-1]))

#------------------------------------------------------------------------
# (iii)
#------------------------------------------------------------------------

hist(claims$wait[-1], freq = F, main = "Histogram of times between house insurance claims", xlab = "waiting times")

lines(0:100, dexp(0:100, L), col = "red", lwd = 2)

#------------------------------------------------------------------------
# (iv)
#------------------------------------------------------------------------

(obs = hist(claims$wait[-1],
            breaks = c(seq(0, 60, 10), Inf),
            plot = F)$counts)

c(seq(0, 60, 10), Inf)
pexp(c(seq(0, 60, 10), Inf), L)

(probs = diff(pexp(c(seq(0, 60, 10), Inf), L)))

(exp = probs * length(claims$wait[-1]))

obs.exp = data.frame(bins = c(">0-10", ">10-20", ">20-30",
                              ">30-40", ">40-50", ">50-60",
                              ">60"),
                     obs = obs,
                     exp = exp)
obs.exp

chisq.test(obs, p = exp, rescale.p = TRUE)

# chisq.test(obs, p = probs)

# sum((obs - exp)^2/exp)
# qchisq(0.95, 6)
# pchisq(sum((obs - exp)^2/exp), 6, lower = FALSE)

#------------------------------------------------------------------------
# (v)
#------------------------------------------------------------------------

(m = mean(claims$claim.cost))

(s = sd(claims$claim.cost))

(beta = m/s^2)

(alpha = beta*m)


#------------------------------------------------------------------------
# (vi)
#------------------------------------------------------------------------

(LY = 365 * L)

LY * m

sqrt(LY * (s^2 + m^2))