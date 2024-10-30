
# Individual risk model

# Simulating claims using the individual risk model
# Example
set.seed(54321)
a.s <- 200; l.s <- 0.8; q.s <- 0.002
a.n <- 300; l.n <- 0.4; q.n <- 0.001

x <- c(rgamma(250, shape = a.s, rate = l.s),
       rgamma(750, shape = a.n, rate = l.n))

death <- c(rbinom(250, size = 1, prob = q.s),
           rbinom(750, size = 1, prob = q.n))

sim <- x*death
sim <- sum(sim)

# summary
set.seed(54321)

a.s <- 200; l.s <- 0.8; q.s <- 0.002
a.n <- 300; l.n <- 0.4; q.n <- 0.001

sims <- 500
S.sim <- rep(NA, sims)

for (j in 1:sims) {
  
  x <- c(rgamma(250, shape = a.s, rate = l.s), rgamma(750, shape = a.n, rate = l.n))
  
  death <- c(rbinom(250, size = 1, prob = q.s), rbinom(750, size = 1, prob = q.n))
  
  sim <- x*death
  sim <- sum(sim)
  S.sim[j] <- sim
  
}

quantile(S.sim, 0.9)

# Question
set.seed(54321)

rate1 <- 0.005; q.1 <- 0.001
rate2 <- 0.006; q.2 <- 0.002
rate3 <- 0.007; q.3 <- 0.003

sims <- 5000
S.sim <- rep(NA, sims)

for (j in 1:sims) {
  
  x <- c(rexp(30, rate1), rexp(50, rate2), rexp(20, rate3))
  
  death <- c(rbinom(30, 1, q.1), rbinom(50, 1, q.2), rbinom(20, 1, q.3))
  
  sim <- x*death
  sim <- sum(sim)
  S.sim[j] <- sim
}
length(S.sim[S.sim > 100])/length(S.sim)

# Excess of loss reinsurance
y <- pmin(x, 200)
sim <- y*death
set.seed(54321)

a.s <- 200; l.s <- 0.8; q.s <- 0.002
a.n <- 300; l.n <- 0.4; q.n <- 0.001

sims <- 500
S.sim <- rep(NA, sims)

for (j in 1:sims) {
  
  x <- c(rgamma(250, shape = a.s, rate = l.s), rgamma(750, shape = a.n, rate = l.n))
  
  y <- pmin(x,200)
  
  death <- c(rbinom(250, size = 1, prob = q.s), rbinom(750, size = 1, prob = q.n))
  
  sim <- y*death
  sim <- sum(sim)
  S.sim[j] <- sim
}
quantile(S.sim, 0.9)

# Proportional reinsurance
# Question

set.seed(54321)

a.s <- 200; l.s <- 0.8; q.s <- 0.002
a.n <- 300; l.n <- 0.4; q.n <- 0.001

sims <- 500
S.sim <- rep(NA, 500)

for (j in 1:500) {
  
  x <- c(rgamma(250, shape = a.s, rate = l.s), rgamma(750, shape = a.n, rate = l.n))
  
  z <- 0.2*x
  
  death <- c(rbinom(250, size = 1, prob = q.s), rbinom(750, size = 1, prob = q.n))
  
  sim <- z*death
  sim <- sum(sim)
  S.sim[j] <- sim
}

mean(S.sim)

# Question 19-20.4

probability.of.claim <- read.table("IRM.txt", header = T)[, 2]
exponential.parameter <- read.table("IRM.txt", header = T)[, 3]

# Part (i)
E.Si <- probability.of.claim/exponential.parameter
sum(E.Si)

# Part (ii)

# a)
death <- rbinom(500, 1, probability.of.claim)
x <- rexp(500, rate = exponential.parameter)
sim <- x*death
sim <- sum(sim)

set.seed(5)
sims <- 10000
S.sim <- rep(NA, sims)

for (i in 1:sims) {
  death <- rbinom(500, 1, probability.of.claim)
  x <- rexp(500, rate = exponential.parameter)
  sim <- x*death
  sim <- sum(sim)
  S.sim[i] <- sim    
}

#b)

lower.bound <- sum(E.Si)*0.9
lower.bound
upper.bound <- sum(E.Si)*1.1
upper.bound

length(S.sim[S.sim >= lower.bound & S.sim <= upper.bound])/length(S.sim)

# Part (iii)(a)
q <- 1 - probability.of.claim
prod(q)
instances.of.0 <- length(S.sim[S.sim == 0])
instances.of.0
(instances.of.0/10000)/prod(q) - 1

# Part (iii)(b)
summary(probability.of.claim)

# Part (iii)(b) alternative solution
hist(S.sim, breaks = 500, freq = FALSE, xlim = c(0, 60000),
     ylim = c(0, 0.0002), main = "Simulated PDF of S")

# Question 19-20.5

set.seed(1812)
q <- runif(200, 0, 0.01)
mu <- runif(200, 5, 13)
sigma <- runif(200, 0.5, 0.8)
policies <- cbind(q, mu, sigma)

# Part (i)
set.seed(1854)

sims <- 5000
SR.sim <- rep(NA, sims)

for (i in 1:sims) {
  death <- rbinom(200, 1, policies[, 1])
  x <- rlnorm(200, meanlog = policies[, 2], sdlog = policies[, 3])
  z <- pmax(0, x - 60000)
  sim <- z*death
  sim <- sum(sim)
  SR.sim[i] <- sim    
}
mean(SR.sim)

# Part (ii)
set.seed(1854)

sims <- 5000
S.sim <- rep(NA, sims)

for (i in 1:sims) {
  death <- rbinom(200, 1, policies[, 1])
  x <- rlnorm(200, meanlog = policies[, 2], sdlog = policies[, 3])
  sim <- x*death
  sim <- sum(sim)
  S.sim[i] <- sim    
}
SR.sim <- pmax(0, S.sim - 300000)
mean(SR.sim)