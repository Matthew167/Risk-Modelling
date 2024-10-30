
# Example 1
# Randomly chosen policy
sims <- 10000
S <- rep(NA, sims)

set.seed(123)

lambda <- sample(x = c(0.1, 0.3), replace = TRUE, size = sims, prob = c(0.5, 0.5))
N <- rpois(sims, lambda)

for (i in 1:sims){
  S[i] <- sum(rgamma(N[i], 750, 0.25))
}
mean(S)
sd(S)


# aggregate portfolio claims
sims <- 10000
policies <- 100
S <- rep(NA, policies)

Results <- matrix(nrow = sims, ncol = policies)

set.seed(123)

for (i in 1:sims){
  
  lambda <- sample(x = c(0.1, 0.3), replace = TRUE, size = policies, prob = c(0.5, 0.5))
  
  N <- rpois(policies, lambda)
  
  for (j in 1:policies){
    S[j] <- sum(rgamma(N[j], 750, 0.25))
  }
  
  Results[i, ] <- S
  
}

Total <- rowSums(Results)

mean(Total)
sd(Total)



# Example 2
# Number of claims from a randomly chosen policy

sims <- 10000
set.seed(123)

lambda <- rgamma(sims, 0.1, 1)
N <- rpois(sims, lambda)

mean(N)
sd(N)


# Claim amounts from a Randomly chosen policy
sims <- 10000
S <- rep(NA, sims)

set.seed(123)
lambda <- rgamma(sims, 0.1,1)
N <- rpois(sims, lambda)

for (i in 1:sims){
  S[i] <- sum(rgamma (N[i], 750, 0.25)) 
}

mean(S)
sd(S)


# aggregate portfolio claims
sims <- 10000
policies <- 100
S <- rep(NA, policies)

Results <- matrix(nrow = sims, ncol = policies)

set.seed(123)

for (i in 1:sims){
  
  lambda <- rgamma(policies, 0.1, 1)
  
  N <- rpois(policies, lambda)
  
  for (j in 1:policies){
    S[j] <- sum(rgamma(N[j], 750, 0.25))
  }
  
  Results[i, ] <- S
  
}

Total <- rowSums(Results)
mean(Total)
sd(Total)



# Example 3
# Randomly chosen policy
sims <- 10000
S <- rep(NA, sims)

set.seed(123)

lambda <- sample(x = c(0.1, 0.3), replace = TRUE, size =
                   sims, prob = c(0.5, 0.5))
N <- rpois(sims, lambda)

for (i in 1:sims){
  S[i] <- sum(rgamma(N[i], 750, 0.25))
}

mean(S)
sd(S)


# aggregate portfolio claims


sims <- 10000
policies <- 100
S <- rep(NA, policies)

Results <- matrix(nrow = sims, ncol = policies)

set.seed(123)

for (i in 1:sims){
  
  lambda <- sample(x = c(0.1, 0.3), replace = TRUE, size = 1,
                   prob = c(0.5, 0.5))
  
  N <- rpois(policies, lambda)
  
  for (j in 1:policies) {
    S[j] <- sum(rgamma(N[j], 750, 0.25))
  }
  
  Results[i, ] <- S
  
}

Total <- rowSums(Results)

mean(Total)
sd(Total)


# Question 19-20.6
# (i) simulations of pi
set.seed(42)

sims <- 5000

pi <- rbeta(sims, 8,5)

head(pi, 10)


# (ii) simulations of the number of claims
n <- rbinom(sims, 50, pi)

mean(n)
# [1] 30.825

sd(n)
# [1] 7.360734


# (iii) simulations of claims for one policy
s <- rep(NA, sims)

for (i in 1:sims){
  x <- rexp(n[i], 0.01)
  s[i] <- sum(x)
}

mean(s)
# [1] 3075.395
sd(s)
# [1] 928.3324


# (iv) simulations of claims for portfolio
set.seed(42)

policies <- 70

results <- matrix(nrow = sims, ncol = policies)

sim.s <- rep(NA, policies)

for (i in 1:sims){
  
  sim.pi <- rbeta(policies, 8,5)
  sim.n <- rbinom(policies, 50, sim.pi)
  
  for (j in 1:policies){
    sim.s[j] <- sum(rexp(sim.n[j], 0.01))
  }
  
  results[i, ] <- sim.s
  
}

total = rowSums(results)

mean(total)
# [1] 215410.1
sd(total)
# [1] 7583.685