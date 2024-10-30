 
# i)
 

# Basic R functions

p0 = 0.85
p1 = 0.11

P = matrix(c(1 - p0, p0, 0, 0,
             1 - p0, 0, p0, 0,
             1 - p0 - p1, p1, 0, p0,
             0, 1 - p0 -p1, p1, p0),
           nrow = 4, ncol = 4, byrow = TRUE)

rownames(P) = c("Level 1", "Level 2", "Level 3", "Level 4")
colnames(P) = rownames(P)

P

# Package functionality

library(markovchain)
mc = new("markovchain", transitionMatrix = P, 
         states = c("Level 1", "Level 2", "Level 3", "Level 4"))
mc

 
# ii)
 

Pn = function(P,n) {
  M = diag(nrow(P)) 
  for (i in 1:n) {
    M = M %*% P 
  }
  dimnames(M) = dimnames(P)
  M
}

 
# iii)
 

# Basic R functions

Pn(P,4)

Pn(P,7)

# Package functionality

mc ^ 4

mc ^ 7

 
# iv)
 

# Basic R functions

p0 = 0.83
p1 = 0.12
P2 = matrix(c(1 - p0, p0, 0, 0,
              1 - p0, 0, p0, 0,
              1 - p0 - p1, p1, 0, p0,
              0, 1 - p0 -p1, p1, p0),
            nrow = 4, ncol = 4, byrow = TRUE)

rownames(P2) = rownames(P)
colnames(P2) = rownames(P)

P2

Pn(P2,30)

Pn(P2,30)[1,]

# package functionality

mc2 = new("markovchain", transitionMatrix = P2, 
          states = c("Level 1", "Level 2", "Level 3", "Level 4"))

steadyStates(mc2)

 
# v)
 

prem.inc = function(n, start, income, P){
  pos = start
  prem = 0
  for (j in 1:n){
    prem = prem + sum(pos * income)
    pos = pos %*% P 
  }
  prem
}

 
# vi)
 

# Basic R functions

discount = c(0, 0.15, 0.25, 0.3)
income = 750 * (1 - discount)

prem.inc(5, c(1,0,0,0), income, P2)

prem.inc(10, c(1, 0, 0, 0), income, P2)

prem.inc(10, c(0, 0, 0, 1), income, P2)

# Package functionality

c(1,0,0,0) %*% expectedRewards(mc2, 4, income)
c(1,0,0,0) %*% expectedRewards(mc2, 9, income)
c(0,0,0,1) %*% expectedRewards(mc2, 9, income)

 
# vii)
 

# Basic R functions

P3 = matrix(c(1 - p0, p0, 0, 0,
              1 - p0, 0, p0, 0,
              1 - p0 - p1, p1, 0, p0,
              0, 0, 1 - p0 -p1, p0 + p1),
            nrow = 4, byrow = TRUE)

rownames(P3) = rownames(P)
colnames(P3) = rownames(P)

P3

prem.inc(10, c(0, 0, 0, 1), income, P3)

# Package functionality

mc3 = new("markovchain", transitionMatrix = P3, 
          states = c("level 1", "level 2", "level 3", "level 4"))

c(0,0,0,1) %*% expectedRewards(mc3, 9, income)

 
# viii)
 

# Basic R functions

(income2 = income + c(0, 0, 0, 10))

(prem.10 = prem.inc(10, c(0, 0, 0, 1), income2, P3))

(income3 = income + c(0, 0, 0, 15))

(prem.15 = prem.inc(10, c(0, 0, 0, 1), income3, P3))

# Package functionality

(prem.10 = c(0,0,0,1) %*% 
    expectedRewards(mc3, 9, income + c(0, 0, 0, 10)))

(prem.15 = c(0,0,0,1) %*% 
    expectedRewards(mc3, 9, income + c(0, 0, 0, 15)))


 
# x)
 

(5408.8624 - prem.10) / (prem.15 - prem.10) * (15 - 10) + 10

(income4 = income + c(0, 0, 0, 13.145161))

prem.inc(10, c(0, 0, 0, 1), income4, P3)

c(0,0,0,1) %*% expectedRewards(mc3, 9, income + c(0, 0, 0, 13.145161))