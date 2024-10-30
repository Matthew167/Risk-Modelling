

# 1.2 Example 1


A <- matrix(2, 2, data = c(0.2, 0.1, 0.4, 0.3))
A

eigen(A)
eigen(A)$values


# 1.3 Example 2


A <- matrix(2, 2, data = c(-0.6, 0.8, 0.5, -0.1))
A

eigen(A)$values


# 1.4 Example 3


A <- matrix(3, 3, data = c(0.7, 1, 3, 0.9, 0.2, 0.2, 0.4, 0.2, 0.7))
A
eigen(A)$values


# 2.2 Cointegrated example


xy <- read.table("cointegration.txt", sep = " ", header = TRUE)
head(xy)

x <- xy[,1]
y <- xy[,2]

PP.test(x) 
PP.test(x)$p.value
PP.test(diff(x))$p.value

PP.test(y)$p.value
PP.test(diff(y))$p.value

find.coint <- function(coint) {
  comb <- coint[1]*x + coint[2]*y
  test <- PP.test(comb)$p.value
  test
}

v <- c(1, 1)

fit <- nlm(find.coint, v)
fit

a <- fit$estimate[1]
b <- fit$estimate[2]

c(a, b)

comb <- a*x + b*y
PP.test(comb)$p.value

set.seed(1234)
z <- rep(0, 1000)
for (i in 2:1000) z[i] <- z[i-1] + rnorm(1)
w <- v <- rep(0, 1000)
w <- runif(1, 5, 7)*z + rnorm(1000)
v <- -runif(1, 2, 4)*z + rnorm(1000)

xp <- PP.test(x)$p.value >= 0.05 && PP.test(diff(x))$p.value < 0.05
yp <- PP.test(y)$p.value >= 0.05 && PP.test(diff(y))$p.value < 0.05

find.coint <- function(coint){
  comb <- coint[1]*x + coint[2]*y
  test <- PP.test(comb)$p.value
  test
}

v <- c(1, 1)
fit <- nlm(find.coint, v)
a <- fit$estimate[1]
b <- fit$estimate[2]
v.fit <- c(a, b)
v.fit.check <- round(a, 6) == 0 && round(b, 6) == 0

comb <- a*x + b*y
combp <- PP.test(comb)$p.value < 0.05

if (xp == TRUE && yp == TRUE && combp == TRUE && v.fit.check == FALSE){
  print("the vectors are cointegrated with cointegrating vector")
  print(v.fit)
} else {
  print("x and y are not cointegrated")
}

is.cointegrated <- function(x, y){
  
  xp<- PP.test(x)$p.value >= 0.05 && PP.test(diff(x))$p.value < 0.05
  yp<- PP.test(y)$p.value >= 0.05 && PP.test(diff(y))$p.value < 0.05
  
  find.coint <- function(coint){
    comb <- coint[1]*x + coint[2]*y
    test <- PP.test(comb)$p.value
    test
  }
  
  v <- c(1, 1)
  fit <- nlm(find.coint, v)
  a <- fit$estimate[1]
  b <- fit$estimate[2]
  v.fit <- c(a, b)
  v.fit.check <- round(a, 6) == 0 && round(b, 6) == 0
  
  comb <-a*x + b*y
  combp <- PP.test(comb)$p.value < 0.05
  
  if (xp == TRUE && yp == TRUE && combp == TRUE && v.fit.check == FALSE) {
    print("the vectors are cointegrated with cointegrating vector");
    print(v.fit)  
  } else {
    print("x and y are not cointegrated")
  }
}

set.seed(1234)
z <- rep(0, 1000)
for (i in 2:1000) z[i] <- z[i-1] + rnorm(1)
w <- v <- rep(0, 1000)
w <- runif(1,5,7)*z + rnorm(1000)
v <- -runif(1,2,4)*z + rnorm(1000)
is.cointegrated(w, v)


##  13-14.15


m <- matrix(c(0.3, -0.2, 0.1, 0.6), 2, 2); m
eigen(m)$values


##  13-14.16




# i)


set.seed(1234)
z <- rep(0, 1000)
for (i in 2:1000) z[i] <- z[i-1] + rnorm(1)

s <- runif(1, 16, 20)*z + rnorm(1000)
t <- -runif(1, 1, 10)*z + rnorm(1000)
PP.test(0.2*s + 1.5*t)$p.value


# ii)


PP.test(s)$p.value
PP.test(diff(s))$p.value
PP.test(t)$p.value; PP.test(diff(t))$p.value

find.coint <- function(coint) {
  comb <- coint[1]*s + coint[2]*t
  test <- PP.test(comb)$p.value
  test
}

v <- c(1, 1)
find.coint(v)

fit <- nlm(find.coint, v)
alpha <- fit$estimate[1]
beta <- fit$estimate[2]

alpha; beta
PP.test(alpha*s + beta*t)$p.value