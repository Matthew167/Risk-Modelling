 
# (i)
rates = read.table("mort_rates.txt", header = TRUE, row.names = 1)
rates

 
# (ii)a)
(ax = colMeans(log(rates)))

# b)
(ax.mat = matrix(rep(ax, each = 5), nrow = 5))
(log.centred = log(rates) - ax.mat)

# c)
(kt = rowSums(log.centred))

 
# (iii)a)
bx.calc = function(log.centred, kt){
  kt.mat = matrix(rep(kt, ncol(log.centred)), nrow = nrow(log.centred))
  colSums(kt.mat * log.centred) / sum(kt^2)
}

# b)
(bx = bx.calc(log.centred, kt))

# c)
sum(bx)
sum(kt)

 
# (iv)a)
lm.60 = lm(log.centred[, 1] ~ kt - 1)
lm.60 = lm(log.centred[, "X60"] ~ kt - 1)
lm.61 = lm(log.centred[, 2] ~ kt - 1)
lm.62 = lm(log.centred[, 3] ~ kt - 1)
lm.63 = lm(log.centred[, 4] ~ kt - 1)
lm.64 = lm(log.centred[, 5] ~ kt - 1)
lm.65 = lm(log.centred[, 6] ~ kt - 1)

lm.60$coefficients
lm.61$coefficients
lm.62$coefficients
lm.63$coefficients
lm.64$coefficients
lm.65$coefficients

output = sapply(1:6, function(i) { lm(log.centred[, i] ~ kt - 1)})
output

 
# (v)a)
kt.calc = function(log.centred, bx){
  bx.mat = matrix(rep(bx, each = nrow(log.centred)), nrow = nrow(log.centred))
  rowSums(bx.mat * log.centred) / sum(bx^2)
}

# b)
kt
bx

kt.calc(log.centred, bx)
bx.calc(log.centred, kt)



## Exercise 2
# (i)a)
mort.svd = svd(log.centred)
mort.svd

# b)
mort.svd$u %*% diag(mort.svd$d) %*% t(mort.svd$v)
log.centred

mort.svd$u %*% diag(mort.svd$d) %*% t(mort.svd$v) - log.centred

 
# (ii)
 

# a)

(kt.svd = mort.svd$u[, 1])
(bx.svd = mort.svd$d[1] * mort.svd$v[, 1])

kt.calc(log.centred, bx.svd)
bx.calc(log.centred, kt.svd)

# b)

sum(bx.svd)
sum(kt.svd)

# c)

(kt.svd = kt.svd * sum(bx.svd))
(bx.svd = bx.svd / sum(bx.svd))

sum(bx.svd)
sum(kt.svd)

kt.calc(log.centred, bx.svd)
bx.calc(log.centred, kt.svd)


## Exercise 3
 
# (i)
 

# install.packages("gnm")
library(gnm)

#a)

mort.df = read.table("mort_rates_df.txt", header = TRUE)
head(mort.df)

# b)

mort.df$lxt.centred = log(mort.df$mxt) - rep(ax, each = 5)
head(mort.df)

log.centred

# c)

set.seed(1)
gnm.LC = gnm(lxt.centred ~ Mult(as.factor(age), as.factor(year)) - 1, data = mort.df)
gnm.LC$coefficients

# d)

bx.gnm = gnm.LC$coefficients[1:6]
kt.gnm = gnm.LC$coefficients[7:11]

sum(bx.gnm)
sum(kt.gnm)

# e)

(kt.gnm = kt.gnm * sum(bx.gnm))
(bx.gnm = bx.gnm / sum(bx.gnm))

sum(kt.gnm)
sum(bx.gnm)

# f)

bx.calc(log.centred, kt.gnm)
kt.calc(log.centred, bx.gnm)

 
# (ii)
 

kt
kt.svd
kt.gnm

bx
bx.svd
bx.gnm


## Exercise 4
# (i)
plot(2010:2014, rates[, 1], type = "l", lwd = 2,
     main = "mortality rates for lives aged 60",
     xlab = "Year",
     ylab = "Mortality rate")

fitted.mort60 = exp(ax[1] + bx.svd[1] * kt)

lines(2010:2014, fitted.mort60, col = "red", lwd = 2)

legend("topright", legend = c("estimated mortality", "fitted mortality"),
       col = c("black", "red"), lwd = 2)

 
# (ii)
 

# a)

(mu = (kt[5] - kt[1]) / 4)
(kt.forecasts = kt[5] + mu * (2015:2030 - 2014))

# b)

plot(2010:2030, c(kt, kt.forecasts), type = "b", col = "red",
     lwd = 2,
     main = "Fitted and projected values of kt",
     xlab = "Year",
     ylab = "kt")
lines(2010:2014, kt, col = "black", lwd = 2, type = "b")

legend("topright", legend = c("estimated kt", "projected kt"),
       col = c("black", "red"), lwd = 2)

# c)

(proj.mort60 = exp(ax[1] + bx.svd[1] * kt.forecasts))

# d)

plot(2010:2030, c(fitted.mort60, proj.mort60), type = "b", col = "red",
     lwd = 2,
     main = "Estimated, fitted and projected mortality rates 
     at age 60 under LC model",
     xlab = "Year",
     ylab = "Mortality rate")

lines(2010:2014, fitted.mort60, col = "blue", lwd = 2, type = "b")

lines(2010:2014, rates[, 1], col = "black", lwd = 2, type = "b")

legend("topright", legend = c("estimated mxt", "fitted mxt", "projected mxt"),
       col = c("black", "red", "blue"), lwd = 2)


## Exercise 5
# (i)
data <- read.csv("ELT.csv", header = TRUE)
data

 
# (iii)a)
Rxt = function(x, t){
  alpha = min(max(0.13 + (1 - 0.13)*(x - 60)/50, 0.13), 1)
  f = min(max(0.55 - 0.26*(x - 60)/50, 0.29), 0.55)
  alpha + (1 - alpha)*(1 - f)^(t/20)
}

(ages = 0:10 * 10)
(ages = seq(0, 100, 10))

# b)
sapply(ages, function(x) Rxt(x, 20))

data$qx2011 = data$ELT15 * sapply(ages, function(x) Rxt(x, 20))
data$qx2011

data$qx2021 = data$ELT15 * sapply(ages, function(x) Rxt(x, 30))
data$qx2021


 
# (iv)
plot(ages, data$ELT17[data$SEX == "M"],
     log = "y", type = "l", lwd = 3,
     xlab = "Age", ylab = "", las = 1, 
     main = "Mortality rates for ELT17 (Males)")

lines(ages, data$qx2011[1:11], col = "red", lty = 2, lwd = 2)

legend(10, 0.1, c("ELT17", "Projected"),
       lty = c(1, 2), col = c("black", "red"))

plot(ages, data$ELT17[data$SEX == "F"], log = "y", 
     type = "l",lwd = 3,
     xlab = "Age", ylab = "", las = 1, 
     main = "Mortality rates for ELT17 (Females)")

lines(ages, data$qx2011[12:22], col = "red", lty = 2, lwd = 2)

legend(10, 0.1, c("ELT17", "Projected"),
       lty = c(1, 2), col = c("black","red"))