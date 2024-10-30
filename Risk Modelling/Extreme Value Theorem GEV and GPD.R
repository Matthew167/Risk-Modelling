
# GEV and GPD distributions

# Data requirements
data<-read.table("EVTdata.txt")
head(data)
data<-read.table("EVTdata.txt",header=TRUE)
head(data)

# Block maxima
maxima<-aggregate(Claim~Year:Month,data,max)
head(maxima)
min(data[,2])
YearIndex<-(data[,2]-1990)%/%5+1
data<-cbind(data,YearIndex)
maxima<-aggregate(Claim~YearIndex,data,max); maxima
maxima<-aggregate(Claim~Year,data,max)
head(maxima)

# Fitting a GEV distribution
alpha<-10; beta<-10; gamma<-10
p<-c(alpha,beta,gamma)

dGEV <- function(x, a, b, g){
  1/b * (1 + g*(x - a)/b)^-(1 + 1/g) * 
    exp(-((1 + g*(x - a)/b)^(-1/g)))
}

fMLE <- function(maxima, params) {
  f<- dGEV(maxima, params[1], params[2], params[3])
  lnf<-log(f)
  sum(-lnf)
}

fMLE(maxima[,2], p) 
MLE <- nlm(fMLE, p, maxima = maxima[, 2]); MLE

# Using the fitted distribution
alpha <- MLE$estimate[1]
beta <-  MLE$estimate[2]
gamma <- MLE$estimate[3]
m<-30000
p <- 1-exp(-(1+gamma*(m-alpha)/beta)^(-1/gamma))
p

# Goodness of fit - histograms
hist(maxima[,2], freq=FALSE, main="Histogram", xlab="maxima") 

x<-seq(0,45000)

lines(x, dGEV(x, alpha, beta, gamma),col="blue")

# Goodness of fit - empirical densities
plot(density(maxima[,2]),ylim=c(0,0.000045),xlab="maxima",
     main="Empirical density function
     versus
     fitted distribution",col="blue")
x<-seq(from= -10000,to=50000)
lines(x, dGEV(x, alpha, beta, gamma),col="red")

# Goodness of fit - Q-Q plots
qGEV <- function(p, a, b, g) {
  b/g * ((-log(p))^(-g) - 1) + a
}

rGEV <- function(n, a, b, g) {
  b/g * ((-log(runif(n)))^(-g) - 1) + a
}

pGEV <- function(q, a, b, g) {
  exp(-(1 + g * (q - a)/b)^(-1/g))
}

n = length(maxima[, 2])
comparison.qs <- qGEV(ppoints(n), alpha, beta, gamma)

qqplot(comparison.qs, maxima[,2], 
       xlab = "Quantiles from fitted distribution",
       ylab = "Sample quantiles",
       main = "QQ plot of data and fitted GEV distribution")

abline(0,1,col="red")


# Generalised Pareto distribution
# Threshold exceedances
u <- 10000
x <- data[,3]
x <- x[x>u]
te <- x-u
te

x<-data[,3]
te<-x[x>10000]-10000; te

# Fitting a GPD distribution - question
beta<-10; gamma<-10
p<-c(beta,gamma)

dGPD <- function(te, b, g){
  1/b * ((1 + te/(b*g))^(-g - 1))
}

fMLE <- function(te, params) {
  f <- dGPD(te, params[1], params[2]) 
  lnf <- log(f)
  sum(-lnf)
}

fMLE(te, p)

MLE<-nlm(fMLE, p, te = te); MLE

# Fitting a GPD distribution - question
# part (i)
hist(te, freq=FALSE, xlab="threshold exceedances", main="Histogram
     versus
     fitted GPD distribution")

x<-seq(0,35000)

beta<-MLE$estimate[1]
gamma<- MLE$estimate[2]

lines(x, dGPD(x, beta, gamma))

# part (ii)
plot(density(te,from=min(te),to=max(te)),xlab="threshold exceedances",main="Empirical density function
     versus
     fitted GPD distribution",col="blue")

x<-seq(from=min(te),to=max(te))

lines(x, dGPD(x, beta, gamma), col = "red")

# part (iii)
qGPD <- function(p, b, g) {
  g * b * ((1 - p)^(-1/g) - 1)
}

n = length(te)
comparison.qs <- qGPD(ppoints(n), beta, gamma)

qqplot(comparison.qs, te,
       xlab = "Quantiles from fitted distribution",
       ylab = "Sample quantiles",
       main = " QQ plot of threshold exceedances")

abline(0,1,col="red")


rGPD <- function(n, b, g) {
  g * b * ((1 - runif(n))^(-1/g) - 1)
}

pGPD <- function(q, b, g) {
  1 - (1 + q / (b*g))^(-g)
}

# Question 16.1

# Part (i)
data<-read.table("table.txt",header=TRUE)
head(data)
maxima<-aggregate(Claim~Year:Month,data,max)
head(maxima)
required.year<-maxima[maxima[,1]==2016,]
required.year
required.year[required.year[,2]=="Jul",]
maxima[maxima[,1]==2016 & (maxima[,2]=="Jul"|maxima[,2]=="Aug"|maxima[,2]=="Sep"),]

# Part (i) alternative solution
maxima2 <- tapply(data$Claim,list(data$Year,data$Month),max)
maxima2
maxima2["2016","Jul"]; maxima2["2016","Aug"]; maxima2["2016","Sep"]

# Part (ii)

months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
data$month.no = match(data$Month, months)

head(data)

data$Quarter = (data$month.no - 1) %/% 3 + 1
maxima.q<-aggregate(Claim ~ Year:Quarter, data, max)
head(maxima.q)
maxima.q[maxima.q$Year==2016 & maxima.q$Quarter==3,]

# Question 16.2

aggClaims <- read.table("aggClaims.txt", header=TRUE)
head(aggClaims)
t<-3000000             

te<-pmax(aggClaims[,-1]-t,0)
te[te>0 & !is.na(te)]

te<-pmax(as.matrix(aggClaims[,-1]-t),0)
te[te>0]

x<-aggClaims[,-1]
xe<-x[x>t]-t
xe

# Alternative solution

aggClaims<-unlist(aggClaims[-1])
xe<-pmax(aggClaims-t,0)
xe[xe>0]


