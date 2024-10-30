
# Measures of tail weight

# Limiting density ratios - example
x<-seq(0.001,0.999, by=0.001)
d1<-dbeta(x,2,0.5)
d2<-dbeta(x,3,0.8)
r<-d1/d2
plot(x,r,type="l",xlim=c(0.95,1),ylim=c(0.5,3),main="Density ratio")

# Hazard rates - example 1
# part (i)
mu<-10
sigma<-2
x<-seq(1,1000)
H<-dlnorm(x,meanlog=10,sdlog=2)/(1-plnorm(x,meanlog=10,sdlog=2))

# part (ii)
plot(x,H,type="l",main="Hazard rate for LogN(10,4)")


# Hazard rates - example 2
dburr <- function(x,a,lambda,g){
  (a*g*lambda^a)*x^(g-1)/((lambda+x^g)^(a+1))
}

pburr <- function(q,a,lambda,g){
  1-(lambda/(lambda+q^g))^a
}

x<-3
alpha<-0.5; lambda<-10; gamma<-2
H<-dburr(x,alpha,lambda,gamma)/
  (1-pburr(x,alpha,lambda,gamma))
H


# Mean residual life - example 1
x<-40
alpha<-50; lambda<-1.5
Sy<-function(y) {
  pgamma(y,shape=alpha,rate=lambda,lower.tail=FALSE)
}

int<-integrate(Sy,x,Inf)
ex<-int$value/pgamma(x,shape=alpha,rate=lambda,lower.tail=FALSE)
ex
ex<-int$value/Sy(x)
ex

# Mean resiudal life - example 2
x<-10
c<-0.001; gamma<-2
b<-c^(-1/gamma) 
Sy<-function(y) {
  pweibull(y,shape=gamma,scale=b,lower.tail=FALSE)
}

integrate(Sy,x,Inf)$value/pweibull(x,shape=gamma,scale=b,
                                   lower.tail=FALSE)
integrate(Sy,x,Inf)$value/Sy(x)

# Question 16.4

x<-seq(0,2,by=0.1)
c<-0.4
g<-5
b<-c^(-1/g) 
H<-dweibull(x,g,b)/(1-pweibull(x,g,b))
H<-dweibull(x,g,b)/pweibull(x,g,b,lower.tail=FALSE)
plot(x,H,type="l",ylab="H(x)",
     main="Hazard rate for Weibull(0.4,5)")

# Question 16.5

dpareto <- function(x,a,lambda){
  a* (lambda^a)/((lambda+x)^(a+1))
}

x<-c(1:1000)
d1<- dpareto(x,2,4)
d2<- dpareto(x,7,12)
r<-d1/d2
plot(r,main="Density ratios",xlab="X",ylab="ratio",type="l")