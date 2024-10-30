
# i) a)
a0 = 0.00005887 
a1 = -0.0004988
b0 = -4.363378
b1 = 5.544956
b2 = -0.620345

mu <- function(x){
  t = (x-70)/50;
  a0 + a1*t + exp(b0 + b1*t + b2*(2*t^2 - 1))
}

# b)
mu(80)

# c)
plot(mu,20,110,log="y", lwd = 3, xlab="Age (x)",ylab="mu(x)",
     main="Mortality rates for AM92 (log scale)")

#or
plot(20:110, mu(20:110), type = "l" ,log="y", lwd = 3, 
     xlab="Age (x)", ylab="mu(x)",main="Mortality rates for AM92 (log scale)")


# ii) b)
(xlist = seq(20,110,by=10))
fnexp <- function(x) {1-exp(-mu(x+0.5))}
qlist = round(fnexp(xlist),6)
cbind(xlist,qlist)


# iii)
(1 - fnexp(25)) * (1 - fnexp(26))
prod(1 - fnexp(25:26))
pxs = 1 - fnexp(25:118)
head(pxs)
tp25s = cumprod(1 - fnexp(25:118))
head(tp25s)
sum(tp25s)