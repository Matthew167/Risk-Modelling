
# (i)
Grad = read.csv("Graduation_Q1.csv")
Grad
Grad$CRUDE = Grad$DEATHS / Grad$ETR

plot(Grad$AGE, Grad$CRUDE, 
     xlab = "Age", 
     ylab = "Mortality rate",
     main = "Crude and graduated mortality rates for
     male population in a region of the UK")
lines(Grad$AGE, Grad$GRAD, col = "red")
legend("topleft", legend = c("Crude rates", "Graduated rates"), 
       pch = c(1,NA), col = c("black", "red"), lty = c(NA, 1))


# (ii)
Grad$THIRD_DIFF = c(diff(Grad$GRAD, differences = 3), NA, NA, NA)
Grad


# (iii)a)
Grad$EXP = Grad$GRAD * Grad$ETR
Grad$ZX = (Grad$DEATHS - Grad$EXP) / sqrt(Grad$EXP)
sum(Grad$ZX^2)
(dof = nrow(Grad) - 3)
qchisq(0.95, dof)

# or
1 - pchisq(sum(Grad$ZX^2), dof)

# b)
length(Grad$ZX[Grad$ZX > 0])
table(sign(Grad$ZX))
binom.test(5, 10, p = 0.5, alt = "two.sided")

# c)
length(Grad$ZX[abs(Grad$ZX) > 2/3])
binom.test(5, 10, 0.5, alt = "greater")

# d)
(test.stat = (sum(Grad$DEATHS) - sum(Grad$EXP)) / sqrt(sum(Grad$EXP)))

# Or
2 * (1 - pnorm(test.stat))

# e)
(m = nrow(Grad))
(r1 = cor(Grad$ZX[-1], Grad$ZX[-m]))
r1 * sqrt(m)

# or

1 - pnorm(r1 * sqrt(m))
(r2 = cor(Grad$ZX[-(1:2)], Grad$ZX[-c(m,m-1)]))
r2 * sqrt(m)
qnorm(0.95)

#or
(alt.1 = acf(Grad$ZX, plot = FALSE)$acf[c(2,3)])
alt.1 * sqrt(m)

#or
(alt.2 = acf(Grad$ZX, plot = FALSE)$acf[c(2,3)] * m / c(m - 1, m - 2))
alt.2 * sqrt(m)

# f)
(n1 = length(Grad$ZX[Grad$ZX > 0]))
(n2 = length(Grad$ZX[Grad$ZX < 0]))
(signs = sign(Grad$ZX))
(G = sum(diff(c(-1, signs)) == 2))
prob = 0
for (t in 1:G){
  prob = prob + choose(n1-1, t-1) * choose(n2 + 1, t) / choose(m, n1)  
}
prob

# Or
phyper(G, n2 + 1 , n1 - 1, n1)