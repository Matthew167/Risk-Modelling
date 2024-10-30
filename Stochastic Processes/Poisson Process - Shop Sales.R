
# i)
L = 50
1 - ppois(199, 4*L)
ppois(199, 200, lower = FALSE)


# ii)
dpois(3,1.805)


# iii)
qpois(0.5,L/6)


# iv)
barplot(dpois(0:15,L/12),names.arg = 0:15, 
        main = "Distributuon of customers arriving in a 5-minute period", 
        xlab = "Number of customers", ylab = "Probability", col = "blue")


# v)(a)
hours = 8
set.seed(999)
weekly.customers = rpois(1000, 7*hours*L)

#(b)
hist(weekly.customers)

# vi)(a)
s = numeric(1000)

set.seed(123)
for(j in 1:1000){
  s[j] = 4*sum(rpois(weekly.customers[j], 1.805))
}

#(b)
length(s[s>19800])/length(s)


# vii)
hours = 8 + 1/6
set.seed(999)
weekly.customers = rpois(1000, 7*hours*L)

set.seed(123)
for (j in 1:1000){
  s[j] = 4*sum(rpois(weekly.customers[j], 1.805))
}
length(s[s>19800])/length(s)