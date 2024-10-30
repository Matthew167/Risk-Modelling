
# (i)
hazard = function(x, t){
  b = 0.66933
  lambda = 0.2439
  lambda * exp(-lambda * t) * exp(b * x)
}

tvalues = seq(0,40,0.1)

plot(tvalues, hazard(1, tvalues), type = "l", lwd = 2, 
     main = "Fitted hazard of compromised and non-compromised patients",
     xlab = "Time",
     ylab = "Fitted hazard")

lines(tvalues, hazard(0, tvalues), type = "l", col = "red", lwd = 2)

legend("topright", legend = c("Compromised hazard", "Non-compromised hazard"),
       col = c("black", "red"), lty = 1)

# (ii)a)
PH_data = read.csv("PH_data_8.1.csv")
head(PH_data)

library(survival)

surv.obj = Surv(PH_data$Event.time, PH_data$Event.code)

cox.fit = coxph(surv.obj ~ Status + Ward, ties = "breslow", data = PH_data)
summary(cox.fit)

# b)
hazard2 = function(x, ward, t){
  b = cox.fit$coefficients[1]
  d = cox.fit$coefficients[2]
  lambda = 0.2439
  lambda * exp(-lambda * t) * exp(b * x + d * ward)
}

plot(tvalues, hazard2(1, 0, tvalues), type = "l", lwd = 2, 
     main = "Fitted hazard of Compromised patients on 
     ward 1 (W1) and ward 2 (W2)",
     xlab = "Time",
     ylab = "Fitted hazard")

lines(tvalues, hazard2(1, 1, tvalues), type = "l", col = "red", lwd = 2)

legend("topright", legend = c("Compromised hazard W1", "Compromised hazard W2"),
       col = c("black", "red"), lty = 1)

# c)
plot(tvalues, hazard2(0, 0, tvalues), type = "l", lwd = 2, 
     main = "Fitted hazard of non-compromised patients on 
     ward 1 (W1) and ward 2 (W2)",
     xlab = "Time",
     ylab = "Fitted hazard")

lines(tvalues, hazard2(0, 1, tvalues), type = "l", col = "red", lwd = 2)

legend("topright", legend = c("Non-compromised hazard W1", "Non-compromised hazard W2"),
       col = c("black", "red"), lty = 1)

# e)
cox.fit.status =  coxph(surv.obj ~ Status, ties = "breslow", data = PH_data)

anova(cox.fit.status, cox.fit, test = "Chi")

# Alternative

cox.fit$loglik
cox.fit.status$loglik

(obs.test.stat = -2*(cox.fit.status$loglik[2] - cox.fit$loglik[2]))
1 - pchisq(obs.test.stat, 1)

# f)
cox.fit = coxph(surv.obj ~ Ward, ties = "breslow", data = PH_data)
summary(cox.fit)