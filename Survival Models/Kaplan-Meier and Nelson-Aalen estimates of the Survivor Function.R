
# (i)
surv.data = read.csv("surv_table_7.1.csv")
surv.data


# (ii)
library(survival)

surv.data$event.time = surv.data$observation_end - surv.data$exposure_time
surv.data$event.code = ifelse(surv.data$reason == "D", 1, 0)

surv.obj = Surv(surv.data$event.time, surv.data$event.code)
surv.obj


# (iii)a)
fitkm = survfit(surv.obj ~ 1, conf.type = "plain")
summary(fitkm)

# b)
plot(fitkm,
     main = "Kaplan-Meier estimate of survival function",
     xlab = "weeks",
     ylab = "SKM(t)")


# (iv)
summary(fitkm)
summary(fitkm)$lower[3]
summary(fitkm)$upper[3]


# (v)a)

fitna = survfit(surv.obj ~ 1,  conf.type = "plain", stype = 2)
summary(fitna)

# b)
plot(fitna,
     main = "Nelson-Aalen estimate of survival function",
     xlab = "weeks",
     ylab = "SNA(t)")