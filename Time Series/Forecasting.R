

# 1.2 Example 1 - stationary data


set.seed(476)
abc <- sort(rbinom(3, 1, 0.6), decreasing = TRUE)*c(0.86, -0.4, 0.15)
data <- 50 + arima.sim(list(ar = abc), n = 1000)

fit <- ar(data)
fit
fit$x.mean

p <- predict(fit, n.ahead = 20)
p

a = fit$ar[1]
b = fit$ar[2]
sig = fit$var.pred
(g1.coeff = a / (1 - b))
(g2.coeff = a * g1.coeff + b)
(g0 = sig / (1 - a * g1.coeff - b * g2.coeff))
sqrt(g0)


p$pred[10]

ts.plot(data, col = "blue", xlim = c(900, 1020), main = "Forecasting example", ylab = "Data")
lines(p$pred, col = "red")

#Alternatively
plot(data, col = "blue", xlim = c(900, 1020), main = "Forecasting example", ylab = "Data")
lines(p$pred, col = "red")


# 1.3 Example 2 - non-stationary data


set.seed(476)
a <- runif(1,0.6,0.8); b <- runif(1, -0.3, 1)

data <- 50 + cumsum(arima.sim(list(ma = c(a, b)), n = 1460))
data <- ts(data, start = c(2015, 1),frequency = 365)

d <- diff(data)
fit <- arima(d, order = c(0, 0, 2))
fit

p <- predict(fit, n.ahead = 180)
p <- p$pred
p.with.trend <- tail(data, 1) + cumsum(p)

p.with.trend <- tail(data, 1)[1] + cumsum(p)
p.with.trend <- data[length(data)] + cumsum(p)
p.with.trend <- utils:::tail.default(data, 1) + cumsum(p)

p.with.trend <- ts(p.with.trend, start = c(2019, 1), frequency = 365)

ts.plot(data,col = "blue", xlim = c(2015, 2019 + 179/365),
        main = "Forecasting example 2", ylab = "Data", 
        ylim = c(-42, 153))

lines(p.with.trend, col = "red")

#Alternatively
plot(data,col = "blue", xlim = c(2015, 2019 + 179/365),
     main = "Forecasting example 2", ylab = "Data", 
     ylim = c(-42, 153))

lines(p.with.trend, col = "red")


# 2.2 Example


series <- read.csv("forecasting.csv",header = FALSE)
head(series)
series <- ts(series, start = c(2017, 20), frequency = 365)
end(series)

HW <- HoltWinters(series, alpha = 0.7, beta = FALSE, gamma = FALSE)

predict(HW, n.ahead = 1) 
predict(HW, level = 0.95, prediction.interval = TRUE)
predict(HW, n.ahead = 5)


# 2.3 Example 2


HW2 <- HoltWinters(series, beta = FALSE, gamma = FALSE)
predict(HW2, level = 0.95, prediction.interval = TRUE)

HW2$alpha


##  13-14.12



# i)


set.seed(1558)

data <- 100 + arima.sim(list(ar = runif(1, -0.3, 0.6), 
                             ma = runif(1, 12, 15)), 
                        n = 40)

data <- ts(data, start = c(2008, 1), frequency = 4)

fit <- arima(data, order = c(1, 0, 1))
start(data); end(data); frequency(data)

p <- predict(fit, n.ahead = 8)
p <- p$pred; p


# ii)


HW <- HoltWinters(data, beta = FALSE, gamma = FALSE)
p2 <- predict(HW, n.ahead = 8); p2


# iii)


ts.plot(data, main = "Data and forecasts", ylab = "Revenue (�000s)", xlim = c(2008, 2020), col = "blue")

#Alternatively
plot(data, main = "Data and forecasts", ylab = "Revenue (�000s)", xlim = c(2008, 2020), col = "blue")

lines(p, col = "red")
lines(p2, col = "dark green")

legend("bottomright", legend = c("data", "step-ahead forecast", "exp smoothing forecast"), 
       col = c("blue", "red", "dark green"), lty = 1)


##  13-14.12



# i)


set.seed(1558)

data <- 100 + arima.sim(list(ar = runif(1, -0.3, 0.6), 
                             ma = runif(1, 12, 15)), 
                        n = 40)

data <- ts(data, start = c(2008, 1), frequency = 4)

fit <- arima(data, order = c(1, 0, 1))
start(data); end(data); frequency(data)

p <- predict(fit, n.ahead = 8)
p <- p$pred; p


# ii)


HW <- HoltWinters(data, beta = FALSE, gamma = FALSE)
p2 <- predict(HW, n.ahead = 8); p2


# iii)


ts.plot(data, main = "Data and forecasts", ylab = "Revenue (�000s)", xlim = c(2008, 2020), col = "blue")

#Alternatively
plot(data, main = "Data and forecasts", ylab = "Revenue (�000s)", xlim = c(2008, 2020), col = "blue")

lines(p, col = "red")
lines(p2, col = "dark green")

legend("bottomright", legend = c("data", "step-ahead forecast", "exp smoothing forecast"), 
       col = c("blue", "red", "dark green"), lty = 1)


##  13-14.13



# i)


set.seed(1952)
x <- arima.sim(list(ar = 0.7), n = 240)
x <- 1800 + cumsum(x)
x <- ts(x, start = c(1990, 1), frequency = 12)
dx <- diff(x)
fit <- ar(dx)
pd <- predict(fit, n.ahead = 60)$pred
px <- cumsum(pd) + tail(x, 1)
px <- ts(px, start = c(2010, 1), frequency = 12) 
px
window(px, c(2014, 12), c(2014, 12))


# ii)


min(px, x); max(px, x)
ts.plot(x, main = "Past data and forecast", ylab = "Number of diagnoses", 
        xlim = c(1990, 2015), ylim = c(1796, 1833), col = "blue")

#Alternatively
plot(x, main = "Past data and forecast", ylab = "Number of diagnoses", 
     xlim = c(1990, 2015), ylim = c(1796, 1833), col = "blue")

lines(px, col = "red")
legend("bottomright", legend = c("data", "forecast"),
       col = c("blue", "red"), lty = 1)


##  13-14.14


set.seed(1952)

y <- 90 + round(arima.sim(list(ma = c(15, 7)), n = 40))
y <- ts(y, start = c(2007, 3), frequency = 4)

fit <- arima(y, order = c(0, 0, 2))
p <- predict(fit, n.ahead = 100)$pred

window(p, c(2038, 4), c(2038, 4))