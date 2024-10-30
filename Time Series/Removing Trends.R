
#Loading the data

testing.stationarity <- read.table("testing.stationarity.txt")
head(testing.stationarity)
testing.stationarity <- ts(testing.stationarity)


# 1.1 Plotting the sample ACF
acf(testing.stationarity, main = "Data: testing.stationarity", ylab = "Sample ACF")


# 1.2 Statistical test for stationarity
PP.test(testing.stationarity)

set.seed(1901)
n = 365
data <- arima.sim(list(ma = c(-1.4, 0.8)), n)
acf(data, main = "Time series: data", ylab = "Sample ACF")
PP.test(data) 


# 2.1 Differencing the data
Xt <- diff(testing.stationarity, lag = 1, differences = 1)

par(mfrow = c(2, 2))

ts.plot(testing.stationarity, main = "Data:testing.stationarity", ylab = "value")

ts.plot(Xt, main = "Differenced data", ylab = "change in value")

#Alternatively
plot(testing.stationarity, main = "Data:testing.stationarity", ylab = "value")

plot(Xt, main = "Differenced data", ylab = "change in value")


acf(testing.stationarity, main = "", ylab = "Sample ACF")
acf(Xt, main = "", ylab = "Sample ACF")
par(mfrow = c(1, 1))

PP.test(Xt)


# 2.2 Choosing d
var(testing.stationarity); var(Xt)

d2t <- diff(testing.stationarity, lag = 1, differences = 2)
d2t <- diff(Xt, lag = 1, differences = 1)
var(d2t)


# 3 Least squares trend removal
set.seed(123)
n = 1000
sim <- arima.sim(list(ar = 0.9), n)
xt <- sim + 2 + 0.05 * time(sim)

ts.plot(xt, col = "blue", main = "Time series with trend", ylab = "Data")

#Alternatively
plot(xt, col = "blue", main = "Time series with trend", ylab = "Data")

time <- seq(1, 1000)
time <- time(xt)

fit <- lm(xt ~ time)

fit$coefficients

yt <- fit$fitted.values 
zt <- fit$residuals

yt <- fit$fit 
zt <- xt - yt

par(mfrow = c(2, 1))
plot(xt, col = "blue", main = "Regression example", ylab = "Data")
abline(fit, col = "red")

plot(zt, type = "l", col = "dark green", xlab = "Time", ylab = "Residuals")
par(mfrow = c(1, 1))


# 4 Identifying seasonality
plot(ldeaths, main = "Monthly deaths from bronchitis, emphysema and asthma in the UK", ylab = "Deaths")
points(ldeaths, pch = 20)
abline(v = 1974:1979, col = "red", lwd = 2)

acf(ldeaths, main = "Sample ACF of the series ldeaths", ylab = "sample ACF", lag.max = 36)


# 5.1 seasonal differencing
sdiff.ldeaths <- diff(ldeaths, lag = 12, differences = 1)

par(mfrow = c(2, 2))
plot(ldeaths, main = "Data: ldeaths", ylab = "number of deaths")
acf(ldeaths, main = "Sample ACF of ldeaths", ylab = "")

plot(sdiff.ldeaths, main = " Data: sdiff.ldeaths", ylab = "increase in number of deaths")
acf(sdiff.ldeaths, main = "Sample ACF of sdiff.ldeaths", ylab = "")
par(mfrow = c(1, 1))

m <- matrix(2, 2, data = c(1, 2, 3, 4), byrow = TRUE)
layout(m)

plot(ldeaths, main = "Data: ldeaths", ylab = "number of deaths")

acf(ldeaths, main = "Sample ACF of ldeaths", ylab = "")

plot(sdiff.ldeaths, main = " Data: sdiff.ldeaths", ylab = "increase in number of deaths")

acf(sdiff.ldeaths, main = "Sample ACF of sdiff.ldeaths", ylab = "")

par(mfrow = c(1, 1))


# 5.2 seasonal means

ldeaths

ldeaths.df = data.frame(year = rep(1974:1979, each = 12), month = rep(1:12, 6), value = ldeaths)

# year = c(rep(1974:1979, each = 12), 1980)
# month = c(rep(1:12, 6), 1)

head(ldeaths.df, 15)

(xbars = aggregate(value ~ month, data = ldeaths.df, FUN = mean))

yt = ldeaths - xbars$value

plot(yt, main = "Monthly deaths from bronchitis, emphysema and asthma in the UK less seasonal means", ylab = "Deaths")
points(yt, pch = 20)

acf(yt, main = "Sample ACF of the series ldeaths less seasonal means", ylab = "sample ACF", lag.max = 36)


# 5.3 Using in-built functions to separate out trend, seasonality and white noise


# The decompose function
plot(decompose(ldeaths, type = "additive"))

decomp <- decompose(ldeaths, type = "additive")
trend <- decomp$trend

head(trend, 7)
tail(trend, 7)

seasonal <- decomp$seasonal
random <- decomp$random

plot(ldeaths, ylab = "", main = "Components of time series: ldeaths", col = "dark grey")
points(ldeaths, cex = 0.5, col = "dark grey")
lines(trend, col = "red")
lines(seasonal + trend, col = "blue")

colours()

# The stl function
plot(stl(ldeaths, s.window = "periodic"), main = "Components of time series: ldeaths")

stl <- stl(ldeaths, s.window = "periodic")
trend <- stl$time.series[, "trend"]

stl
trend

seasonal <- stl$time.series[, "seasonal"]
remainder <- stl$time.series[, "remainder"]

plot(ldeaths, ylab = "", main = "Components of time series: ldeaths", col = "dark grey")
points(ldeaths, cex = 0.5, col = "dark grey")
lines(trend, col = "red")
lines(seasonal + trend, col = "blue")


## QUESTION 13-14.5


# Running the code provided
xx <- numeric(1003)
set.seed(4567)
ww <- rnorm(1003)
xx[1:3] <- ww[1:3]

for (t in 4:1003) { 
  xx[t] <- 0.8*xx[t-1] +0.2* xx[t-3] + ww[t]+ 2*ww[t-1]
}

s <- ts(xx[4:1003])

# Differencing and analysing
ds <- diff(s)
dds <- diff(ds)
par(mfrow = c(1, 3))
acf(s); acf(ds); acf(dds)
par(mfrow = c(1, 1))
var(s); var(ds); var(dds)
PP.test(s)$p.value; PP.test(ds)$p.value; PP.test(dds)$p.value


## QUESTION 13-14.6
# i)
sales <- read.table("sales.txt")
head(sales)
sales <- ts(sales, frequency = 12, start = c(2005, 1))
sales


# ii)
decomposed <- (decompose(sales, type = "additive"))
untrended <- decomposed$random


# iii)
acf(untrended)
r <- untrended[!is.na(untrended)]
par(mfrow = c(1,2))
acf(r, main = "Untrended data"); pacf(r, main = "Untrended data")
par(mfrow = c(1, 1))


## QUESTION 13-14.7
# i)
seas <- read.table("seasonality.txt")
head(seas)
seas <- ts(seas, frequency = 12, start = c(2010, 1))


# ii)
m <- matrix(2, 2, data = c(1, 2, 1, 3))
layout(m)
ts.plot(seas, main = "Data: Reservoir level", ylab = "gallons (millions)")
acf(seas, lag.max = 36, main = "")
pacf(seas, lag.max = 36, main = "")

#Alternatively
plot(seas, main = "Data: Reservoir level", ylab = "gallons (millions)")
acf(seas, lag.max = 36, main = "")
pacf(seas, lag.max = 36, main = "")

par(mfrow = c(1, 1))


# iii)
d <- diff(seas, lag = 12, differences = 1)
head(d)


# iv)
m <- matrix(2, 2, data = c(1, 2, 1, 3))
layout(m)
ts.plot(d, main = "Change since same month last year", ylab = "gallons (millions)")
abline(h = 0, lty = "dashed",col="red")
acf(d, lag.max = 36, main = "")
pacf(d, lag.max = 36, main = "")

#Alternatively
plot(d, main = "Change since same month last year", ylab = "gallons (millions)")
abline(h = 0, lty = "dashed",col="red")
acf(d, lag.max = 36, main = "")
pacf(d, lag.max = 36, main = "")

par(mfrow = c(1, 1))
