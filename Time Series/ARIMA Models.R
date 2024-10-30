#----------------------------------------------------------------------------------------------
# 1.2 Choosing a model - Example 1
#----------------------------------------------------------------------------------------------

data <- ts(read.csv("fittingmodelEg1.csv", header = FALSE))
head(data)

m <- matrix(2, 2, data = c(1, 2, 1, 3))
m
layout(m)

ts.plot(data, main = "What model to fit?", ylab = "Data: Example 1")
acf(data, main = "")
pacf(data, main = "")

#Alternatively:
plot(data, main = "What model to fit?", ylab = "Data: Example 1")
acf(data, main = "")
pacf(data, main = "")


par(mfrow = c(1, 1))

#----------------------------------------------------------------------------------------------
# 1.3 Choosing a model - Example 2
#----------------------------------------------------------------------------------------------

data2 <- ts(read.csv("fittingmodelEg2.csv", header = FALSE))
head(data2)

m <- matrix(2, 2, data = c(1, 2, 1, 3))
layout(m)

ts.plot(data2, main = "What model to fit?", ylab = "Data: Example 2")
acf(data2, main = "")
pacf(data2, main = "")

#Alternatively
plot(data2, main = "What model to fit?", ylab = "Data: Example 2")
acf(data2, main = "")
pacf(data2, main = "")


par(mfrow = c(1, 1))

#----------------------------------------------------------------------------------------------
# 1.4 Choosing a model - Example 3
#----------------------------------------------------------------------------------------------

data3 <- ts(read.csv("fittingmodelEg3.csv", header = FALSE))
head(data3)

m <- matrix(2, 2, data = c(1, 2, 1, 3))
layout(m)

ts.plot(data3, main = "What model to fit?", ylab = "Data: Example 3")
acf(data3, main = "", ylab = "Sample ACF")
pacf(data3, main = "", ylab = "Sample PACF")

#Alternatively
plot(data3, main = "What model to fit?", ylab = "Data: Example 3")
acf(data3, main = "", ylab = "Sample ACF")
pacf(data3, main = "", ylab = "Sample PACF")


par(mfrow = c(1, 1))

#----------------------------------------------------------------------------------------------
# 1.5 Choosing a model - Example 4
#----------------------------------------------------------------------------------------------

data4 <- ts(read.csv("fittingmodelEg4.csv", header = FALSE))
head(data4)

d <- diff(data4)
m <- matrix(2, 4, data = c(1, 2, 1, 3, 4, 5, 4, 6)); m

layout(m)

ts.plot(data4, main = "Data without differencing", ylab = "Data: Example 4")
acf(data4, main = "", ylab = "Sample ACF")
pacf(data4, main = "", ylab = "Sample PACF")

ts.plot(d, main = "Differenced data", ylab = "increase in data4")
acf(d, main = "", ylab = "Sample ACF")
pacf(d, main = "", ylab = "Sample PACF")

#Alternatively
plot(data4, main = "Data without differencing", ylab = "Data: Example 4")
acf(data4, main = "", ylab = "Sample ACF")
pacf(data4, main = "", ylab = "Sample PACF")

plot(d, main = "Differenced data", ylab = "increase in data4")
acf(d, main = "", ylab = "Sample ACF")
pacf(d, main = "", ylab = "Sample PACF")


par(mfrow = c(1, 1))

#----------------------------------------------------------------------------------------------
# 2.1 Fitting a model - Example 2
#----------------------------------------------------------------------------------------------

arima(data2, order = c(0, 0, 3))

#----------------------------------------------------------------------------------------------
# 2.2 Fitting a model - Example 3
#----------------------------------------------------------------------------------------------

arima(data3, order = c(2, 0, 0))

#----------------------------------------------------------------------------------------------
# 2.3 Fitting a model - Example 3
#----------------------------------------------------------------------------------------------

ar(data3)
ar(data3)$x.mean

#----------------------------------------------------------------------------------------------
# 2.4 Fitting a model - Example 4
#----------------------------------------------------------------------------------------------

arima(data4, order = c(1, 1, 1))
arima(d, order = c(1, 0, 1))

#----------------------------------------------------------------------------------------------
# 3.2 Calculating residuals
#----------------------------------------------------------------------------------------------

ma3 <- arima(data2, order = c(0, 0, 3))

e <- ma3$residuals
e <- residuals(ma3)

par(mfrow = c(2, 1))

ts.plot(e, main = "MA(3): analysis of residuals", ylab = "Residuals", col = "blue")
acf(e, main = "", ylab = "ACF of residuals")

#Alternatively
plot(e, main = "MA(3): analysis of residuals", ylab = "Residuals", col = "blue")
acf(e, main = "", ylab = "ACF of residuals")


par(mfrow = c(1, 1))

tsdiag(ma3)

#----------------------------------------------------------------------------------------------
# 3.3 Ljung-Box test
#----------------------------------------------------------------------------------------------

Box.test(e, lag = 5, type = "Ljung", fitdf = 3)

ar2 <- arima(data3, order = c(2, 0, 0))
e2 <- ar2$residuals
Box.test(e2, lag = 10, type = "Ljung", fitdf = 2)

#----------------------------------------------------------------------------------------------
# 3.4 AIC
#----------------------------------------------------------------------------------------------

arima(d, order = c(2, 0, 1))
arima(d, order = c(2, 0, 1))$aic
arima(d, order = c(1, 0, 2))$aic


## QUESTION 13-14.8


#------------------------------------------------------------------------------------
# i)
#------------------------------------------------------------------------------------

TS <- read.table("TS.txt", sep = "\t")
TS <- ts(TS[, 1], start = 1, end = length(TS[, 1]), frequency = 1)
fit <- arima(TS, order = c(2, 0, 0))
fit

#------------------------------------------------------------------------------------
# ii)
#------------------------------------------------------------------------------------

Box.test(fit$residuals, lag = 5, type = "Ljung", fitdf = 2)


## QUESTION 13-14.9


Wt <- read.table("Wt.csv", sep = "", header = TRUE)
Wt <- ts(Wt$value, start = min(Wt$day), end = max(Wt$day))

par(mfrow = c(2, 1))

acf(Wt, main = "Data: Wt", ylab = "sample ACF")
pacf(Wt, main = "", ylab = "sample PACF")

par(mfrow = c(1, 1))


## QUESTION 13-14.10


#------------------------------------------------------------------------------------
# i)
#------------------------------------------------------------------------------------

Xt <- read.table("Xt.csv", sep = ",", header = TRUE)
Xt <- ts(Xt, 1990, frequency = 12)

answer <- numeric(4)
model <- arima(Xt, order = c(0, 1, 2))

aic <- model$aic
aic <- arima(Xt, order = c(0, 1, 2))$aic

row <- c(0, 1, 2, aic)
row

answer <- rbind(answer, row)
answer

answer <- numeric(4)

for (p in 0:2) for (d in 0:2) for (q in 0:2) {
  aic <- arima(Xt, order = c(p, d, q))$aic
  row <- c(p, d, q, aic)
  answer <- rbind(answer, row)
}

answer = answer[-1, ]
answer

#------------------------------------------------------------------------------------
# ii)
#------------------------------------------------------------------------------------

min(answer[, 4])

which(answer[, 4] == min(answer[, 4]))

answer[15, ]


## QUESTION 13-14.11


#------------------------------------------------------------------------------------
# i)
#------------------------------------------------------------------------------------

Yt <- read.table("Yt.csv", sep = ",", header = TRUE)
Yt <- ts(Yt[, 2], 2005, frequency = 12)

fit <- arima(Yt, order = c(1, 0, 1))
fit

#------------------------------------------------------------------------------------
# ii)
#------------------------------------------------------------------------------------

et <- fit$residuals
acf(et, lag.max = 24)
tsdiag(fit) 

#------------------------------------------------------------------------------------
# iii)
#------------------------------------------------------------------------------------

et <- fit$residuals
n <- length(et)
turns = 0

for (i in 2:(n - 1)) {
  if (et[i] > et[i + 1] & et[i] > et[i - 1]) {
    turns <- turns + 1
  }
  else if (et[i] < et[i + 1] & et[i] < et[i - 1]) {
    turns <- turns + 1
  }
}

turns

E <- 2/3 * (n - 2)
V <- (16*n - 29)/90
E; turns
ts <- (turns - E + 0.5)/V^0.5
ts 