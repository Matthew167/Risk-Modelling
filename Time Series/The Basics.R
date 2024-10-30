

# 1.1 Simulating time series data


# Simulating stationary data - example 1
Yt <- arima.sim(n = 100, list(ar = c(0.5, -0.1), ma = 0.2), sd = 3)
Wt <- 10 + arima.sim(n = 50, list(ar = 0.8, ma = c(0.4, 0.1)), sd = 5^0.5)

# Simulating stationary data - example 2
c <- 10
n <- 200
xt <- numeric(n)

set.seed(1789)
et <- rnorm(n)

for (i in 2:n) {
  xt[i] = c + 0.9*xt[i-1] + et[i]
}

head(xt)
xt <- ts(xt)
plot(xt, main = "Simulated data")

#Alternatively
ts.plot(xt,main = "Simulated data")

# Question
set.seed(1215)
n <- 50
xt <- arima.sim(list(ar = 0.9, ma = -0.2), n)
Yt <- xt + 1805 + 0.5*seq(1, n)

# Simulating data with a stochastic trend
set.seed(8143)
n = 100
xt <- arima.sim(list(ar = c(0.5,0.2), ma = -0.2), n)

Yt <- 80 + cumsum(xt)
Yt <- ts(Yt)

par(mfrow = c(2, 1))
plot(xt, main = "Differenced time series", col = "red")
plot(Yt, main = "ARIMA(2,1,1)", col = "blue")
par(mfrow = c(1, 1))

# intercept vs. mean
mean(Yt)


# 1.2 Importing time series data


# first attempt
Xt <- read.table("Xt.csv")
head(Xt)

# second attempt
Xt <- read.table("Xt.csv", sep = ",")
head(Xt)

# creating a time series object
str(Xt)
ts.Xt <- ts(Xt, start = c(2000, 1), frequency = 12)

ts.quarterlyXt <- ts(Xt, start = c(1912, 2), frequency = 4)

window(ts.Xt, start = c(2004, 2), end = c(2007, 5))


# 2.1 Plotting sample time series data


ts.plot(ts.Xt, main = "Time series Xt", ylab = "Value", col = "blue")

#Alternatively
plot(ts.Xt, main = "Time series Xt", ylab = "Value", col = "blue")

points(ts.Xt, col = "red", cex = 0.7)


# 2.1 Plotting the sample ACF and sample PACF


par(mfrow = c(2, 1))

acf(ts.Xt, lag.max = 60, main = "Time series Xt", ylab = "Sample ACF")

pacf(ts.Xt, lag.max = 60, main = "Time series Xt", ylab = "Sample PACF")


# 2.3 Plotting the theoretical ACF and PACF


modelacf <- ARMAacf(ar = c(0.5, -0.1), ma = 0.2, lag.max = 12)
modelpacf <- ARMAacf(ar = c(0.5, -0.1), ma = 0.2, lag.max = 12, pacf = TRUE)

modelacf
modelpacf

barplot(modelacf, main = "ACF of ARMA(2,1)", col = "red", xlab = "Lag")
barplot(modelpacf, main = "PACF of ARMA(2,1)", col = "blue", xlab = "Lag")

barplot(modelacf, main = "ACF of ARMA(2,1)", col = "red", xlab = "Lag")
barplot(modelpacf, main = "PACF of ARMA(2,1)", col = "blue", xlab = "Lag", names.arg = seq(1,12))

barplot(modelacf[-1], main = "ACF of ARMA(2,1)", col = "red",xlab = "Lag")
barplot(modelpacf, main = "PACF of ARMA(2,1)", col = "blue",xlab = "Lag", names.arg = seq(1,12))

par(mfrow = c(1, 1))


# 3 Extracting key numbers from time series data


frequency(ts.Xt)
start(ts.Xt)
end(ts.Xt)

# ACF and PACF calculations
a <- acf(ts.Xt, plot = FALSE, lag.max = 60)
a
head(a)
a$acf[13]

p <- pacf(ts.Xt, plot = FALSE, lag.max = 125)
p
p$acf[120]


## Solution 13-14.1


AR2acf <- ARMAacf(ar = c(0.8, -0.6), lag.max = 20)
AR2pacf <- ARMAacf(ar = c(0.8, -0.6), lag.max = 20, pacf = TRUE)

par(mfrow = c(2, 1))

barplot(AR2acf, main = "ACF of AR(2)", col = "blue", xlab = "Lag")
barplot(AR2pacf, main = "PACF of AR(2)", col = "red", 
        xlab = "Lag", names.arg = seq(1,20))

par(mfrow = c(1, 1))


## Solution 13-14.2


# part (a)
frequency(ldeaths)

# part (b)
start(ldeaths)

# part (c)
end(ldeaths)

# part (d)
a <- acf(ldeaths, plot = FALSE, lag.max = 72)
length(ldeaths)
a$acf[25]

# part (e)
ldeaths
ldeaths[50]

window(ldeaths, c(1978, 2), c(1978, 2))

# part (f)

sum(ldeaths[25:36])
sum(window(ldeaths, c(1976, 1), c(1976, 12)))


## Solution 13-14.3


par(mfrow = c(1, 2))

acf(ldeaths, lag.max = 60, main = "Number of deaths from lung disease", 
    ylab = "Sample ACF", col = "red")

pacf(ldeaths, lag.max = 60, main = "Number of deaths from lung disease", 
     ylab = "Sample PACF", col = "blue")

par(mfrow = c(1, 1))


## Solution 13-14.4


# Running the code provided
set.seed(2224)
sim <- arima.sim(n = 480, 
                 model = list(ar = c(0.8,-0.4), ma = 0.6),
                 sd = 100) + 2000
sim <- ts(sim, start = c(2020,1), end = c(2059, 12),frequency = 12)

# Creating a subset of the data
sim.subset = window(sim, c(2024, 1), c(2025, 12))

# Constructing the plot
plot(sim.subset, main = "Values of the process from Jan 2024 to Dec 2025",
     ylab = "Values")

points(sim.subset, col = "red")