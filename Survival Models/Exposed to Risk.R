
# Exercise 1 i)
FuneralData = read.csv("FuneralData.csv")
head(FuneralData)

# b)
str(FuneralData)

# c)
nrow(FuneralData)

# d)
FuneralData$BIRTH = as.Date(FuneralData$BIRTH, format = "%d/%m/%Y")
FuneralData$DEATH = as.Date(FuneralData$DEATH, format = "%d/%m/%Y")
FuneralData$ENTRY = as.Date(FuneralData$ENTRY, format = "%d/%m/%Y")

# e)
str(FuneralData)

# f)
inv_start = as.Date("01/01/2013", format = "%d/%m/%Y")
inv_end = as.Date("31/12/2017", format = "%d/%m/%Y")
nrow(FuneralData[FuneralData$DEATH >= inv_start & FuneralData$DEATH <= inv_end,])


# Exercise 1 ii
# a)
Alive = FuneralData[FuneralData$ENTRY <= inv_end & FuneralData$DEATH >= inv_start, ]

# b)
nrow(Alive)


# Exercise 1 iii)
# a)
interim = as.POSIXlt(Alive$BIRTH)
interim$year = interim$year + 70
Alive$B70 = as.Date(interim)

# b)
interim = as.POSIXlt(Alive$BIRTH)
interim$year = interim$year + 71
Alive$B71 = as.Date(interim)

# c)
head(Alive, 10)


# Exercise 1 iv)
Alive70 = Alive[Alive$ENTRY < Alive$B71 &
                  Alive$DEATH > Alive$B70 &
                  Alive$B70 <= inv_end &
                  Alive$B71 > inv_start, ]


# Exercise 1 vi)
# a)
Alive70$ETR_START = pmax(Alive70$ENTRY, Alive70$B70, inv_start)

# b)
Alive70$ETR_END = pmin(Alive70$DEATH, Alive70$B71, inv_end)

# c)
Alive70$ETR = Alive70$ETR_END - Alive70$ETR_START
head(Alive70[, 3:9])

# d)
(ETR70 = sum(Alive70$ETR) / 365.25)
str(Alive70$ETR)
(ETR70 = as.numeric(sum(Alive70$ETR) / 365.25))


# Exercise 1 vii)
(deaths70 = nrow(Alive70[Alive70$DEATH <= inv_end & Alive70$B71 > Alive70$DEATH, ]))

deaths70 / ETR70


# Exercise 1 viii)


# a)

census = function(date, entry, death, b70, b71){
  ifelse(death > date & entry < date & b70 <= date & b71 > date, 1, 0)
}

# b)

census(inv_start,
       Alive70$ENTRY,
       Alive70$DEATH,
       Alive70$B70,
       Alive70$B71)

sum(census(inv_start,
           Alive70$ENTRY,
           Alive70$DEATH,
           Alive70$B70,
           Alive70$B71))

# sum(census(inv_start,
#            Alive$ENTRY,
#            Alive$DEATH,
#            Alive$B70,
#            Alive$B71))

# c)

census1 = as.Date("01/01/2014", format = "%d/%m/%Y")
census2 = as.Date("01/01/2015", format = "%d/%m/%Y")
census3 = as.Date("01/01/2016", format = "%d/%m/%Y")
census4 = as.Date("01/01/2017", format = "%d/%m/%Y")
census5 = as.Date("01/01/2018", format = "%d/%m/%Y")

census_dates = c(census1, census2, census3, census4, census5)

inforce = rep(NA, 5)

for (i in 1:5){
  inforce[i] = sum(census(census_dates[i],
                          Alive70$ENTRY,
                          Alive70$DEATH,
                          Alive70$B70,
                          Alive70$B71))
}

inforce

inforce = c(12, inforce)

# d)

(ETR70_CENSUS = sum(inforce) - 0.5 * (inforce[1] + inforce[6]))

deaths70 / ETR70_CENSUS


# Additional code


date.seq = seq(inv_start, census5, "days")
head(date.seq)

inforce.daily = rep(NA, length(date.seq))

for(i in 1:length(date.seq)){
  inforce.daily[i] = sum(census(date.seq[i],
                                Alive70$ENTRY,
                                Alive70$DEATH,
                                Alive70$B70,
                                Alive70$B71))
}

plot(date.seq, inforce.daily, type = "l",
     main = "Number of lives aged 70 last birthday",
     xlab = "Date",
     ylab = "Number of people")

lines(c(inv_start, census_dates), inforce, col = "red", lty = 3)

legend("topleft", legend = c("daily count", "census counts on 1st Jan"), 
       col = c("black", "red"), lty = c(1,3))
