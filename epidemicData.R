# LOAD DATA
# We will load two datasets, compare them, and choose one

# Data from data.gouv.fr
# Source: https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/#_
# The date in these data is the date of the test
URL <- "https://www.data.gouv.fr/fr/datasets/r/dd0de5d9-b5a5-4503-930a-7b08dc0adc7c"
# Download and read data
today <- Sys.Date() # today's date
dataFile <- paste0("data/SIDEP_Fra_", today, ".csv") # name file with today's date
download.file(URL, dataFile) # download file from repo
dat <- read.csv(dataFile, sep = ";")

# The data are by age class, but there is a line for all age classes for each day!
# it is the lines for which cl_age90 is 0
dat.agg <- dat[dat$cl_age90 < 1, ]
names(dat.agg)[2] <- c("date")

# Add day of week
dat.agg$weekDay <- weekdays(as.Date(dat.agg$date))

# Data from JHU
# The date in these data is the date of the result's annoucement
URL <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
# Download and read data
dataFile <- paste0("data/JHU_cases_all_", today, ".csv") # name file with today's date
download.file(URL, dataFile) # download file from repo
datJHU <- read.csv(dataFile, header = TRUE, check.names = FALSE)
# Extract data about France
datJHU.France <- datJHU[datJHU$`Country/Region` == "France", ]

# Collapse data for all France (including DOM TOM)
datJHU.France.agg <- apply(datJHU.France[, -(1:4)], 2, sum)
# Need to reformat the data: have one column for dates and one column for values
datJHUFr <- data.frame(matrix(0, ncol = 2, nrow = length(datJHU.France.agg)))
datJHUFr$date <- as.Date(names(datJHU.France.agg), format = "%m/%d/%y")
datJHUFr$cumP <- unname(datJHU.France.agg)
datJHUFr <- datJHUFr[, -(1:2)]
# Get week day
datJHUFr$weekDay <- weekdays(as.Date(datJHUFr$date))
# Extract data per day (JHU provides cumulative values)
datJHUFr$P <- c(datJHUFr[1, "cumP"], diff(datJHUFr$cumP))

#-------------------------------------------------------------

# Compare the two sources
# Raw data
source("commonPlot.R")
plot(as.Date(dat.agg$date), dat.agg$P, pch = 16, col = cols[dat.agg$weekDay], 
     xlab = "date", ylab = "nb pos tests", main = "Nb positive tests, raw data")
points(as.Date(datJHUFr$date), datJHUFr$P, pch = 1, col = cols[datJHUFr$weekDay], cex = 1, lwd = 1)
legend(as.Date("2020-07-01"), y = 50000, legend = c(names(cols), "data.gouv.fr", "JHU"), col = c(cols, 1, 1), pch = c(rep(17, 7), 16, 1), box.lwd = 0)


source("commonFuncs.R")
dat.agg$P.WeekAve <-  sliding.ave(dat.agg$P, winwdt = 7, pos = 4, na.rm = TRUE)
plot(as.Date(dat.agg$date), dat.agg$P.WeekAve, pch = 16, col = cols[dat.agg$weekDay], 
     xlab = "date", ylab = "nb pos tests (7-day ave)", main = "Nb positive tests, 7-day sliding window", type = "o", cex = 0.75)
points(as.Date(datJHUFr$date), sliding.ave(datJHUFr$P, winwdt = 7, pos = 4, na.rm = TRUE), pch = 1, col = cols[datJHUFr$weekDay], cex = 1, lwd = 1, type = "o")
legend(as.Date("2020-07-01"), y = 40000, legend = c(names(cols), "data.gouv.fr", "JHU"), col = c(cols, 1, 1), pch = c(rep(15, 7), 16, 1), box.lwd = 0)

# There is a delay between the two, because data.gouv.fr are in terms of sampling date, and JHU are in terms of announcement date. 

# Add the sliding window value to the JHU data
datJHUFr$P.WeekAve <- sliding.ave(datJHUFr$P, winwdt = 7, pos = 4, na.rm = TRUE)

