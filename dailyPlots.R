##
## Moved to .Rmd
## not updated anymore
##

# Data Source : https://www.data.gouv.fr/fr/datasets/metriques-dutilisation-de-lapplication-tousanticovid/
# URL of the data on data.gouv.fr
URL <- "https://www.data.gouv.fr/fr/datasets/r/0e2168ec-24c7-4b49-a900-c7dd12f8e88c"

# Download and read data
today <- Sys.Date() # today's date
dataFile <- paste0("data/TAC_", today, ".csv") # name file with today's date
download.file(URL, dataFile) # download file from repo
dat <- read.csv(dataFile)

## Description from the website (2020-12-23)
## Le présent jeu de données informe pour chaque jour depuis le lancement de l'application le 2 juin 2020 :
##
## Total cumulé du nombre d'applications enregistrées moins le nombre de désenregistrements.
## Total cumulé d’utilisateurs notifiés par l’application : le nombre d’utilisateurs notifiés par l’application comme contacts à risque suite à une exposition à la COVID-19, depuis le 2 juin 2020.
## Total cumulé d’utilisateurs se déclarant comme des cas de COVID-19 par jour : le nombre d’utilisateurs qui se sont déclarés comme des cas de COVID-19 dans l’application, depuis le 2 juin 2020.

# Check data
# Time differences between dates
unique(diff(as.Date(dat$date))) # -> there are some gaps in the data

# New columns with delta between dates
dat$diffDate <- c(NA, diff(as.Date(dat$date))) # Difference in days between data points
# for the other columns, divide by the time difference to have differences per day
# (otherwise data not comparable when different time differences)
dat$diffRegisterPerDay <- c(NA, diff(dat$register) / dat$diffDate[-1]) 
dat$diffQRPerDay <- c(NA, diff(dat$qrCode) / dat$diffDate[-1])
dat$diffNotifPerDay <- c(NA, diff(dat$notified) / dat$diffDate[-1])

# Create new dataset without gaps in the dates
# New dates, without gaps
rangeDates <- range(as.Date(dat$date))
allDates <- seq(rangeDates[1], rangeDates[2], by="days")
dat.temp <- as.data.frame(as.character(allDates))
names(dat.temp) <- "date"

# Merge the datasets by date 
tmp <- merge(dat, dat.temp, by = "date", all = TRUE)
# note: this adds NA at the dates where there were no data; 
# these dates are put at the end of the table

# Sort the new dataset by date
sortedDateIndex <- sort(as.character(tmp$date), index.return = TRUE)$ix
dat.all <- tmp[sortedDateIndex, ]

# Add day of the week
dat.all$weekDay <- weekdays(as.Date(dat.all$date))

source("commonFuncs.R")

# Compute the average values, 1-week window, focal point in the middle
dat.all$diffRegisterPerDay.WeekAve <- sliding.ave(dat.all$diffRegisterPerDay)
dat.all$diffQRPerDay.WeekAve <- sliding.ave(dat.all$diffQRPerDay)
dat.all$diffNotifPerDay.WeekAve <- sliding.ave(dat.all$diffNotifPerDay)

# PLOTTING
# Colors
source("commonPlot.R")
plot(as.Date(dat.all$date), dat.all$register, 
     xlab = "date", 
     ylab = "Registrations", 
     pch = 16, col = cols[dat.all$weekDay])

plot(as.Date(dat.all$date), dat.all$diffRegisterPerDay, log = "y",      
     xlab = "date", 
     ylab = "delta registrations per day", 
     pch = 16, col = cols[dat.all$weekDay])
legend(as.Date("2020-07-01"), y = 200000, legend = names(cols), col = cols, pch = 16, box.lwd = 0)

abline(v = as.Date("2020-10-30"))
abline(v = as.Date("2020-10-22"))
abline(v = as.Date("2020-11-28"))

plot(as.Date(dat.all$date), dat.all$diffQRPerDay,      
     xlab = "date", 
     ylab = "delta QRcodes per day", 
     pch = 16, col = cols[dat.all$weekDay])
legend(as.Date("2020-07-01"), y = 2000, legend = names(cols), col = cols, pch = 16, box.lwd = 0)

plot(as.Date(dat.all$date), dat.all$diffNotifPerDay,       
     xlab = "date", 
     ylab = "delta notifications per day", 
     pch = 16, col = cols[dat.all$weekDay])
legend(as.Date("2020-07-01"), y = 1500, legend = names(cols), col = cols, pch = 16, box.lwd = 0)

# Weekly Notification minima are on Mondays, while weekly QR code minima are on Sundays
# This was already noticed by https://flesueur.medium.com/réalignement-temporel-des-chiffres-tousanticovid-covid-7bda2f0bf1c1


# Notifs per QR, as a function of time
plot(as.Date(dat.all$date), dat.all$diffNotifPerDay / dat.all$diffQRPerDay)
# With a 1 day difference
points(c(NA, as.Date(dat.all$date)), c(dat.all$diffNotifPerDay, NA) / c(NA, dat.all$diffQRPerDay), col = "red")

# same with sliding window
plot(as.Date(dat.all$date), dat.all$diffNotifPerDay.WeekAve / dat.all$diffQRPerDay.WeekAve)
# Sliding window and 1 day difference
points(c(NA, as.Date(dat.all$date)), c(dat.all$diffNotifPerDay.WeekAve, NA) / c(NA, dat.all$diffQRPerDay.WeekAve), col = "red")

# Notifs per QRcode, As function of number of registrations
plot(c(NA, dat.all$register), c(dat.all$diffNotifPerDay.WeekAve, NA) / c(NA, dat.all$diffQRPerDay.WeekAve), xlim = c(0, max(dat.all$register, na.rm = TRUE)), 
     xlab = "Enregistrements totaux nets", 
     ylab = "Notifications par QR code (moy 7, décalage 1)")

# Approximate size of the French population
popFR <- 67*10^6

plot(as.Date(dat.all$date), ((c(dat.all$diffNotifPerDay.WeekAve, NA) / c(NA, dat.all$diffQRPerDay.WeekAve)) / (c(NA, dat.all$register)/popFR))[-1], 
     xlab = "date", 
     ylab = "Notifications par QR code (moy 7, décalage 1) / Enregistrements totaux nets par hab")



# Key dates
# 22 octobre sortie TAC
# 30 octobre confinement 2
# fin nov campagne pub (Dec 1?)
# 17 oct couvre feu

# Source epidemic data and merge with TAC data
# Using JHU data because they are in terms of reporting date, and not test date
# because we expect the QR code to be scanned when the test result is received
dat.all$date <- as.Date(dat.all$date)
source("epidemicData.R")
dat.withEpi <- merge(dat.all, datJHUFr, by = "date")
names(dat.withEpi)[9] <- "weekDay"


plot(dat.withEpi$date, dat.withEpi$P.WeekAve, log = "y", ylim = c(1, max(dat.withEpi$P.WeekAve, na.rm = TRUE)))
points(dat.withEpi$date, dat.withEpi$diffQRPerDay.WeekAve)

plot(dat.withEpi$date, dat.withEpi$diffQRPerDay.WeekAve / dat.withEpi$P.WeekAve, 
     xlab = "date", ylab = "QRcode / cas du jour (moy 7j)", 
     ylim = c(0, 0.1))

# Estimation de la fraction d'utilisateurs réellement actifs
plot(dat.withEpi$date, dat.withEpi$diffQRPerDay.WeekAve / dat.withEpi$P.WeekAve, 
     xlab = "date", ylab = "QRcode / cas du jour (moy 7j)")

propUtil <- sliding.ave(dat.withEpi$register, winwdt = 7, pos = 4) / popFR

propUtilActif <- dat.withEpi$diffQRPerDay.WeekAve / dat.withEpi$P.WeekAve

plot(dat.withEpi$date, propUtilActif, 
     xlab = "date", ylab = "Proportions d'utilisateurs (moy 7j)", ylim = c(0, 0.2), 
     pch = 16, col = rgb(0.4, 0.7, 0))
points(dat.withEpi$date, propUtil, pch = 16, col = gray(0.4))
legend(as.Date("2020-07-01"), 0.15, col = c(gray(0.4), rgb(0.4, 0.7, 0)), pch = 16, 
       legend = c("Prop. enregistrés", "Prop. utilisateurs actifs"), 
       box.lwd = 0)
axis(4)

notifPerQR <- dat.all$diffNotifPerDay / c(NA, dat.all$diffQRPerDay[ - nrow(dat.all)])
notifPerQR.WeekAve <- dat.all$diffNotifPerDay.WeekAve / c(NA, dat.all$diffQRPerDay.WeekAve[ - nrow(dat.all)])

plot(dat.all$date, notifPerQR)
plot(dat.all$date, notifPerQR.WeekAve)

plot(dat.all$date, notifPerQR / propUtil)
plot(dat.all$date, notifPerQR / propUtilActif)


plot(dat.all$date, notifPerQR.WeekAve / propUtil)
plot(dat.all$date, notifPerQR.WeekAve / propUtilActif)


# Notifs per QRcode, divided by average proportion users the last 7 days
plot(as.Date(dat.all$date), ((c(dat.all$diffNotifPerDay, NA) / c(NA, dat.all$diffQRPerDay)) / (c(NA, sliding.ave(dat.all$register, winwdt = 15, pos = 15))/popFR))[-1], 
     xlab = "date", 
     ylab = "Notifications par QR code (décalage 1) / Enregistrements totaux nets par hab", ylim = c(0, 15), 
     pch = 16, col = rgb(0.7, 0, 0), 
     type = "n"
)
arrows(x0 = as.Date("2020-10-22"), 13, y1 = 11, length = 0.1)
text(x = as.Date("2020-10-22"), y = 13, "StopCovid devient \nTousAntiCovid", adj = c(0.5, -0.5))

arrows(x0 = as.Date("2020-11-28"), 14, y1 = 12, length = 0.1)
text(x = as.Date("2020-11-28"), y = 14, "Changement critères \n pour notification", adj = c(0.5, -0.5))

arrows(x0, y0, x1 = x0, y1 = y0, length = 0.25, angle = 30,
       code = 2, col = par("fg"), lty = par("lty"),
       lwd = par("lwd"), ...)

for(i in 0:15){
  abline(h = i, col = gray(0.8))
}
for(i in c(0, 5, 10, 15)){
  abline(h = i, col = gray(0.8), lwd = 2)
}

points(as.Date(dat.all$date), ((c(dat.all$diffNotifPerDay, NA) / c(NA, dat.all$diffQRPerDay)) / (c(NA, sliding.ave(dat.all$register, winwdt = 7, pos = 7))/popFR))[-1], 
       pch = 16, col = rgb(0.7, 0, 0))


