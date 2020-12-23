# Settings for the plots

# Days of the week
library(RColorBrewer)
cols <- brewer.pal(n = 7, name = "Set2")

plot(1:7, pch = 16, col = cols)
names(cols) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

par(las = 1)
