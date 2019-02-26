library(data.table)
library(forecast)
library(lubridate)
library(xts)
library(ggplot2)

daily <-fread("NOAAdaily.csv")
monthly <- fread("NOAAmonthly.csv")
#
# Create YYYY-MM-DD Field
daily$Ymd <- as.Date(paste(daily$Year, daily$Month, daily$Day, sep = "-"))
summary(daily)
sd(daily$Ra)
# cor(daily$Ra)
var(daily$Ra)
summary(monthly)
sd(monthly$Ra)
var(monthly$Ra)
# cor(monthly$Ra)
hist(daily$Ra)
boxplot(daily$Ra)
hist(monthly$Ra)
boxplot(monthly$Ra)
ggplot(data=monthly,aes(Year,Ra)) + geom_smooth() +geom_point()
ggplot(data=daily,aes(Ymd,Ra)) + geom_point() +geom_smooth()