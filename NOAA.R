library(data.table)
library(forecast)
library(lubridate)
library(xts)
library(ggplot2)
library(sqldf)
library(RSQLite)

daily <-fread("https://www.aavso.org/sites/default/files/solar/NOAAfiles/NOAAdaily.csv")
monthly <- fread("https://www.aavso.org/sites/default/files/solar/NOAAfiles/NOAAmonthly.csv")
#
# Create YYYY-MM-DD Field
daily$Ymd <- as.Date(paste(daily$Year, daily$Month, daily$Day, sep = "-"))
daily$Vote <- ifelse(daily$Ra == 0, 0, 1 )
monthly$Vote <- ifelse(monthly$Ra == 0, 0, 1 )
summary(daily)
sd(daily$Ra)
# cor(daily$Ra)var(daily$Ra)
summary(monthly)
sd(monthly$Ra)
var(monthly$Ra)
# cor(monthly$Ra)
hist(daily$Ra)
boxplot(daily$Ra)
hist(monthly$Ra)
boxplot(monthly$Ra)
daily$ma <-ma(daily$Ra,order=365)
plot(daily$Ymd,daily$Ra)
lines(daily$Ymd,daily$Ra,col="red")
ggplot(data=monthly,aes(Year,Ra)) + geom_smooth() +geom_point()
ggplot(data=daily,aes(Ymd,Ra)) + geom_point() +geom_smooth()
#
# sqldf stuff and plots
# Yearly summary
mty <- sqldf("Select Year,avg(Ra) as ct from monthly where Ra >=1 group by Year")
ggplot(mty,aes(Year,ct)) +geom_bar(stat="summary",fun.y="sum")  + geom_smooth() +ggtitle("Total Sunspots by Year")
#
# Monthly Summary
mtm <- sqldf("Select Month,avg(Ra) as ct from monthly where Ra >=1 group by Month")
ggplot(data=mtm,aes(Month,ct)) + geom_smooth() +geom_line()
# Day Summary
mtd <- sqldf("Select Day,avg(Ra) as ct from daily where Ra >=1 group by Day")
ggplot(data=mtd,aes(Day,ct,size=ct)) + geom_smooth() +geom_point() + ggtitle("Sunspot Summary by Day")
# Bar Plots + Smooth
#




