library(data.table)
library(forecast)
library(RSQLite)
library(ggplot2)
library(lubridate)
library(xts)
library(sqldf)

daily <-fread("https://www.aavso.org/sites/default/files/solar/NOAAfiles/NOAAdaily.csv")
monthly <- fread("https://www.aavso.org/sites/default/files/solar/NOAAfiles/NOAAmonthly.csv")
#
# Convert  Year, Month,Day and Vote to factors
daily$Year <- as.factor(daily$Year)
daily$Month <-as.factor(daily$Month)
daily$Day <- as.factor(daily$Day)
daily$Vote <- as.factor(daily$Vote)
# Added factors for Year and Month field in Monthly table
monthly$Year <- as.factor(monthly$Year)
monthly$Month <-as.factor(monthly$Month)
#
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
daily$ma <-ma(daily$Ra,order=180)
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
ggplot(data=mtd,aes(Day,ct,size=ct)) + geom_smooth() +geom_point() 
    + ggtitle("Sunspot Summary by Day")
vote <- sqldf("select Year,count(*) as ct from daily where Vote=1 group by Year")
ggplot(vote,aes(Year,ct)) +geom_bar(stat="summary",fun.y="sum")  + geom_smooth() +labs(title="Total Reported Days with  Sunspots by Year")
# Bar Plots + Smooth
#
yrmon <- sqldf("select Year,Month,count(*) as ct from daily where Vote=1 group by Year,Month")
ggplot(yrmon,aes(Year,ct)) +geom_bar(stat="summary",fun.y="sum")  + geom_smooth() +labs(title="Total Reported Days with  Sunspots by Year")
# Bar Plots + Smooth
# Write sumary tables to solar.sqlite2 database
#
db <- dbConnect(SQLite(), dbname="C:/Users/davidjayjackson/Documents/GitHub/db/solar.sqlite3")
dbListTables(db)
# # Creat table and Insert data.frame(overwrites existing table)
# Convert Ymd field to Character for import into sqlite
DAILY$Ymd <- as.character(DAILY$Ymd)
dbWriteTable(db, "mtd", mtd,overwrite=TRUE)
dbWriteTable(db, "mtm", mtm,overwrite=TRUE)
dbWriteTable(db, "mty", mty,overwrite=TRUE)
dbWriteTable(db, "vote", vote,overwrite=TRUE)
dbWriteTable(db, "yrmon", yrmon,overwrite=TRUE)
dbListTables(db)
#
dbDisconnect(db)


