#install.packages("lubridate")
library(lubridate)
#install.packages("dplyr")
library(dplyr)

s <- "E:\\Energy Forecasting Project\\Load data\\"
files <- list.files(s, pattern="*.csv", full.names=TRUE, recursive = TRUE)
fname = "E:\\Energy Forecasting Project\\Load data\\Electricity_Demand.csv"

 #Converting Electricity demand data hourly 

for (file in files) {
  tryCatch({
    load <- read.csv(file, header = TRUE)
    load$x <- as.POSIXct(strptime(load$RTD.End.Time.Stamp, "%Y/%m/%d %H:%M:%S", tz="UTC")) - as.difftime(5, unit="mins")
    load$Date  <- as.Date(load$x)
    load$hour <- as.POSIXlt(load$x)$hour + 1
    load$RTD.Actual.Load[is.na(load$RTD.Actual.Load)] <- round(mean(load$RTD.Actual.Load, na.rm = TRUE))
    df <- aggregate(load$RTD.Actual.Load, by = list(load$Date, load$hour, load$Zone.Name), max)
    df$mean <- aggregate(load$RTD.Actual.Load, by = list(load$Date, load$hour, load$Zone.Name), mean)$x
    df$sum <- aggregate(load$RTD.Actual.Load, by = list(load$Date, load$hour, load$Zone.Name), sum)$x
    colnames(df) <- c("Date","Hour","Zone.Name", "Peak","Average", "Total")
    df$day <- weekdays(as.POSIXct(df$Date))
    df$year <- year(ymd(df$Date))
    df$month <- month(ymd(df$Date))
    write.table(df, file=fname, sep = ",", row.names = FALSE, append = TRUE, col.names=!file.exists(fname))
    print(file)
  }, error = function(cond) {
    print(cond)
    print("here")
    load <- read.csv(file, header = TRUE)
    load$x <- as.POSIXct(strptime(load$RTD.End.Time.Stamp, "%m/%d/%Y %H:%M", tz="UTC")) - as.difftime(5, unit="mins")
    load$Date  <- as.Date(load$x)
    load$hour <- as.POSIXlt(load$x)$hour + 1
    load$RTD.Actual.Load[is.na(load$RTD.Actual.Load)] <- round(mean(load$RTD.Actual.Load, na.rm = TRUE))
    df <- aggregate(load$RTD.Actual.Load, by = list(load$Date, load$hour, load$Zone.Name), max)
    df$mean <- aggregate(load$RTD.Actual.Load, by = list(load$Date, load$hour, load$Zone.Name), mean)$x
    df$sum <- aggregate(load$RTD.Actual.Load, by = list(load$Date, load$hour, load$Zone.Name), sum)$x
    colnames(df) <- c("Date","Hour","Zone.Name", "Peak", "Average", "Total")
    df$day <- weekdays(as.POSIXct(df$Date))
    df$year <- year(ymd(df$Date))
    df$month <- month(ymd(df$Date))
    write.table(df, file=fname, sep = ",", row.names = FALSE, append = TRUE, col.names=!file.exists(fname))
    print(file)
  })
}

fname = "C:/Users/khush/Documents/IP/Electricity_Demand.csv"
load <- read.csv(fname, header = TRUE)
summary(load)

#Splitting Electricity-Demand into 2 dataframes

Electricity_Demand.1 <- load[c(0:900000),]
write.csv(Electricity_Demand.1,"E:\\Energy Forecasting Project\\Electricity_Demand.1.csv")
Electricity_Demand.2 <- load[c(900001:1430649),]
write.csv(Electricity_Demand.2,"E:\\Energy Forecasting Project\\Electricity_Demand.2.csv")

load <- na.omit(load)
summary(load)




