#install.packages("lubridate")
library(lubridate)
#install.packages("dplyr")
library(dplyr)
library(corrplot)
library(ggplot2)

s <- "E:\\NYISO\\Weather Data"
files <- list.files(s, pattern="*.csv", full.names=TRUE, recursive = TRUE)
fname = "E:\\NYISO\\Weather Data\\Weather_West.csv"

#Assign Date and Hour 
for (file in files) {
  tryCatch({
    Weather<- read.csv(file, header = TRUE)
    Weather<- Weather[c("STATION",'DATE','HourlyDewPointTemperature','HourlyDryBulbTemperature','HourlyPrecipitation','HourlyRelativeHumidity','HourlyStationPressure','HourlyWetBulbTemperature','HourlyWindSpeed')]
    Weather$DATE <- as.POSIXct(Weather$DATE, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    Weather$Date <- as.Date(Weather$DATE)
    Weather$Hour <- as.POSIXlt(Weather$DATE)$hour+1
    write.table(Weather, file=fname, sep = ",", row.names = FALSE, append = TRUE, col.names=!file.exists(fname))
    print(file)
  }, error = function(cond) {
    
    print(cond)
    print("here")
    Weather <- read.csv(file, header = TRUE)
    Weather<-Weather[c("STATION",'DATE','HourlyDewPointTemperature','HourlyDryBulbTemperature','HourlyPrecipitation','HourlyRelativeHumidity','HourlyStationPressure','HourlyWetBulbTemperature','HourlyWindSpeed')]
    Weather$DATE <- as.POSIXct(Weather$DATE, format = "%Y-%m-%dT%H:%M:%S",tz = "UTC")
    Weather$Date <- as.Date(Weather$DATE)
    Weather$Hour <- as.POSIXlt(Weather$DATE)$hour+1
    write.table(Weather, file=fname, sep = ",", row.names = FALSE, append = TRUE, col.names=!file.exists(fname))
    print(file)
  })
}

Weather <- read.csv(fname, header = TRUE)

#Convering variables into numeric
Weather[3:9] <- lapply(Weather[3:9], function(x) as.numeric(as.character(x)))
str(Weather)
Weather<- Weather[-1,-2]

#Replacing DPT NA values by creating function
XY <- function(df, i) {
  Se1 <- df$Date == df$Date[i] & df$Hour == df$Hour[i]
  imputed <- mean(df$HourlyDewPointTemperature[Se1],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(Weather$HourlyDewPointTemperature))) {
  Weather$HourlyDewPointTemperature[i] <- XY(Weather, i)
}

#Replacing Wind NA values by creating function
AA <- function(df, i) {
  Se1 <- df$Date == df$Date[i] & df$Hour == df$Hour[i]
  imputed <- mean(df$HourlyWindSpeed[Se1],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(Weather$HourlyWindSpeed))) {
  Weather$HourlyWindSpeed[i] <- AA(Weather, i)
}

#Replacing WIND NA values by creating function
AK <- function(df, i) {
  Se1 <- df$Date == df$Date[i] & df$Hour == df$Hour[i]
  imputed <- mean(df$HourlyWindSpeed[Se1],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(Weather$HourlyWindSpeed))) {
  Weather$HourlyWindSpeed[i] <- XY(Weather, i)
}

#Replacing DBT NA values by creating function
AB <- function(df1, i) {
  Se2 <- df1$Date == df1$Date[i] & df1$Hour == df1$Hour[i]
  imputed <- mean(df1$HourlyDryBulbTemperature[Se2],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(Weather$HourlyDryBulbTemperature))) {
  Weather$HourlyDryBulbTemperature[i] <- AB(Weather, i)
}

#Replacing HRH NA values by creating function
CD <- function(df2, i) {
  Se3 <- df2$Date == df2$Date[i] & df2$Hour == df2$Hour[i]
  imputed <- mean(df2$HourlyRelativeHumidity[Se3],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(Weather$HourlyRelativeHumidity))) {
  Weather$HourlyRelativeHumidity[i] <- CD(Weather, i)
}

#Replacing HSP NA values by creating function
EF <- function(df7, i) {
  Se4 <- df7$Date == df7$Date[i] & df7$Hour == df7$Hour[i]
  imputed <- mean(df7$HourlyStationPressure[Se4],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(Weather$HourlyStationPressure))) {
  Weather$HourlyStationPressure[i] <- EF(Weather, i)
}

#Replacing WBT NA values by creating function
GH <- function(df4, i) {
  Se5 <- df4$Date == df4$Date[i] & df4$Hour == df4$Hour[i]
  imputed <- mean(df4$HourlyWetBulbTemperature[Se5],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(Weather$HourlyWetBulbTemperature))) {
  Weather$HourlyWetBulbTemperature[i] <- GH(Weather, i)
}

#Replacing HWS NA values by creating function
IJ <- function(df5, i) {
  Se6 <- df5$Date == df5$Date[i] & df5$Hour == df5$Hour[i]
  imputed <- mean(df5$HourlyWindSpeed[Se6],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(Weather$HourlyWindSpeed))) {
  Weather$HourlyWindSpeed[i] <- IJ(Weather, i)
}

#Replacing HP values by creating function
KL <- function(df6, i) {
  Se7 <- df6$Date == df6$Date[i] & df6$Hour == df6$Hour[i]
  imputed <- mean(df6$HourlyPrecipitation[Se7],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(Weather$HourlyPrecipitation))) {
  Weather$HourlyPrecipitation[i] <- KL(Weather, i)
}

summary(Weather)
new_DF<-subset(Weather,Weather$HourlyStationPressure=="NA") 

#Averaging data for every hour
Weather_West <- Weather %>% group_by(Date,Hour) %>% summarise(mean_DPT = mean(HourlyDewPointTemperature), mean_DBT = mean(HourlyDryBulbTemperature), mean_HP = mean(HourlyPrecipitation), mean_HRH = mean(HourlyRelativeHumidity), mean_HSP = mean(HourlyStationPressure), mean_WBT = mean(HourlyWetBulbTemperature), mean_HWS = mean(HourlyWindSpeed))
summary(Weather_West)

#Loading West region from Load
Load_west <- load[ which(load$Zone.Name=='WEST'), ]

#Merging West and Weather.West
total <- merge(Load_west,Weather_West,by=c("Date","Hour"), all.x = T)
total<- read.csv(file = file, header = TRUE)
write.csv(total,"E:\\NYISO\\total.csv")


df <- total[,-13]
df <- na.omit(df)

df <- df[,c(1,10,11,12,13,14,15,2,9,5,6,4)]

colnames(df) <- c("Date","DewPointTemperature","DryBulbTemperature", "RelativeHumidity", "StationPressure", "WetBulbTemperature", "WindSpeed","Hour","Month","Hourly5minAverageLoad","HourlyTotalLoad","HourlyPeakLoad")

df$day <- strftime(df$Date, "%w")
df$day <- as.integer(df$day)
df <- df[,-8]

df$Date <- as.Date(df$Date)

df$Hour <- as.integer(df$Hour)

df$Hour <- df$Hour*3600

df$trial <- seconds_to_period(df$Hour)

df$Time <- sprintf('%02d:%02d:%02d', df$trial@hour, minute(df$trial), second(df$trial))




write.csv(df,"E:\\Energy Forecasting Project\\total_west.csv")

