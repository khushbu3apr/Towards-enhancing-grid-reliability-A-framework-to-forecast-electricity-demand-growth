#install.packages("lubridate")
library(lubridate)
#install.packages("dplyr")
library(dplyr)
library(corrplot)
library(ggplot2)


s <- "E:\\NYISO\\Weather Data\\WEST"
files <- c( "E:\\NYISO\\Weather Data\\WEST/JAMESTOWN_1.csv","E:\\NYISO\\Weather Data\\WEST/JAMESTOWN_2.csv")
fname = "E:\\NYISO\\Weather Data\\WEST_JAMESTOWN.csv"

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
Weather<-Weather[Weather[["Date"]] >= "2007-01-01", ]

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
  Weather$HourlyWindSpeed[i] <- AK(Weather, i)
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




#Averaging data for every hour
Weather <- Weather %>% group_by(Date,Hour) %>% summarise(HourlyDewPointTemperature = mean(HourlyDewPointTemperature), HourlyDryBulbTemperature = mean(HourlyDryBulbTemperature), HourlyPrecipitation = mean(HourlyPrecipitation), HourlyRelativeHumidity = mean(HourlyRelativeHumidity), HourlyStationPressure = mean(HourlyStationPressure), HourlyWetBulbTemperature = mean(HourlyWetBulbTemperature), HourlyWindSpeed = mean(HourlyWindSpeed))
summary(Weather)
str(Weather)


#data_ggp <- data.frame(x = Weather$Date,                            # Reshape data frame
#                       y = c(Weather$HourlyDewPointTemperature,Weather$HourlyDryBulbTemperature,Weather$HourlyPrecipitation,Weather$HourlyRelativeHumidity,Weather$HourlyStationPressure,Weather$HourlyWetBulbTemperature,Weather$HourlyWindSpeed),
#                       group = c(rep("HourlyDewPointTemperature", nrow(Weather)),
#                                 rep("HourlyDryBulbTemperature", nrow(Weather)),
#                                 rep("HourlyPrecipitation", nrow(Weather)),
#                                 rep("HourlyRelativeHumidity", nrow(Weather)),
#                                 rep("HourlyStationPressure", nrow(Weather)),
#                                 rep("HourlyWetBulbTemperature", nrow(Weather)),
#                                 rep("HourlyWindSpeed", nrow(Weather))))

#ggp <- ggplot(data_ggp, aes(x, y, col = group)) +             # Create ggplot2 plot
#  geom_line()
#ggp + facet_grid(group ~ .)   


#for(i in 1:ncol(Weather)){
#  Weather[is.na(Weather[,i]), i] <- mean(Weather[,i], na.rm = TRUE)
#}

elec = "E:\\NYISO\\Load Data\\Electricity_Demand.csv"
load <- read.csv(elec, header = TRUE)

#Loading West region from Load
Load_west <- load[ which(load$Zone.Name=='WEST'),]

#Merging West and Weather.West
total <- merge(Load_west,Weather,by=c("Date","Hour"), all.x = T)
#total <- na.omit(total)
head(total)
total<-total[total[["Date"]] >= "2007-01-01", ]


total$DAY <- format(as.Date(total$Date,format="%Y-%m-%d"), format = "%d")

total <- total[c("Date","DAY","Hour","day","year","month",'HourlyDewPointTemperature','HourlyDryBulbTemperature','HourlyPrecipitation','HourlyRelativeHumidity','HourlyStationPressure','HourlyWetBulbTemperature','HourlyWindSpeed',"Peak")]

summary(total)
#Replacing DPT NA values by creating function
XY_1 <- function(df, i) {
  Se1 <-df$Hour == df$Hour[i] & df$DAY == df$DAY[i] & df$month == df$month[i] & df$year == (df$year[i]-1)
  imputed <- mean(df$HourlyDewPointTemperature[Se1],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(total$HourlyDewPointTemperature))) {
  total$HourlyDewPointTemperature[i] <- XY_1(total, i)
}


#Replacing WIND NA values by creating function
AK_1 <- function(df, i) {
  Se1 <- df$Hour == df$Hour[i] & df$DAY == df$DAY[i] & df$month == df$month[i] & df$year == (df$year[i]-1)
  imputed <- mean(df$HourlyWindSpeed[Se1],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(total$HourlyWindSpeed))) {
  total$HourlyWindSpeed[i] <- AK_1(total, i)
}

#Replacing DBT NA values by creating function
AB_1 <- function(df1, i) {
  Se2 <- df1$Hour == df1$Hour[i] & df1$DAY == df1$DAY[i] & df1$month == df1$month[i] & df1$year == (df1$year[i]-1)
  imputed <- mean(df1$HourlyDryBulbTemperature[Se2],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(total$HourlyDryBulbTemperature))) {
  total$HourlyDryBulbTemperature[i] <- AB_1(total, i)
}

#Replacing HRH NA values by creating function
CD_1 <- function(df2, i) {
  Se3 <- df2$Hour == df2$Hour[i] & df2$DAY == df2$DAY[i] & df2$month == df2$month[i] & df2$year == (df2$year[i]-1)
  imputed <- mean(df2$HourlyRelativeHumidity[Se3],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(total$HourlyRelativeHumidity))) {
  total$HourlyRelativeHumidity[i] <- CD_1(total, i)
}

#Replacing HSP NA values by creating function
EF_1 <- function(df7, i) {
  Se4 <- df7$Hour == df7$Hour[i] & df7$DAY == df7$DAY[i] & df7$month == df7$month[i] & df7$year == (df7$year[i]-1)
  imputed <- mean(df7$HourlyStationPressure[Se4],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(total$HourlyStationPressure))) {
  total$HourlyStationPressure[i] <- EF_1(total, i)
}

#Replacing WBT NA values by creating function
GH_1 <- function(df4, i) {
  Se5 <- df4$Hour == df4$Hour[i] & df4$DAY == df4$DAY[i] & df4$month == df4$month[i] & df4$year == (df4$year[i]-1)
  imputed <- mean(df4$HourlyWetBulbTemperature[Se5],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(total$HourlyWetBulbTemperature))) {
  total$HourlyWetBulbTemperature[i] <- GH_1(total, i)
}

#Replacing HWS NA values by creating function
IJ_1 <- function(df5, i) {
  Se6 <- df5$Hour == df5$Hour[i] & df5$DAY == df5$DAY[i] & df5$month == df5$month[i] & df5$year == (df5$year[i]-1)
  imputed <- mean(df5$HourlyWindSpeed[Se6],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(total$HourlyWindSpeed))) {
  total$HourlyWindSpeed[i] <- IJ_1(total, i)
}

#Replacing HP values by creating function
KL_1 <- function(df6, i) {
  Se7 <- df6$Hour == df6$Hour[i] & df6$DAY == df6$DAY[i] & df6$month == df6$month[i] & df6$year == (df6$year[i]-1)
  imputed <- mean(df6$HourlyPrecipitation[Se7],na.rm = TRUE)
  if(is.nan(imputed)){
    imputed <- NA
  }
  return(imputed)
}

for (i in which(is.na(total$HourlyPrecipitation))) {
  total$HourlyPrecipitation[i] <- KL_1(total, i)
}


summary(total)
str(total)




write.csv(total,"E:\\NYISO\\Weather Data\\WEST_JAMESTOWN.csv")
