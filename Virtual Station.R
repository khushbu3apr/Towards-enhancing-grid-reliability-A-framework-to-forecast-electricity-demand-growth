library(lubridate)
library(dplyr)
library(corrplot)
library(ggplot2)
library(data.table)
library(svMisc)

## Loading Weather Stations ##

### Buffalo
file_1 <- "E:\\NYISO\\Weather Data\\WEST_BUFFALO.CSV"
df_1<- read.csv(file_1, header = TRUE)
df_1<-df_1[df_1[["Date"]] >= "2010-01-01", ]
rownames(df_1) <- NULL

### Jamestown
file_2 <- "E:\\NYISO\\Weather Data\\WEST_JAMESTOWN.CSV"
df_2<- read.csv(file_2, header = TRUE)
df_2<-df_2[df_2[["Date"]] >= "2010-01-01", ]
rownames(df_2) <- NULL

### Niagara
file_3 <- "E:\\NYISO\\Weather Data\\WEST_NIAGARA.CSV"
df_3<- read.csv(file_3, header = TRUE)
df_3<-df_3[df_3[["Date"]] >= "2010-01-01", ]
rownames(df_3) <- NULL

### Dunkirk
file_4 <- "E:\\NYISO\\Weather Data\\WEST_DUNKIRK.CSV"
df_4<- read.csv(file_4, header = TRUE)
df_4<-df_4[df_4[["Date"]] >= "2010-01-01", ]
rownames(df_4) <- NULL

## Loading Population Data
file_5 <- "E:\\NYISO\\Population\\WEST.csv"
p_1 <- read.csv(file_5, header = TRUE)
p_1$month <- month(as.POSIXlt(p_1$DATE, format="%m/%d/%Y"))
p_1$year <-  year(as.POSIXlt(p_1$DATE, format="%m/%d/%Y"))


## Merging Population data with Weather station
df_1 <- merge(df_1,p_1,by=c("month","year"), all.x = T)
Buffalo <- df_1[c("Date","Hour","HourlyDewPointTemperature","HourlyDryBulbTemperature","HourlyPrecipitation","HourlyRelativeHumidity","HourlyStationPressure","HourlyWetBulbTemperature","HourlyWindSpeed","Peak","Erie")]
names(Buffalo)[names(Buffalo)=="Erie"] <- "Population"
head(Buffalo)

df_3 <- merge(df_3,p_1,by=c("month","year"), all.x = T)
Niagara <- df_3[c("Date","Hour","HourlyDewPointTemperature","HourlyDryBulbTemperature","HourlyPrecipitation","HourlyRelativeHumidity","HourlyStationPressure","HourlyWetBulbTemperature","HourlyWindSpeed","Peak","Niagara")]
names(Niagara)[names(Niagara)=="Niagara"] <- "Population"
head(Niagara)

### Combining Weather Stations(Jamestown, Dunkirk)
df_2 <- df_2[-c(0:6)]
df_4 <- df_4[-c(0:6)]
df_5 <- Reduce(`+`, mget(paste0("df_", c(2,4))))/2
df_5[c("Date","month","year")] <- df_1[c("Date","month","year")]

df_5 <- merge(df_5,p_1,by=c("month","year"), all.x = T)
Chautauqua <- df_3[c("Date","Hour","HourlyDewPointTemperature","HourlyDryBulbTemperature","HourlyPrecipitation","HourlyRelativeHumidity","HourlyStationPressure","HourlyWetBulbTemperature","HourlyWindSpeed","Peak","Chautauqua")]
names(Chautauqua)[names(Chautauqua)=="Chautauqua"] <- "Population"
head(Chautauqua)


##weighing the weather stations with population
for (i in 0:87649){
  Niagara$Weight[i] <- Niagara$Population[i]/(Niagara$Population[i]+Buffalo$Population[i]+Chautauqua$Population[i])
}

for (i in 0:87649){
  Buffalo$Weight[i] <- Buffalo$Population[i]/(Niagara$Population[i]+Buffalo$Population[i]+Chautauqua$Population[i])
}

for (i in 0:87649){
  Chautauqua$Weight[i] <- Chautauquahead$Population[i]/(Niagara$Population[i]+Buffalo$Population[i]+Chautauqua$Population[i])
}


head(Niagara[3:10])
for (i in 0:87649){
  Niagara[3:10][i,] <- Niagara[3:10][i,]*Niagara$Weight[i]
}

for (i in 0:87649){
  Buffalo[3:10][i,] <- Buffalo[3:10][i,]*Buffalo$Weight[i]
}

for (i in 0:87649){
  Chautauqua[3:10][i,] <- Chautauqua[3:10][i,]*Chautauqua$Weight[i]
}

for (i in 0:87649){
  Niagara[11][i,] <- Niagara[11][i,]*Niagara$Weight[i]
}

for (i in 0:87649){
  Buffalo[11][i,] <- Buffalo[11][i,]*Buffalo$Weight[i]
}

for (i in 0:87649){
  Chautauqua[11][i,] <- Chautauqua[11][i,]*Chautauqua$Weight[i]
}

West <- Niagara
for (i in 0:87649){
  West$Date[i] <- Niagara$Date[i]
  West$Hour[i] <- Niagara$Hour[i] + Buffalo$Hour[i] +Chautauqua$Hour[i]
  West$HourlyDewPointTemperature[i] <- Niagara$HourlyDewPointTemperature[i] + Buffalo$HourlyDewPointTemperature[i] +Chautauqua$HourlyDewPointTemperature[i]
  West$HourlyDryBulbTemperature[i] <- Niagara$HourlyDryBulbTemperature[i] + Buffalo$HourlyDryBulbTemperature[i] +Chautauqua$HourlyDryBulbTemperature[i]
  West$HourlyPrecipitation[i] <- Niagara$HourlyPrecipitation[i] + Buffalo$HourlyPrecipitation[i] +Chautauqua$HourlyPrecipitation[i]
  West$HourlyRelativeHumidity[i] <- Niagara$HourlyRelativeHumidity[i] + Buffalo$HourlyRelativeHumidity[i] +Chautauqua$HourlyRelativeHumidity[i]
  West$HourlyStationPressure[i] <- Niagara$HourlyStationPressure[i] + Buffalo$HourlyStationPressure[i] +Chautauqua$HourlyStationPressure[i]
  West$HourlyWetBulbTemperature[i] <- Niagara$HourlyWetBulbTemperature[i] + Buffalo$HourlyWetBulbTemperature[i] +Chautauqua$HourlyWetBulbTemperature[i]
  West$HourlyWindSpeed[i] <- Niagara$HourlyWindSpeed[i] + Buffalo$HourlyWindSpeed[i] +Chautauqua$HourlyWindSpeed[i]
  West$Peak[i] <- Niagara$Peak[i] + Buffalo$Peak[i] +Chautauqua$Peak[i]
  West$Population[i] <- Niagara$Population[i] + Buffalo$Population[i] +Chautauqua$Population[i]
  West$Weight[i] <- Niagara$Weight[i] + Buffalo$Weight[i] +Chautauqua$Weight[i]
}
head(West)
tail(West)
West <- West[-12]
write.csv(West,"E:\\NYISO\\Virtual Stations\\WEST.csv")
