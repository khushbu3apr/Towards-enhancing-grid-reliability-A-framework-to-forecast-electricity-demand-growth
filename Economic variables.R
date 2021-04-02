
### Merging Unemployment Data
file <- "E:\\NYISO\\Unemployment\\WEST\\WEST.csv"
UE <- read.csv(file, header = TRUE)
head(UE)
UE$Avg <- rowMeans(UE[2:8])
head(UE)
UE$Date <- as.POSIXlt(UE$DATE, format="%m/%d/%Y")
UE$Date <- as.Date(UE$Date)
UE$month <- month(as.POSIXlt(UE$Date, format="%m/%d/%Y"))
UE$year <-  year(as.POSIXlt(UE$Date, format="%m/%d/%Y"))
West$Date <- as.Date(West$Date)
West$year <-  year(as.POSIXlt(West$Date, format="%Y/%m/%d"))
West$month <- month(as.POSIXlt(West$Date, format="%Y/%m/%d"))
UE <- UE[UE[["year"]] >= "2010", ]
UE <- UE[c("Avg","month","year")]
rownames(UE) <- NULL

West_1 <- merge(West,UE,by=c("month","year"), all.x = T)
names(West_1)[names(West_1)=="Avg"] <- "Unemployment"
head(West_1)

### Merging Poverty Data
file_1 <- "E:\\NYISO\\Poverty\\WEST.csv"
PO <- read.csv(file_1, header = TRUE)
head(PO)
PO$Avg <- rowMeans(PO[2:8])
head(PO)
PO$Date <- as.POSIXlt(PO$DATE, format="%m/%d/%Y")
PO$Date <- as.Date(PO$Date)
PO$month <- month(as.POSIXlt(PO$Date, format="%m/%d/%Y"))
PO$year <-  year(as.POSIXlt(PO$Date, format="%m/%d/%Y"))

PO <- PO[PO[["year"]] >= "2010", ]
PO <- PO[c("Avg","month","year")]
rownames(PO) <- NULL

West_2 <- merge(West_1,PO,by=c("month","year"), all.x = T)
names(West_2)[names(West_2)=="Avg"] <- "Poverty"
head(West_2)


### Merging Household Income
file_2 <- "E:\\NYISO\\House Hold Income\\WEST.csv"
HI <- read.csv(file_2, header = TRUE)
head(HI)
HI$Avg <- rowMeans(HI[2:8])
head(HI)
HI$Date <- as.POSIXlt(HI$DATE, format="%m/%d/%Y")
HI$Date <- as.Date(HI$Date)
HI$month <- month(as.POSIXlt(HI$Date, format="%m/%d/%Y"))
HI$year <-  year(as.POSIXlt(HI$Date, format="%m/%d/%Y"))

HI <- HI[HI[["year"]] >= "2010", ]
HI <- HI[c("Avg","month","year")]
rownames(HI) <- NULL

West_3 <- merge(West_2,HI,by=c("month","year"), all.x = T)
names(West_3)[names(West_3)=="Avg"] <- "Household_Income"
head(West_3)
West <- West_3[-c(1,2)]
West$Hour <- West$Hour+5
West$Time <- strftime(as.POSIXct(West$Hour * 60 *60, origin = Sys.Date(), tz = "ET"), format = "%H:%M:%S")
West <- West[-c(2)]
head(West)
tail(West)
write.csv(West,"E:\\NYISO\\Final Datasets\\WEST.csv")
summary(West)

## Summer
West$month <- month(as.POSIXlt(West$Date, format="%Y/%m/%d"))
West_Summer <- subset(West, month > 4 & month < 10)
West_Summer <- West_Summer[-c(15)]
head(West_Summer)
write.csv(West_Summer,"E:\\NYISO\\Final Datasets\\WEST_Summer.csv")
