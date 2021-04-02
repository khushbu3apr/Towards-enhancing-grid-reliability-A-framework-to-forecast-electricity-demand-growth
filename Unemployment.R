file <- "E:\\NYISO\\Data\\Unemployment\\WEST.csv"
UE <- read.csv(file, header = TRUE)
summary(UE)


str(UE)
rownames(UE) <- UE$DATE
UE <- UE[-1]
k <- rowMeans(UE)
UE$Avg <- k


