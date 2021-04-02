library(lubridate)
library(dplyr)
library(corrplot)
library(ggplot2)
library(MLmetrics)
library(pracma)



file <- "E:\\NYISO\\Weather Data\\WEST_JAMESTOWN.CSV"
df<- read.csv(file, header = TRUE)
rownames(df) <- NULL
head(df)


df$DAY <- as.factor(df$DAY)
df$month <- as.factor(df$month)
df$day <- as.factor(df$day)
df$Hour <- as.factor(df$Hour)
str(df)
df <- df[c("month","day","Hour","HourlyDryBulbTemperature","Peak")]
df <- na.omit(df)
## 75% of the sample size
smp_size <- floor(0.75 * nrow(df))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]


lmMod <- lm( Peak ~  month +  day +  Hour +  day: Hour +  HourlyDryBulbTemperature + I( HourlyDryBulbTemperature^2) + I( HourlyDryBulbTemperature^3) +  month: HourlyDryBulbTemperature +  month:I( HourlyDryBulbTemperature^2) +  month:I( HourlyDryBulbTemperature^3) +  Hour: HourlyDryBulbTemperature +  Hour:I( HourlyDryBulbTemperature^2) +  Hour:I( HourlyDryBulbTemperature^3), data = train )
summary(lmMod)

p_load <- predict(lmMod , test)

MAPE(p_load,test$Peak)



## LINEAR COMBINATION

file_1 <- "E:\\NYISO\\Weather Data\\WEST_BUFFALO.CSV"
df_1<- read.csv(file_1, header = TRUE)
rownames(df_1) <- NULL
head(df_1)
df_1$month <- as.numeric(df_1$month)
df_1$day <- as.factor(df_1$day)
df_1$Hour <- as.factor(df_1$Hour)
df_1 <- df_1[c("month","day","Hour","HourlyDryBulbTemperature","Peak")]
df_1 <- subset(df_1, month > 5 & month < 10)
df_1$month <- as.factor(df_1$month)
nrow(df_1)


file_2 <- "E:\\NYISO\\Weather Data\\WEST_JAMESTOWN.CSV"
df_2<- read.csv(file_1, header = TRUE)
df_2 <- subset(df_2, month > 5 & month < 10)
rownames(df_2) <- NULL
nrow(df_2)


file_3 <- "E:\\NYISO\\Weather Data\\WEST_NIAGARA.CSV"
df_3<- read.csv(file_3, header = TRUE)
df_3 <- subset(df_3, month > 5 & month < 10)
rownames(df_3) <- NULL
nrow(df_3)


file_4 <- "E:\\NYISO\\Weather Data\\WEST_DUNKIRK.CSV"
df_4<- read.csv(file_4, header = TRUE)
df_4 <- subset(df_4, month > 5 & month < 10)
rownames(df_4) <- NULL
nrow(df_4)


df_1$HourlyDryBulbTemperature <- 0.4*df_1$HourlyDryBulbTemperature + 0.3*df_2$HourlyDryBulbTemperature + 0.2*df_3$HourlyDryBulbTemperature + 0.1*df_4$HourlyDryBulbTemperature
df_1 <- na.omit(df_1)

set.seed(123)
smp_size <- floor(0.75 * nrow(df_1))
train_ind <- sample(seq_len(nrow(df_1)), size = smp_size)

train <- df_1[train_ind, ]
test <- df_1[-train_ind, ]


lmMod <- lm( Peak ~  month +  day +  Hour +  day: Hour +  HourlyDryBulbTemperature + I( HourlyDryBulbTemperature^2) + I( HourlyDryBulbTemperature^3) +  month: HourlyDryBulbTemperature +  month:I( HourlyDryBulbTemperature^2) +  month:I( HourlyDryBulbTemperature^3) +  Hour: HourlyDryBulbTemperature +  Hour:I( HourlyDryBulbTemperature^2) +  Hour:I( HourlyDryBulbTemperature^3), data = train )
summary(lmMod)

p_load <- predict(lmMod , test)

MAPE(p_load,test$Peak)


## EXPONENTIAL COMBINATION
file_1 <- "E:\\NYISO\\Weather Data\\WEST_BUFFALO.CSV"
df_1<- read.csv(file_1, header = TRUE)
rownames(df_1) <- NULL
head(df_1)
df_1$month <- as.numeric(df_1$month)
df_1$day <- as.factor(df_1$day)
df_1$Hour <- as.factor(df_1$Hour)
df_1 <- df_1[c("month","day","Hour","HourlyDryBulbTemperature","Peak")]
df_1 <- subset(df_1, month > 5 & month < 10)
df_1$month <- as.factor(df_1$month)
nrow(df_1)


file_2 <- "E:\\NYISO\\Weather Data\\WEST_JAMESTOWN.CSV"
df_2<- read.csv(file_1, header = TRUE)
df_2 <- subset(df_2, month > 5 & month < 10)
rownames(df_2) <- NULL
nrow(df_2)


file_3 <- "E:\\NYISO\\Weather Data\\WEST_NIAGARA.CSV"
df_3<- read.csv(file_3, header = TRUE)
df_3 <- subset(df_3, month > 5 & month < 10)
rownames(df_3) <- NULL
nrow(df_3)


file_4 <- "E:\\NYISO\\Weather Data\\WEST_DUNKIRK.CSV"
df_4<- read.csv(file_4, header = TRUE)
df_4 <- subset(df_4, month > 5 & month < 10)
rownames(df_4) <- NULL
nrow(df_4)



df_1$HourlyDryBulbTemperature <- 0.53333*df_1$HourlyDryBulbTemperature + 0.2667*df_2$HourlyDryBulbTemperature + 0.1333*df_3$HourlyDryBulbTemperature + 0.0667*df_4$HourlyDryBulbTemperature
df_1 <- na.omit(df_1)

set.seed(123)
smp_size <- floor(0.75 * nrow(df_1))
train_ind <- sample(seq_len(nrow(df_1)), size = smp_size)

train <- df_1[train_ind, ]
test <- df_1[-train_ind, ]


lmMod <- lm( Peak ~  month +  day +  Hour +  day: Hour +  HourlyDryBulbTemperature + I( HourlyDryBulbTemperature^2) + I( HourlyDryBulbTemperature^3) +  month: HourlyDryBulbTemperature +  month:I( HourlyDryBulbTemperature^2) +  month:I( HourlyDryBulbTemperature^3) +  Hour: HourlyDryBulbTemperature +  Hour:I( HourlyDryBulbTemperature^2) +  Hour:I( HourlyDryBulbTemperature^3), data = train )
summary(lmMod)

p_load <- predict(lmMod , test)

MAPE(p_load,test$Peak)



## MAPE COMBINATION

file_1 <- "E:\\NYISO\\Weather Data\\WEST_BUFFALO.CSV"
df_1<- read.csv(file_1, header = TRUE)
rownames(df_1) <- NULL
head(df_1)
df_1$month <- as.numeric(df_1$month)
df_1$day <- as.factor(df_1$day)
df_1$Hour <- as.factor(df_1$Hour)
df_1 <- df_1[c("month","day","Hour","HourlyDryBulbTemperature","Peak")]
df_1 <- subset(df_1, month > 5 & month < 10)
df_1$month <- as.factor(df_1$month)
nrow(df_1)


file_2 <- "E:\\NYISO\\Weather Data\\WEST_JAMESTOWN.CSV"
df_2<- read.csv(file_1, header = TRUE)
df_2 <- subset(df_2, month > 5 & month < 10)
rownames(df_2) <- NULL
nrow(df_2)


file_3 <- "E:\\NYISO\\Weather Data\\WEST_NIAGARA.CSV"
df_3<- read.csv(file_3, header = TRUE)
df_3 <- subset(df_3, month > 5 & month < 10)
rownames(df_3) <- NULL
nrow(df_3)


file_4 <- "E:\\NYISO\\Weather Data\\WEST_DUNKIRK.CSV"
df_4<- read.csv(file_4, header = TRUE)
df_4 <- subset(df_4, month > 5 & month < 10)
rownames(df_4) <- NULL
nrow(df_4)



df_1$HourlyDryBulbTemperature <- 0.25028588*df_1$HourlyDryBulbTemperature + 0.250039633*df_2$HourlyDryBulbTemperature + 0.250022668*df_3$HourlyDryBulbTemperature + 0.249651819*df_4$HourlyDryBulbTemperature
df_1 <- na.omit(df_1)

set.seed(123)
smp_size <- floor(0.75 * nrow(df_1))
train_ind <- sample(seq_len(nrow(df_1)), size = smp_size)

train <- df_1[train_ind, ]
test <- df_1[-train_ind, ]


lmMod <- lm( Peak ~  month +  day +  Hour +  day: Hour +  HourlyDryBulbTemperature + I( HourlyDryBulbTemperature^2) + I( HourlyDryBulbTemperature^3) +  month: HourlyDryBulbTemperature +  month:I( HourlyDryBulbTemperature^2) +  month:I( HourlyDryBulbTemperature^3) +  Hour: HourlyDryBulbTemperature +  Hour:I( HourlyDryBulbTemperature^2) +  Hour:I( HourlyDryBulbTemperature^3), data = train )
summary(lmMod)

p_load <- predict(lmMod , test)

MAPE(p_load,test$Peak)


## ENSEMBLE COMBINATION

file_1 <- "E:\\NYISO\\Weather Data\\WEST_BUFFALO.CSV"
df_1<- read.csv(file_1, header = TRUE)
rownames(df_1) <- NULL
head(df_1)
df_1$month <- as.numeric(df_1$month)
df_1$day <- as.factor(df_1$day)
df_1$Hour <- as.factor(df_1$Hour)
df_1 <- df_1[c("month","day","Hour","HourlyDryBulbTemperature","Peak")]
df_1 <- subset(df_1, month > 5 & month < 10)
df_1$month <- as.factor(df_1$month)
nrow(df_1)


file_2 <- "E:\\NYISO\\Weather Data\\WEST_JAMESTOWN.CSV"
df_2<- read.csv(file_1, header = TRUE)
df_2 <- subset(df_2, month > 5 & month < 10)
rownames(df_2) <- NULL
nrow(df_2)


file_3 <- "E:\\NYISO\\Weather Data\\WEST_NIAGARA.CSV"
df_3<- read.csv(file_3, header = TRUE)
df_3 <- subset(df_3, month > 5 & month < 10)
rownames(df_3) <- NULL
nrow(df_3)


file_4 <- "E:\\NYISO\\Weather Data\\WEST_DUNKIRK.CSV"
df_4<- read.csv(file_4, header = TRUE)
df_4 <- subset(df_4, month > 5 & month < 10)
rownames(df_4) <- NULL
nrow(df_4)



df_1$HourlyDryBulbTemperature <- (df_1$HourlyDryBulbTemperature+df_2$HourlyDryBulbTemperature+df_3$HourlyDryBulbTemperature+df_4$HourlyDryBulbTemperature)/4

df_1 <- na.omit(df_1)

set.seed(123)
smp_size <- floor(0.75 * nrow(df_1))
train_ind <- sample(seq_len(nrow(df_1)), size = smp_size)

train <- df_1[train_ind, ]
test <- df_1[-train_ind, ]


lmMod <- lm( Peak ~  month +  day +  Hour +  day: Hour +  HourlyDryBulbTemperature + I( HourlyDryBulbTemperature^2) + I( HourlyDryBulbTemperature^3) +  month: HourlyDryBulbTemperature +  month:I( HourlyDryBulbTemperature^2) +  month:I( HourlyDryBulbTemperature^3) +  Hour: HourlyDryBulbTemperature +  Hour:I( HourlyDryBulbTemperature^2) +  Hour:I( HourlyDryBulbTemperature^3), data = train )
summary(lmMod)

p_load <- predict(lmMod , test)

MAPE(p_load,test$Peak)


