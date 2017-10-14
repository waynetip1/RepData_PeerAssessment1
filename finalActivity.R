library(dplyr)
library(tidyr)
library(data.table)
library(Hmisc)
setwd("C:/Users/Wayne Office Laptop/Documents/GitHub/personal-Activity-Monitoring---Peer-Graded-Assignment-")
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL,destfile = "./activity.zip")
unzip(zipfile ="./activity.zip")
pathdata <- "./"
list.files("./")
# read data
activityData <- fread("activity.csv",na.strings = c("NA"))


### 1. Transform the data by removing NAs
# data before transform
str(activityData)
actDataNoNa <- activityData %>%
        drop_na(steps)
#make date from character to a date type
actDataNoNa$date <- as.Date(actDataNoNa$date, "%Y-%m-%d")
str(actDataNoNa)


### 2. What is mean total number of steps taken per day?  ignore NAs
### A. Calculate the total number of steps taken per day
actDataStepsDay <- actDataNoNa %>%
        group_by(date)%>%
        summarise(totalSteps = sum(steps))
head(actDataStepsDay)
        
### B.Make a histogram of the total number of steps taken each day
hist(actDataStepsDay$totalSteps, col = 'Blue',xlab = "Number of Steps",
     main = "Histogram of Daily Steps Taken", ylim = c(0,40))

### C. Calculate and report the mean and median
###    of the total number of steps taken per day
rawMean <- mean(actDataStepsDay$totalSteps)
rawMedian <- median(actDataStepsDay$totalSteps)
raw <- cbind(rawMean,rawMedian)
raw

### 3. What is the average daily activity pattern?
### a. Make a time series plot of the 5 min intervals (x-axis), and average
###    number of steps across all days (y-axis)
actDataStepsInterval <- actDataNoNa %>%
        group_by(interval)%>%
        summarise(totalSteps = sum(steps))
#plot time series of the median steps for each day
plot(actDataStepsInterval$interval,actDataStepsInterval$totalSteps, type = "l",
     col="green", ylab = "Total Steps Taken", xlab = "Time Interval",
     lwd=2, ylim = c(0,12000), main=" Total Steps per 5 min Interval")
#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
maxInterval <- filter(actDataStepsInterval,totalSteps==max(totalSteps))
maxInterval

### 4. Imputing missing values
### A. Calculate and report the total number of missing values in the dataset
sum(is.na(activityData)==TRUE)
# percentage of missing data
mean(is.na(activityData$steps))
### B. Devise a strategy for filling in all of the missing values in the dataset.
### use Hmisc package to impute mean value of steps
### Create a new dataset that is equal to the 
###  original dataset but with missing data filled in
actDataImpute <- activityData
actDataImpute$imputedMeanSteps <- with(actDataImpute,impute(steps,mean))
actDataImpute$date <- as.Date(actDataImpute$date, "%Y-%m-%d")
head(actDataImpute)
### Make a histogram of the total number of steps taken each day
actDataStepsImpute<- actDataImpute %>%
        group_by(date)%>%
        summarise(totalSteps = sum(imputedMeanSteps))
head(actDataStepsImpute)
# Plot histogram
hist(actDataStepsImpute$totalSteps, col = 'Orange',xlab = "Number of Steps",
     main = "Histogram of Daily Steps Taken", ylim = c(0,40)) 
###  Calculate and report the mean and median
imputeMean <- mean(actDataStepsImpute$totalSteps)
imputeMedian <- median(actDataStepsImpute$totalSteps)
imputed <- cbind(imputeMean,imputeMedian)
imputed
### What is the impact of imputing missing data 
Differencemean <- rawMean - imputeMean
DifferenceMedian <- abs(rawMedian-imputeMedian)
Difference  <- cbind(Differencemean,DifferenceMedian)
raw_imputed <- as.data.frame(rbind(raw,imputed,Difference))

colnames(raw_imputed) <- c("Mean", 'Median')
rownames(raw_imputed) <- c("Raw Data","Imputed Data", "Difference")

raw_imputed

### Are there differences in activity patterns between weekdays and weekends?
### A. Create a new factor variable in the dataset with 
####  two levels"weekday" and "weekend" 
wdays<-c('Monday','Tuesday','Wednesday','Thursday','Friday')
imputedActivityDataStepsInt <- actDataImpute 
imputedActivityDataStepsInt$dayofweek <- 
        factor((weekdays(imputedActivityDataStepsInt$date)%in%wdays),
               levels = c(TRUE,FALSE), labels = c('Weekday','Weekend'))
head(imputedActivityDataStepsInt)

meanInterval <- aggregate(imputedMeanSteps ~ interval + dayofweek, 
                          imputedActivityDataStepsInt, FUN = mean)
head(meanInterval)

#create weekday and weekend step data views
stepsWD<-meanInterval %>%
        filter(dayofweek=='Weekday')

stepsWE <- meanInterval %>% 
        filter(dayofweek=='Weekend')
### Create time series plots for weekday and weekend activity. 
# plot weekdays
par(mfrow=c(2,1))
par(mar=c(2,2,1,1))
plot(stepsWD$interval,stepsWD$imputedMeanSteps, type = "l",
     lwd = 2, ylim = c(0,225), main = "Weekday Steps",
     ylab = "Number of Steps", xlab = "Interval", col = "purple")

#plot weekends
plot(stepsWE$interval,stepsWE$imputedMeanSteps, type = "l",
     lwd = 2, ylim = c(0,225), main = "Weekend Steps",
     ylab = "Number of Steps", xlab = "Interval", col = "dark green")




        