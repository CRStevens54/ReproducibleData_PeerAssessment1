## project week 2##

# 1. write up report in markdown and knitr

#check into github repository
#push repository to github
#submit link to github repository
#submit SHA-1 Commit Hash

# ============================================= #

# Code for Analyzing Data #

data <- "C:/Users/SteveC13/Desktop/Data Science Text Books/Reproducible Research/activity.csv"
class = c("integer", "character", "integer")
activity <- read.csv(data, head=TRUE, colClasses=class, na.strings="NA")

activity$date <- as.Date(activity$date)

activity_omitNA <- na.omit(activity)


## what is the mean total number of steps taken per day? ##
library(ggplot2)
library(plyr)

Sumsteps <- aggregate(activity_omitNA$steps~activity_omitNA$date, FUN = sum)
colnames(Sumsteps)<- c("Day", "Number of Steps")

rmNAhist<-hist(Sumsteps$`Number of Steps`, breaks = 5, xlab = "Total Steps", ylab = "Frequency", main = "Frequency of Total Steps Taken Per Day with NA Removed", col = "red",ylim = c(0,50) )
mean(Sumsteps$`Number of Steps`)
median(Sumsteps$`Number of Steps`)

## What is the average daily activity pattern? ##

dailyact <- aggregate(activity_omitNA$steps ~ activity_omitNA$interval, activity_omitNA, mean)

p2 <- ggplot(dailyact, aes(x = dailyact$`activity_omitNA$interval`, y = dailyact$`activity_omitNA$steps`), xlab = "Interval", ylab="Average Number of Steps")
p2 + geom_line(linetype = "solid", size=0.75, color="blue")+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")

## find interval with maximum steps ##

Interval_Max <- dailyact[which.max(dailyact$`activity_omitNA$steps`),1]
Interval_Max

## Counting number of intervals with no steps  ##

Missingvals<-apply(activity, 1, function(x) sum(is.na(x)))
sum(Missingvals)

## subset data with NA and replace NA with mean steps per day for each interval ##

datawithNA<-subset(activity, is.na(activity$steps))

datawithNA[is.na(datawithNA)]<- dailyact$`activity_omitNA$steps`

## Merge Activity_omitNA with datawithNA  ##
activity_impute <- rbind(datawithNA, activity_omitNA)

## Histogram of Total Steps each Day ##
Sumsteps_impute <- aggregate(activity_impute$steps~activity_impute$date, FUN = sum)
colnames(Sumsteps_impute)<- c("Day", "Number of Steps")

imputehist <- hist(Sumsteps_impute$`Number of Steps`, breaks = 5, xlab = "Total Steps", ylab = "Frequency", main = "Frequency of Total Steps Taken Per Day with Imputed NA", col = "purple",ylim = c(0,50))
imputehist
mean(Sumsteps_impute$`Number of Steps`)
median(Sumsteps_impute$`Number of Steps`)


plot( imputehist, col = "Red", xlab = "Total Steps", ylab = "Frequency", main = "Frequency of Total Steps Taken Per Day: Imputed vs Removed NA", ylim = c(0,50))  # first histogram
plot( rmNAhist,col = "Blue", add=T)  # second
legend("topright", c("Imputed Data", "NA-Removed Data"), fill=c("Red", "Blue"))


## Are there difference in activity patterns between weekdays and weekends? ##

days <- weekdays(activity_omitNA[,2])
activity_omitNA <- cbind(activity_omitNA,days)
activity_omitNA$days <-
revalue(activity_omitNA$days, c("Monday"="weekday","Tuesday"="weekday","Wednesday"="weekday","Thursday"="weekday","Friday"="weekday"))
activity_omitNA$days <- revalue(activity_omitNA$days, c("Saturday"="weekend","Sunday"="weekend"))
library(reshape2)
library(lattice)
activity_meanbyinterv <- tapply(activity_omitNA$steps, list(activity_omitNA$interval, activity_omitNA$days), mean)
activity_meanbyinterv <- melt(activity_meanbyinterv)

colnames(activity_meanbyinterv) <- c("interval", "day", "steps")
xyplot(activity_meanbyinterv$steps ~ activity_meanbyinterv$interval | activity_meanbyinterv$day, layout=c(1,2), type="l", 
       main="Time Series Plot of Average of Total Steps", 
       xlab = "Time intervals (minutes)", 
       ylab="Average of Total Steps")

                                

