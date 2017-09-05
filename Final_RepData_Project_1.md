# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
First we will read in the data by making the path of the csv file an object.

```r
data <- "C:/Users/SteveC13/Desktop/Data Science Text Books/Reproducible Research/activity.csv"
```
Next we will define what class the columns are to call into the read.csv function.  Using the read.csv function we can call in the data from the csv as a usable data frame.

```r
class = c("integer", "character", "integer")
activity <- read.csv(data, head=TRUE, colClasses=class, na.strings="NA")
```
Lasly we will convert the dates to a proper format and also omit any rows that contain an NA from the data frame.

```r
activity$date <- as.Date(activity$date)

activity_omitNA <- na.omit(activity)
```

## What is mean total number of steps taken per day?


```r
library(ggplot2)
library(plyr)

Sumsteps <- aggregate(activity_omitNA$steps~activity_omitNA$date, FUN = sum)
colnames(Sumsteps)<- c("Day", "Number of Steps")
```
Plotting the frequency of steps taken per day:
First we want to see what the average number of steps taken per day.  We see that the number of average number of steps taken per day was aroun 10,000 to 15,000.


```r
rmNAhist<-hist(Sumsteps$`Number of Steps`, breaks = 5, xlab = "Total Steps", ylab = "Frequency", main = "Frequency of Total Steps Taken Per Day with NA Removed", col = "red",ylim = c(0,50) )
```

![](Final_RepData_Project_1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
rmNAhist
```

```
## $breaks
## [1]     0  5000 10000 15000 20000 25000
## 
## $counts
## [1]  5 12 28  6  2
## 
## $density
## [1] 1.886792e-05 4.528302e-05 1.056604e-04 2.264151e-05 7.547170e-06
## 
## $mids
## [1]  2500  7500 12500 17500 22500
## 
## $xname
## [1] "Sumsteps$`Number of Steps`"
## 
## $equidist
## [1] TRUE
## 
## attr(,"class")
## [1] "histogram"
```

The Mean and Median Steps Taken per Day:
We also see that the median number of steps taken per day was 10,765 steps and a mean of 10,766 steps per day.

```r
mean(Sumsteps$`Number of Steps`)
```

```
## [1] 10766.19
```

```r
median(Sumsteps$`Number of Steps`)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

When looking at the average daily activity pattern we use the aggregate function to look at the mean number of steps taken per interval on a given day.  With this new data frame we can plot the daily activity and see that the highest daily activity occurs between interval 500 and 1000.


```r
dailyact <- aggregate(activity_omitNA$steps ~ activity_omitNA$interval, activity_omitNA, mean)

p2 <- ggplot(dailyact, aes(x = dailyact$`activity_omitNA$interval`, y = dailyact$`activity_omitNA$steps`), xlab = "Interval", ylab="Average Number of Steps")
p2 + geom_line(linetype = "solid", size=0.75, color="blue")+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")
```

![](Final_RepData_Project_1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
## find interval with maximum steps

The daily activity plot gives us an idea of the intervals with the most steps, but with it was found that the interval with the highest activity was 835.


```r
Interval_Max <- dailyact[which.max(dailyact$`activity_omitNA$steps`),1]
Interval_Max
```

```
## [1] 835
```

## Imputing missing values
## 1.Counting number of intervals with no steps  ##

```r
Missingvals<-apply(activity, 1, function(x) sum(is.na(x)))
sum(Missingvals)
```

```
## [1] 2304
```
Next we subset the data that contains "NA" and replace "NA" with mean steps per day for each interval

```r
datawithNA<-subset(activity, is.na(activity$steps))

datawithNA[is.na(datawithNA)]<- dailyact$`activity_omitNA$steps`
```
Last we merge the original activity_omitNA object that had all "NA" removed with the newly subsetted data where the "NA" was substituted for the mean steps per day for each interval


```r
activity_impute <- rbind(datawithNA, activity_omitNA)
```
Lastly we wanted to explore what differences we see in our data if we impute the data by replacing the "NA" values as compared to simply removing all the "NA" values for steps.

First we aggregated the data so that we looked at the sum of all steps on a given day and this will show us the frequency of steps per interval on a given day as was done above when we looked at the data with "NA" removed.


```r
Sumsteps_impute <- aggregate(activity_impute$steps~activity_impute$date, FUN = sum)
colnames(Sumsteps_impute)<- c("Day", "Number of Steps")
```
Next we can see the histogram of the imputed data as well as the mean and median for number of steps per day


```r
imputehist <- hist(Sumsteps_impute$`Number of Steps`, breaks = 5, xlab = "Total Steps", ylab = "Frequency", main = "Frequency of Total Steps Taken Per Day with Imputed NA", col = "purple",ylim = c(0,50))
```

![](Final_RepData_Project_1_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
mean(Sumsteps_impute$`Number of Steps`)
```

```
## [1] 10766.19
```

```r
median(Sumsteps_impute$`Number of Steps`)
```

```
## [1] 10766.19
```

Lastly we will overlay these histograms to better understand what inherent variation we are adding to our data by removing the "NA" data from the data frame

```r
plot( imputehist, col = "Red", xlab = "Total Steps", ylab = "Frequency", main = "Frequency of Total Steps Taken Per Day: Imputed vs Removed NA", ylim = c(0,50))  # first histogram
plot( rmNAhist,col = "Blue", add=T)  # second
legend("topright", c("Imputed Data", "NA-Removed Data"), fill=c("Red", "Blue"))
```

![](Final_RepData_Project_1_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
Looking at this overlay histogram it shows that by removing the "NA" values we are losing some frequency of steps in the range of intervals between 10,000 and 15,000.  Overall the data fits well and the missing "NA" values may not make a large difference in the overall fit of the data.

## Are there differences in activity patterns between weekdays and weekends?
In the last evaluation we wanted to look into the data further and see if there are any differences in activity between the weekdays and weekends.
In order to do this we created a new column and utilized the weekdays function to classify each date as a day of the week.  After this we can then classify each day as either a day of the weekend or weekday.  With this new data frame with the day classification we can plot two lines plots for weekday data and weekend data.
When we plot these two variables we see that the majority of the weekday steps are around intervals 800-900, then activity slows down for the remainder for the day.  When compared to the weekend we still see a spike in activity around interval 850-1000, but activity on the weekends differes from that on the weekday because intervals after 1000 continue to spike until interval 2000.

```r
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
```

![](Final_RepData_Project_1_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


Thank you for reading my report.
