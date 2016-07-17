# Reproducible Research: Peer Assessment 1


```r
library(ggplot2)
library(sqldf)
```


## Loading and preprocessing the data

    Load the data (i.e. read.csv())
    Process/transform the data (if necessary) into a format suitable for your analysis
    

```r
setwd("C:/Users/cam7de/Desktop/Coursera/07172016/RepData_PeerAssessment1")
getwd()
```

```
## [1] "C:/Users/cam7de/Desktop/Coursera/07172016/RepData_PeerAssessment1"
```

```r
unzip("activity.zip")
import <- read.csv("activity.csv")
import$date <- as.Date(import$date,format = "%Y-%m-%d")

str(import)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

    Calculate the total number of steps taken per day
    If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
    Calculate and report the mean and median of the total number of steps taken per day


```r
#x1_sum <- aggregate(steps~date,data=import,FUN=sum,na.rm=TRUE)
#problem is that this loses the days without data
x1_sum <- aggregate(import$steps,by=list(import$date),FUN=sum,na.rm=TRUE)
#this works but loses the names, so need to add them
names(x1_sum) <- c("date","steps")
qplot(x1_sum$steps,geom="histogram",binwidth=250)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
x1_mean <- mean(x1_sum$steps)
x1_med <- median(x1_sum$steps)
x1_mean
```

```
## [1] 9354.23
```

```r
x1_med
```

```
## [1] 10395
```

## What is the average daily activity pattern?

    Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
    Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
    

```r
x2_mean <- aggregate(import$steps,by=list(import$interval),FUN=mean,na.rm=TRUE)
names(x2_mean) = c("interval","mean_steps")
ggplot(x2_mean,aes(x=interval,y=mean_steps)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
x2_mean[x2_mean$mean_steps == max(x2_mean$mean_steps), ]
```

```
##     interval mean_steps
## 104      835   206.1698
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

    Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
    Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
    Create a new dataset that is equal to the original dataset but with the missing data filled in.
    Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
    

```r
count_na <- nrow(subset(import,is.na(steps)))
x3_median <- aggregate(import$steps,by=list(import$interval),FUN=median,na.rm=TRUE)
names(x3_median) = c("interval","median_steps")

import_withreplace <- sqldf('
select 
coalesce(b.median_steps,a.steps) as steps,
a.date,
a.interval
from import a
  left join x3_median b on a.interval = b.interval and a.steps is null
      ')
```

```
## Loading required package: tcltk
```

```r
x3_sum <- aggregate(import_withreplace$steps,by=list(import_withreplace$date),FUN=sum,na.rm=TRUE)
names(x3_sum) = c("date","steps")
qplot(x3_sum$steps,geom="histogram",binwidth=250)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
x3_mean <- mean(x3_sum$steps)
x3_med <- median(x3_sum$steps)
x3_mean
```

```
## [1] 9503.869
```

```r
x3_med
```

```
## [1] 10395
```

No difference in median observed and mild difference in mean with this fill method.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

    Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
    Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
import_withreplace$day_type <- 
  ifelse(weekdays(import_withreplace$date)=="Saturday"|weekdays(import_withreplace$date)=="Sunday",
         "WEEKEND","WEEKDAY")
x4 <- sqldf('
select
interval,
avg(case when day_type = "WEEKEND" then steps end) as mean_steps_weekend,
avg(case when day_type = "WEEKDAY" then steps end) as mean_steps_weekday
from import_withreplace
group by 1')
par(mfrow=c(1,2))
plot(x4$interval,x4$mean_steps_weekend,main="WEEKEND",type="l")
plot(x4$interval,x4$mean_steps_weekday,main="WEEKDAY",type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

