#Reproducible Research: Peer Assessment 1
========================================================

##Loading and preprocessing the data


```r
activity <- read.csv("activity.csv")
data <- activity[which(activity$steps!= "NA"), ]
```

##What is mean total number of steps taken per day?

```r
library(plyr)
daily_steps <- ddply(data, .(date), summarise, steps=sum(steps))
hist(daily_steps$steps, xlab="steps taken per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
dev.copy(png, file="plot1.png",width=480,height=480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
mean(daily_steps$steps)
```

```
## [1] 10766
```

```r
median(daily_steps$steps)
```

```
## [1] 10765
```
## What is the average daily activity pattern?
##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
average_date <- ddply(data, .(interval), summarise, steps=mean(steps))
plot(average_date$interval, average_date$steps, type="l", xlab="5-minute interval", 
ylab="Average step",main="Average daily activity")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
dev.copy(png, file="plot2.png",width=480,height=480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

average_date[average_date$steps==max(average_date$steps),]
```

```
##     interval steps
## 104      835 206.2
```

```r
colnames(average_date)[2] <- "intervalAvg"
```
## Imputing missing values

```r
#  Calculate and report the total number of missing values in the dataset
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
# Imputing NA's with average on 5-min interval
merged <- arrange(join(activity, average_date), interval)
```

```
## Joining by: interval
```

```r
merged$steps[is.na(merged$steps)] <- merged$intervalAvg[is.na(merged$steps)]
# plot hist
new_daily_steps <- ddply(merged, .(date), summarise, steps=sum(steps))
hist(new_daily_steps$steps, main="Number of Steps", 
     xlab="steps taken each day",,)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
dev.copy(png, file="plot3.png",width=480,height=480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
# mean and median total number of steps taken per day don't change significantly
mean(new_daily_steps$steps)
```

```
## [1] 10766
```

```r
median(new_daily_steps$steps)
```

```
## [1] 10766
```
##Do these values differ from the estimates from the first part of the assignment?

```r
daily_steps_1 <- sum(data$steps)
daily_steps_2 <- sum(merged$steps)
diff <- daily_steps_2 -daily_steps_1 []
diff
```

```
## [1] 86130
```

## Are there differences in activity patterns between weekdays and weekends?

##Create a new factor variable in the dataset with two levels ¨C ¡°weekday¡± and ¡°weekend¡± indicating whether a given date is a weekday or weekend day

```r
library(lattice)
weekdays <- weekdays(as.Date(merged$date))
data_weekdays <- transform(merged, day=weekdays)
data_weekdays$wk <- ifelse(data_weekdays$day %in% c("Saturday", "Sunday"),"weekend", "weekday")
average_week <- ddply(data_weekdays, .(interval, wk), summarise, steps=mean(steps))

xyplot(steps ~ interval | wk, data = average_week, layout = c(1, 2), type="l")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
dev.copy(png, file="plot4.png",width=480,height=480)
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```
