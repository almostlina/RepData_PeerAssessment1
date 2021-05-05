---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document: 
    keep_md: true 
---


## Loading and preprocessing the data


```r
data <- read.csv("activity.csv")
data$date <- as.Date(data$date)
```


## What is mean total number of steps taken per day?

The mean total total number of steps taken per day is:

```r
stepsbyday <- tapply(data$steps, data$date, sum, na.rm=TRUE)
mean(stepsbyday, na.rm = TRUE)
```

```
## [1] 9354.23
```

The median total total number of steps taken per day is:

```r
median(stepsbyday, na.rm = TRUE)
```

```
## [1] 10395
```


```r
hist(stepsbyday, xlab = "Steps per Day" , main = "Total Number of Steps Taken Each Day", col = "wheat3")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)


## What is the average daily activity pattern?


```r
stepsbyinterval <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
plot(x = unique(data$interval), y = stepsbyinterval, type = "l", col = "wheat3")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

The 5-minute interval that contains the maximum number of steps and the value of steps in it are respectively: 

```r
stepsbyinterval[which.max(stepsbyinterval)]
```

```
##      835 
## 206.1698
```


## Imputing missing values

The total number of rows with NA is: 


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

The strategy I choose for filling in all of the missing values in the dataset is to use the mean for each missing value.


```r
library(Hmisc)
datafilled <- data
datafilled$steps <- impute(data$steps, fun=mean)
```

The new mean total total number of steps taken per day is:

```r
newstepsbyday <- tapply(datafilled$steps, datafilled$date, sum, na.rm=TRUE)
mean(newstepsbyday, na.rm = TRUE)
```

```
## [1] 10766.19
```

The new median total total number of steps taken per day is:

```r
median(newstepsbyday, na.rm = TRUE)
```

```
## [1] 10766.19
```


```r
hist(newstepsbyday, xlab = "Steps per Day" , main = "Total Number of Steps Taken Each Day (after imputing)", col = "wheat3")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

The mean increased after imputing the missing values, and the median became equal to the mean.

## Are there differences in activity patterns between weekdays and weekends?


```r
datafilled$day <- ifelse(weekdays(datafilled$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

##average over intervals and over weekends and weekdays separately 
activitypatterndata <- aggregate(steps ~ interval + day, data=datafilled, mean)

library(ggplot2)
ggplot(activitypatterndata, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5-minute intervals") + ylab("Average Number of Steps") + ggtitle("Activity Patterns on Weekdays and Weekends")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

We observe that the peak is higher on weekdays, yet the number of steps on weekends are generally higher. 
