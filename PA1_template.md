# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
setwd("~/Desktop/RepData_PeerAssessment1/")
unzip("activity.zip")
my_data <- read.csv("activity.csv", sep=",")

dates <- as.Date(my_data$date, format = "%Y-%m-%d")
my_data$date <- dates
```


## What is mean total number of steps taken per day?

```r
sum_by_date<- aggregate(steps ~ date, data=my_data, FUN=sum)
breaks <- sum_by_date$date
breaks<-strptime(sum_by_date$date, format = "%Y-%m-%d")

# 1 - Make a histogram of the total number of steps taken each day
hist(sum_by_date$steps, xlab="Sum of daily steps", ylab="Frequence", col="red", breaks=20)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
# 2 - Calculate and report the mean and median total number of steps taken per day
median_1 <- median(sum_by_date$steps)
mean_1 <- mean(sum_by_date$steps)
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
```
The **median**  is 10765.0 and the **mean** is 10766.2


## What is the average daily activity pattern?

```r
# 1 - Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
average_steps_by_interval <- aggregate(steps ~ interval, my_data, FUN=mean)
plot(average_steps_by_interval, type="l", xlab ="Interval", ylab="Average of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# 2 - Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
index <- which.max(average_steps_by_interval$steps)
average_steps_by_interval$interval[index]
```

```
## [1] 835
```


## Imputing missing values

```r
# 1 - Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

n_mis <- nrow(my_data[is.na(my_data),])
n_mis2 <- sum(!complete.cases(my_data))
n_mis
```

```
## [1] 2304
```

```r
# 2 - Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

not_na_my_data <- my_data[!is.na(my_data),]
na_my_data <- my_data[!is.na(my_data),]

merged_data <- merge(na_my_data[,c("interval", "date")], average_steps_by_interval, by = "interval")

# 3 - Create a new dataset that is equal to the original dataset but with the missing data filled in.
v_columns <- c("steps", "date", "interval");
new_data <- rbind(not_na_my_data[,v_columns], merged_data[, v_columns])

# 4 - Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

sum_by_date2<- aggregate(steps ~ date, data=new_data, FUN=sum)

hist(sum_by_date2$steps, xlab="Sum of daily steps", ylab="Frequence", col="red", breaks=20)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
median_2 <- median(sum_by_date2$steps)
mean_2 <- mean(sum_by_date2$steps)
```
The **median**  is 21531.2 and the **mean** is 21532.4

## Are there differences in activity patterns between weekdays and weekends?


```r
new_data$weekdaytype = as.factor(weekdays(new_data$date))
levels(new_data$weekdaytype) = c("weekday", "weekday", "weekday", "weekday", "weekday", "weekend", "weekend")

avs_weekday <- aggregate(steps ~ interval , new_data[new_data$weekdaytype=="weekday",], FUN=mean)
avs_weekend <- aggregate(steps ~ interval , new_data[new_data$weekdaytype=="weekday",], FUN=mean)
#av_weekday <- avs_weekday[avs_by_interval_and_type$weekdaytype=="weekday",]
#av_weekend <- avs_weekday[avs_by_interval_and_type$weekdaytype=="weekend",]
plot(avs_weekday[,c("interval", "steps")], type="l", xlab ="Interval", ylab="Average of steps")
plot(avs_weekend[,c("interval", "steps")], type="l", xlab ="Interval", ylab="Average of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 
