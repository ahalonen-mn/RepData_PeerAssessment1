---
title: "Course 5 Week 2 Assignment"
author: "Halonen, Ally"
date: "10/19/2020"
output: github_document
---



## Part 1
First, clean the dataset and calculate the total number of steps taken per day.


```r
df <- read.csv('activity.csv')
df$date <- as.Date(df$date, format = "%Y-%m-%d")
df <- df %>%
  rename(date_col = date)
df_with_na <- df
df <- na.omit(df)

df_days <- df %>%
  group_by(date_col) %>%
  summarise(steps = sum(steps))

head(df_days)
```

```
## # A tibble: 6 x 2
##   date_col   steps
##   <date>     <int>
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```


Then, make a histogram of the total number of steps taken each day


```r
hist(df_days$steps, xlab = 'Number of Steps Per Day', main = "Number of Steps")
```

![](PA1_template1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Calculate the mean and median. 

```r
mean_steps <- mean(df_days$steps)
median_steps <- median(df_days$steps)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps
```

```
## [1] 10765
```

## Part 2

Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
df_avg <- df %>%
  group_by(interval) %>%
  summarise(steps = mean(steps))

plot(df_avg$interval, df_avg$steps, type = "l", ylab = "5 Minute Interval", xlab = "Average Steps", main = "Average Number of Steps")
```

![](PA1_template1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
df_avg$interval[df_avg$steps == max(df_avg$steps)]
```

```
## [1] 835
```


## Part 3

Calculate and report the total number of missing values in the dataset


```r
sum(is.na(df_with_na))
```

```
## [1] 2304
```

Imput missing values using the mean for the interval

```r
avg_interval_df <-  df_with_na %>%
    group_by(interval) %>%
    summarise(steps = mean(steps, na.rm = TRUE)) %>%
    rename(avg_steps = steps)

df_with_na <- merge(df_with_na,avg_interval_df,by="interval")

df_with_na$steps[is.na(df_with_na$steps) == TRUE] <- df_with_na$avg_steps[is.na(df_with_na$steps) == TRUE]
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps


```r
df_days_na <- df_with_na %>%
  group_by(date_col) %>%
  summarise(steps = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(df_days_na$steps, xlab = 'Number of Steps Per Day', main = "Number of Steps")
```

![](PA1_template1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
mean_steps_na <- mean(df_days_na$steps)
median_steps_na <- median(df_days_na$steps)
mean_steps_na
```

```
## [1] 10766.19
```

```r
median_steps_na
```

```
## [1] 10766.19
```


## Part 4
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
#weekdays(df_with_na$date_col)

for (i in 1:nrow(df_with_na)) {
  if(weekdays(df_with_na$date_col[i]) == "Saturday" || weekdays(df_with_na$date_col[i]) == "Sunday") {
    df_with_na$weekday[i] <- "Weekend"
  } else {
    df_with_na$weekday[i] <- "Weekday"
  }
}

df_with_na_wk<- aggregate(steps ~ interval + weekday, data = df_with_na, mean)
```

Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
xyplot(steps ~ interval | weekday, df_with_na_wk, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template1_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

