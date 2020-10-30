---
title: "Course 5 Week 2 Assignment"
author: "Halonen, Ally"
date: "10/19/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(dplyr)
library(lattice)
```

## Part 1
First, clean the dataset and calculate the total number of steps taken per day.

```{r, echo=TRUE, warning=FALSE, message=FALSE}
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


Then, make a histogram of the total number of steps taken each day

```{r, echo=TRUE, warning=FALSE}
hist(df_days$steps, xlab = 'Number of Steps Per Day', main = "Number of Steps")
```

Calculate the mean and median. 
```{r, echo=TRUE, warning=FALSE, message=FALSE}
mean_steps <- mean(df_days$steps)
median_steps <- median(df_days$steps)
mean_steps
median_steps
```

## Part 2

Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```{r, echo=TRUE, warning=FALSE, message=FALSE}
df_avg <- df %>%
  group_by(interval) %>%
  summarise(steps = mean(steps))

plot(df_avg$interval, df_avg$steps, type = "l", ylab = "5 Minute Interval", xlab = "Average Steps", main = "Average Number of Steps")
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r, echo=TRUE, warning=FALSE, message=FALSE}

df_avg$interval[df_avg$steps == max(df_avg$steps)]
```


## Part 3

Calculate and report the total number of missing values in the dataset

```{r, echo=TRUE, warning=FALSE, message=FALSE}
sum(is.na(df_with_na))
```

Imput missing values using the mean for the interval
```{r, echo=TRUE, warning=FALSE, message=FALSE}
avg_interval_df <-  df_with_na %>%
    group_by(interval) %>%
    summarise(steps = mean(steps, na.rm = TRUE)) %>%
    rename(avg_steps = steps)

df_with_na <- merge(df_with_na,avg_interval_df,by="interval")

df_with_na$steps[is.na(df_with_na$steps) == TRUE] <- df_with_na$avg_steps[is.na(df_with_na$steps) == TRUE]
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps

```{r, echo=TRUE, warning=FALSE}
df_days_na <- df_with_na %>%
  group_by(date_col) %>%
  summarise(steps = sum(steps))

hist(df_days_na$steps, xlab = 'Number of Steps Per Day', main = "Number of Steps")
mean_steps_na <- mean(df_days_na$steps)
median_steps_na <- median(df_days_na$steps)
mean_steps_na
median_steps_na

```


## Part 4
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r, echo=TRUE, warning=FALSE}

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
```{r, echo=TRUE, warning=FALSE}

xyplot(steps ~ interval | weekday, df_with_na_wk, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
```

