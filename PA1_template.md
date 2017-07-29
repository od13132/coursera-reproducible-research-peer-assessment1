---
title: "PA1_Template"
output:
  html_document: defaults
---

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Data used for this analysis was taken from the following link: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables incuded in this dataset are:

* steps: Number of steps taking in a 5-minute interval 

* date: The date on which the measurement was taken in YYYY-MM-DD format

* interval: Identifier for the 5-minute interval in which measurement was taken

#Loading and Preprocessing the data

```{r}
library(ggplot2)
library(dplyr)
```

1. Load the data

```{r}
setwd("C:/Users/Osian/Desktop/Data_Science/Projects/Reproducible Research/Week 2")
raw_df <- read.csv("activity.csv")
```

2. Transform the data into a format suitable for analysis.

```{r}
df <- raw_df
df$date <- as.Date(raw_df$date)
```

##What is the mean of the total number of steps taken per day?

1. Calculate the total number of steps taken per day
```{r}
sumByDay <- aggregate(df$steps, by = list(df$date), sum, na.rm = TRUE)
names(sumByDay)[1] = "date"
names(sumByDay)[2] = "steps"
head(sumByDay)
```

2. Make a histogram of the total number of steps taken each day

```{r, fig.height=6, fig.width=8}
hist(sumByDay$steps, breaks = 20, col = "light blue", main = "Total Daily Steps")
```

3. Calculate and report the mean and median of the total number of steps taken per day

```{r}
meanSteps <- mean(sumByDay$steps)
meanSteps
```

```{r}
median(sumByDay$steps)
```

#What is the average daily activity pattern?

1. Make a time series plot of the 5 minute interval (x-axis) and the average number of steps taken, averages across all days (y-axis).

```{r}
meanInterval <- aggregate(df$steps, by = list(df$interval), mean, na.rm = TRUE)
names(meanInterval)[1] = "interval"
names(meanInterval)[2] = "steps"
head(meanInterval)
```
```{r, fig.height=6, fig.width=6}
plot(meanInterval, type = "l", col = "red", main = "Mean Number of Steps in an Interval")
```

2. Which 5-minute interval, on average across all the days, contains the maximum number of steps?

```{r}
meanInterval[which.max(meanInterval$steps),]
```

#Imputing missing values

1. Calculate and report the total number of missing values in the dataset.

```{r}
sum(is.na(df))
```

2. Devise a strategy for filling in all of the missing values in the dataset.

The plan is to replace all with the daily mean divided by the number of intervals within the day (288). 

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
new_df <- df
new_df[is.na(df$steps), ]$steps <- meanSteps/288
head(new_df)
```

4. Make a histogram of the total number of steps and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
new_sumByDay <- aggregate(new_df$steps, by=list(new_df$date), sum)
names(new_sumByDay)[1] = "date"
names(new_sumByDay)[2] = "steps"
head(new_sumByDay)
```

```{r, fig.height=6, fig.width=8}
hist(new_sumByDay$steps, breaks = 20, col = "light blue", main = "Revised Total Daily Steps")
```

```{r}
mean(new_sumByDay$steps)
```

```{r}
median(new_sumByDay$steps)
```

By comparing the previous results with the new results, we can see that the median remains the same while the new mean is greater than the previous mean.  

Hence we can see that the method of imputing missing values increases the estimates of the data.

#Are there any differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r}
new_df$weekday <- weekdays(new_df$date)
new_df$weekday <- ifelse(new_df$weekday == "Saturday" | new_df$weekday == "Sunday", "Weekend", "Weekday")
mean_new_df <- aggregate(new_df$steps, by=list(new_df$weekday, new_df$interval), mean)
names(mean_new_df)[1] = "WeekdayOrWeekend"
names(mean_new_df)[2] = "interval"
names(mean_new_df)[3] = "steps"
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r, fig.height=6, fig.width=13}
ggplot(mean_new_df, aes(x = interval, y = steps, color = WeekdayOrWeekend)) +
geom_line() +
facet_grid(WeekdayOrWeekend ~ .) +
labs(title = "Mean of Steps by Interval (Weekday vs Weekend)", x = "Interval (per 5 minutes)", y = "Steps")
```

From the above graph we can see that the activity is higher in the mornings on a weekday than on the weekend, however after 10:00am we can see that the activity is generally higher on weekends.

