---
title: "Reproducible Research: Peer Assessment 1"
author: "Delphine"
date: "14/07/2023"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data
First, we unzip data in order to obtain a csv file that we can work on and we save it into a dataframe named activity


```r
library("data.table")
library(ggplot2)

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")

activity <- as.data.table(read.csv(file = "data/activity.csv", header = TRUE))
```



## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day


```r
Total_Week_Steps <- activity[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 
```



2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

A histogram represents the frequency distribution of continuous variables. Conversely, a bar graph is a diagrammatic comparison of discrete variables. Histogram presents numerical data whereas bar graph shows categorical data.


```r
ggplot(Total_Week_Steps, aes(x = steps)) +
    geom_histogram(fill = "grey", binwidth = 1000) +
    labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](Recherche-monitoring-data_files/figure-html/unnamed-chunk-3-1.png)<!-- -->




3.Calculate and report the mean and median of the total number of steps taken per day

```r
Total_Week_Steps[, .(Mean_Steps = mean(steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]
```

```
##    Mean_Steps Median_Steps
## 1:   10766.19        10765
```



## What is the average daily activity pattern?


1. Make a time series plot (i.e. type = "l"type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
Interval <- activity[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 

plot(Interval, type = "l")
```

![](Recherche-monitoring-data_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
Interval[steps == max(steps), .(max_interval = interval)]
```

```
##    max_interval
## 1:          835
```



## Imputing missing values
1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NANAs)



```r
nrow(activity[is.na(steps),])
```

```
## [1] 2304
```
2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
To be able to have a new dataset equal to the original without the missing data, I will replace the NA by the median value of the dataset.


```r
activity[is.na(steps), "steps"] <- activity[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
```

4. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data.table::fwrite(x = activity, file = "data/Activity_without_NA.csv", quote = FALSE)
```




5. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# total number of steps taken per day
Total_Week_Steps <- activity[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 

# mean and median total number of steps taken per day
Total_Week_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]
```

```
##    Mean_Steps Median_Steps
## 1:    9354.23        10395
```

```r
ggplot(Total_Week_Steps, aes(x = steps)) + 
  geom_histogram(fill = "blue", binwidth = 1000) + 
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

![](Recherche-monitoring-data_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

Sorry my computer is in french so the week day are in French
Lundi means Monday, Mardi means Tuesday, Mercredi means Wenesday ... 


```r
activity[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activity[, `Day of Week`:= weekdays(x = date)]
activity[grepl(pattern = "lundi|mardi|mercredi|jeudi|vendredi", x = `Day of Week`), "weekday or weekend"] <- "weekday"
activity[grepl(pattern = "samedi|dimanche", x = `Day of Week`), "weekday or weekend"] <- "weekend"
activity[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(activity, 10)
```

```
##     steps       date interval Day of Week weekday or weekend
##  1:     0 2012-10-01        0       lundi            weekday
##  2:     0 2012-10-01        5       lundi            weekday
##  3:     0 2012-10-01       10       lundi            weekday
##  4:     0 2012-10-01       15       lundi            weekday
##  5:     0 2012-10-01       20       lundi            weekday
##  6:     0 2012-10-01       25       lundi            weekday
##  7:     0 2012-10-01       30       lundi            weekday
##  8:     0 2012-10-01       35       lundi            weekday
##  9:     0 2012-10-01       40       lundi            weekday
## 10:     0 2012-10-01       45       lundi            weekday
```

2. Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
activity[is.na(steps), "steps"] <- activity[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
Interval <- activity[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 

ggplot(Interval , aes(x = interval , y = steps, color=`weekday or weekend`)) + 
  geom_line() + 
  labs(title = "Avgerage Daily Steps by Weektype", x = "Interval", y = "No. of Steps") +
  facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
```

![](Recherche-monitoring-data_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
