---
title: "Course Project 1"
author: "Ibrohim Safarov"
date: "18 May 2020"
output: html_document
---

```{r}
# Set working directory
# Download file
setwd("E:/R")
if(!file.exists('activity.csv')){
        temp <- tempfile()
        fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        download.file(fileUrl, temp, method = "curl")
        unzip(temp, 'activity.csv')
        unlink(temp)
}
activity <- read.csv("activity.csv", header=T, sep =",")
str(activity)
activity$date <- as.Date(activity$date, format="%Y-%m-%d")
```
## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day

```{r}
StepsPerDay <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE); dim(StepsPerDay)
head(StepsPerDay,3)
```
2. Make a histogram of the total number of steps taken each day

```{r}
library(ggplot2)
Hist <- ggplot(data = na.omit(StepsPerDay), aes(x = steps)) + 
        geom_histogram(fill = "green", binwidth = 1000) +
        xlab("Total number of steps Daily") +
        ylab("Frequency") +
        ggtitle("Histogram of the Total Number of Steps Taken Each Day")
print(Hist)
```
3. Calculate and report the mean and median of the total number of steps taken per day

```{r}
stepsDailyMean <- mean(StepsPerDay$steps, na.rm = TRUE)
print(stepsDailyMean)
stepsDailyMedian <- median(StepsPerDay$steps, na.rm = TRUE)
print(stepsDailyMedian)
```
## What is the average daily activity pattern?
1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across 
```{r}
average <- aggregate(steps ~ interval, activity, mean, na.rm = TRUE)
library(ggplot2)
timePlot <- ggplot(data = average, aes(x = interval, y = steps)) +
        geom_line(color = "purple") +
        xlab("5-minute interval") +
        ylab("Average steps") +
        ggtitle("Average steps in 5-minute interval")
print(timePlot)
```
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
names(average)[1] = "Intervals"
names(average)[2] = "Average_steps"
head(average, 15)
intervalMax <- average[which.max(average$Average_steps),]
intervalMax
```
## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
activityNAs <- is.na(activity$steps)
totalNas <- sum(activityNAs)
totalNas
```
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
missingValues <- is.na(activity)
table(missingValues)
# Replace missing data with mean of steps
# Use the Hmisc package to impute mean i.e. install.packages("Hmisc"). Then load the package.
library(Hmisc)
imputing <- activity
imputing$steps <- impute(activity$steps, mean)
sum(is.na(imputing$steps))
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r}
sumImputing <- aggregate(steps ~ date, imputing, sum)
names(sumImputing)[1] = "date"
names(sumImputing)[2] = "Imputedsteps"
head(sumImputing, 20)
library(ggplot2)
Hist2 <- ggplot(data = sumImputing, aes(x = Imputedsteps)) +
        geom_histogram(fill = "red", binwidth = 1000) +
        xlab("Total number of steps each day") +
        ylab("Frequency") +
        ggtitle("Histogram of the Total Number of Steps Taken Each Day")
print(Hist2)
# Mean of sumIputing
mean(sumImputing$Imputedsteps)
# Median of sumIputing
median(sumImputing$Imputedsteps)
```
## Are there differences in activity patterns between weekdays and weekends?

## For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r}
imputing$dating <- ifelse(as.POSIXlt(imputing$date)$wday %in% c(0,6), "Weekend", "Weekday")
head(imputing)
```

2. Make a panel plot containing a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r}
meanImputing <- aggregate(steps ~ interval + dating, imputing, mean)
head(meanImputing)
# Use ggplot2
library(ggplot2)
panelPlot <- ggplot(data = meanImputing, aes(x = interval, y = steps)) +
        geom_line(color = "gold") +
        facet_grid(dating ~ .) +
        xlab("5-minute interval") +
        ylab("Average steps") +
        ggtitle("Average steps in 5-minute interval")
print(panelPlot)
```
