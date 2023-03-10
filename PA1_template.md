---
title: 'Reproducible Research: Project_1'
author: "Mansour_haghi"
date: "2022-12-31"
output:
  pdf_document: default
  html_document: default
keep_md: yes
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, fig.width = 10, fig.height = 5,
                      fig.keep = 'all' ,fig.path = 'figures\ ', dev = 'png')
```

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a [Fitbit](https://www.fitbit.com/home), [Nike Fuelband](https://www.nike.com/us/en_us/c/nikeplus-fuelband), or [Jawbone Up](https://jawbone.com/up). These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:


The variables included in this dataset are:

-   **steps**: Number of steps taking in a 5-minute interval (missing values are coded as *NA*) </br>
-   **date**: The date on which the measurement was taken in YYYY-MM-DD format </br>
-   **interval**: Identifier for the 5-minute interval in which measurement was taken </br> The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## required packages

```{r, message=FALSE}
# Loading packages
library(ggplot2)
library(ggthemes)
library(scales)
library(lubridate)

```

## Loading and preprocessing the data

Downloading and unzipping data.

```{r}

path = getwd()
unzip("repdata_data_activity.zip", exdir = path)



```

Reading csv and summary.

```{r}
activity <- read.csv("activity.csv")

activity$date <- as.POSIXct(activity$date, "%Y%m%d")

day <- weekdays(activity$date)

activity <- cbind(activity, day)

summary(activity)
```

## What is mean total number of steps taken per day?

## Calculating total steps taken on a day _  Changing col _Converting the data set into a data frame to be able to use ggplot2 names_Plotting a histogram using ggplot2
### Creating a Histogram of the total number of steps taken each day.

```{r}
activity_T_Steps <- with(activity, aggregate(steps, by = list(date), sum, na.rm = TRUE))
names(activity_T_Steps) <- c("Date", "Steps")

total_Steps_d <- data.frame(activity_T_Steps)

plot1 <- ggplot(total_Steps_d, aes(x = Steps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "#f383ff", col = "red") + 
  ylim(0, 30) + 
  xlab("Total Steps Taken Per Day") + 
  ylab("Frequency") + 
  ggtitle("Total Number of Steps Taken on a Day") + 
  theme_classic(base_family = "serif")

print(plot1)
```

## The mean and median of steps taken per day

```{r}
mean(activity_T_Steps$Steps, na.rm = T)
median(activity_T_Steps$Steps, na.rm = T)
```
## What is the average daily activity pattern?
Calculating the average steps taken for each 5-minute interval.
## Time Series plot
```{r}
av_Daily_Activity <- aggregate(activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE)

```

Changing col names and Converting the data set into a dataframe.

```{r}
names(av_Daily_Activity) <- c("Interval", "Mean")

av_Activity_d <- data.frame(av_Daily_Activity)
```

we have the valid date-time format we can have a cleaner looking time-series plot, for the average 24-hour period.

```{r}
plot2 <- ggplot(av_Activity_d, mapping = aes(Interval, Mean)) + 
  geom_line(col = "green") +
  xlab("Interval") + 
  ylab("Average Number of Steps") + 
  ggtitle("Average Number of Steps Per Interval") +
  theme_dark(base_family = "serif")
  
print(plot2)
```

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```{r}
av_Daily_Activity[which.max(av_Daily_Activity$Mean), ]$Interval

```

## Imputing missing data

The total amount of NA's and the percentage of missing step data.

```{r}
sum(is.na(activity$steps))
```

 Matching the mean of daily activity with the missing values


```{r}
Imputed_Steps <-av_Daily_Activity$Mean[match(activity$interval, av_Daily_Activity$Interval)]
```

Transforming steps in activity if they were missing values with the filled values from above.
Forming the new dataset with the imputed missing values.
Changing col names
```{r}

activity_Imputed <- transform(activity, 
                             steps = ifelse(is.na(activity$steps), yes = Imputed_Steps, no = activity$steps))


totalactivity_Imputed <- aggregate(steps ~ date, activity_Imputed, sum)


names(totalactivity_Imputed) <- c("date", "dailySteps")

sum(is.na(totalactivity_Imputed$dailySteps))
```

## Histogram of the total number of steps taken each day after missing values were imputed
## Converting the data set into a data frame to be able to use ggplot2
## Plotting a histogram using ggplot2


```{r}
totalImputed_Stepsdf <- data.frame(totalactivity_Imputed)

p <- ggplot(totalImputed_Stepsdf, aes(x = dailySteps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "#f383ff", col = "black") + 
  ylim(0, 30) + 
  xlab("Total Steps Taken Per Day") + 
  ylab("Frequency") + 
  ggtitle("Total Number of Steps Taken on a Day") + 
  theme_classic(base_family = "serif")

print(p)
```
The mean of the total number of steps taken per day is:

```{r}
mean(totalactivity_Imputed$dailySteps)
```

The median of the total number of steps taken per day is:
```{r}
median(totalactivity_Imputed$dailySteps)
```

## Are there differences in activity patterns between weekdays and weekends?
## Panel plot comparing the average number of steps taken per 5-minute intervals for Weekdays and Weekends

Coverting 'interval' column data to a vailid date-time format.

```{r}
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))

activity$dayType <- sapply(activity$date, function(x) {
  if(weekdays(x) == "????" | weekdays(x) == "??????")
  {AA <- "Weekend"}
  else {AA <- "Weekday"}
  AA
})
```

## Creating the data set that will be plotted and Plotting using ggplot2
Now that we have the valid date time format we can have a cleaner looking Time-Seriesplot, for comparison of the average 24-hour period on weekdays and weekends.

```{r}

activity_inDay <-  aggregate(steps ~ interval + dayType, activity, mean, na.rm = TRUE)


plot3 <-  ggplot(activity_inDay, aes(x = interval , y = steps, color = dayType)) + 
  geom_line() + ggtitle("average daily steps by say type") + 
  xlab("interval") + 
  ylab("average number of Steps") +
  facet_wrap(~dayType, ncol = 1, nrow=2) +
  scale_color_discrete(name = "day type") +
  theme_economist(base_family = "serif")

print(plot3) 
```
