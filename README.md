# Reproducible-Research-Project-1
Reproducible Research: Project_1
Mansour_haghi
2022-12-31
Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.
required packages
# Loading packages
library(ggplot2)
library(ggthemes)
library(scales)
library(lubridate)
Loading and preprocessing the data
Downloading and unzipping data.

path = getwd()
unzip("repdata_data_activity.zip", exdir = path)
Reading csv and summary.

activity <- read.csv("activity.csv")

activity$date <- as.POSIXct(activity$date, "%Y%m%d")

day <- weekdays(activity$date)

activity <- cbind(activity, day)

summary(activity)
##      steps             date               interval          day           
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Length:17568      
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Class :character  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Mode  :character  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5                     
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2                     
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0                     
##  NA's   :2304
What is mean total number of steps taken per day?
Calculating total steps taken on a day _ Changing col _Converting the data set into a data frame to be able to use ggplot2 names_Plotting a histogram using ggplot2
Creating a Histogram of the total number of steps taken each day.
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


The mean and median of steps taken per day
mean(activity_T_Steps$Steps, na.rm = T)
## [1] 9354.23
median(activity_T_Steps$Steps, na.rm = T)
## [1] 10395
What is the average daily activity pattern?
Calculating the average steps taken for each 5-minute interval. ## Time Series plot

av_Daily_Activity <- aggregate(activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE)
Changing col names and Converting the data set into a dataframe.

names(av_Daily_Activity) <- c("Interval", "Mean")

av_Activity_d <- data.frame(av_Daily_Activity)
we have the valid date-time format we can have a cleaner looking time-series plot, for the average 24-hour period.

plot2 <- ggplot(av_Activity_d, mapping = aes(Interval, Mean)) + 
  geom_line(col = "green") +
  xlab("Interval") + 
  ylab("Average Number of Steps") + 
  ggtitle("Average Number of Steps Per Interval") +
  theme_dark(base_family = "serif")
  
print(plot2)


Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
av_Daily_Activity[which.max(av_Daily_Activity$Mean), ]$Interval
## [1] 835
Imputing missing data
The total amount of NA’s and the percentage of missing step data.

sum(is.na(activity$steps))
## [1] 2304
Matching the mean of daily activity with the missing values

Imputed_Steps <-av_Daily_Activity$Mean[match(activity$interval, av_Daily_Activity$Interval)]
Transforming steps in activity if they were missing values with the filled values from above. Forming the new dataset with the imputed missing values. Changing col names

activity_Imputed <- transform(activity, 
                             steps = ifelse(is.na(activity$steps), yes = Imputed_Steps, no = activity$steps))


totalactivity_Imputed <- aggregate(steps ~ date, activity_Imputed, sum)


names(totalactivity_Imputed) <- c("date", "dailySteps")

sum(is.na(totalactivity_Imputed$dailySteps))
## [1] 0
Histogram of the total number of steps taken each day after missing values were imputed
Converting the data set into a data frame to be able to use ggplot2
Plotting a histogram using ggplot2
totalImputed_Stepsdf <- data.frame(totalactivity_Imputed)

p <- ggplot(totalImputed_Stepsdf, aes(x = dailySteps)) + 
  geom_histogram(breaks = seq(0, 25000, by = 2500), fill = "#f383ff", col = "black") + 
  ylim(0, 30) + 
  xlab("Total Steps Taken Per Day") + 
  ylab("Frequency") + 
  ggtitle("Total Number of Steps Taken on a Day") + 
  theme_classic(base_family = "serif")

print(p)
 The mean of the total number of steps taken per day is:

mean(totalactivity_Imputed$dailySteps)
## [1] 10766.19
The median of the total number of steps taken per day is:

median(totalactivity_Imputed$dailySteps)
## [1] 10766.19
Are there differences in activity patterns between weekdays and weekends?
Panel plot comparing the average number of steps taken per 5-minute intervals for Weekdays and Weekends
Coverting ‘interval’ column data to a vailid date-time format.

activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))

activity$dayType <- sapply(activity$date, function(x) {
  if(weekdays(x) == "شنبه" | weekdays(x) == "یکشنبه")
  {AA <- "Weekend"}
  else {AA <- "Weekday"}
  AA
})
Creating the data set that will be plotted and Plotting using ggplot2
Now that we have the valid date time format we can have a cleaner looking Time-Seriesplot, for comparison of the average 24-hour period on weekdays and weekends.

activity_inDay <-  aggregate(steps ~ interval + dayType, activity, mean, na.rm = TRUE)


plot3 <-  ggplot(activity_inDay, aes(x = interval , y = steps, color = dayType)) + 
  geom_line() + ggtitle("average daily steps by say type") + 
  xlab("interval") + 
  ylab("average number of Steps") +
  facet_wrap(~dayType, ncol = 1, nrow=2) +
  scale_color_discrete(name = "day type") +
  theme_economist(base_family = "serif")

print(plot3) 
