Loading and preprocessing the data
----------------------------------

### Load the data file "activity.csv" by using read.csv into Activity Monitoring Data(amd)

        amd <- read.csv("activity.csv", head=TRUE, na.strings="NA")
        str(amd)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

### Preprocess the data by converting the data to Year-Month-Day format and ignoring the NA values in steps

    amd$date <- as.Date(amd$date)
    amd_ignore <- subset(amd, !is.na(amd$steps))

What is mean total number of steps taken per day?
-------------------------------------------------

### Calculating the total number of steps taken per day and showing the histogram

    dailystep <- tapply(amd_ignore$steps, amd_ignore$date, sum, na.rm=TRUE)
    dailystep <- dailystep[!is.na(dailystep)]

    hist(x=dailystep,
         col="blue",
         breaks = 30,
         xlab="Total Daily Steps",
         ylab="Frequency",
         main="The total number of steps daily (without missinf")

![](PA1_template_files/figure-markdown_strict/steps%20total-1.png)
\#\#\#Calculate and report the mean and median of the total number of
steps taken per day

    mean(dailystep)

    ## [1] 10766.19

    median(dailystep)

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

### Time series plot of the 5-minutes interval and the average number of steps taken, averaged across all days

    avg_daily <- tapply(amd_ignore$steps, amd_ignore$interval, mean, na.rm=TRUE)
    dailyPattern <- data.frame(interval=as.integer(names(avg_daily)), avg=avg_daily)

    with(dailyPattern,
         plot(interval,
              avg,
              col="red",
              type="l",
              xlab="5-minute Intervals",
              ylab="Average Steps in the Interval Across All Days"))

![](PA1_template_files/figure-markdown_strict/plot%20avergae%20steps-1.png)
\#\#\#The 5-minutes interval that has the maximum number of steps on
average across all the days in the dataset.

    max_steps <- max(dailyPattern$avg)
    dailyPattern[dailyPattern$avg == max_steps, ]

    ##     interval      avg
    ## 835      835 206.1698

Imputing missing values
-----------------------

### The total number of missing values in the dataset

    na_number <- sum(is.na(amd$steps))
    na_number

    ## [1] 2304

### Devise a strategy for filling in all of the missing values in the dataset.

### Since the missing values is quite large, therefore we impute missing values based on average number of steps in particular 5-minutes interval.

### A new dataset that is equal to the original dataset but with the missing data filled in

    amd_missing <- amd
    amd_misssteps <- is.na(amd_missing$steps)
    avg_daily <- tapply(amd_ignore$steps, amd_ignore$interval, mean, na.rm=TRUE)
    amd_missing$steps[amd_misssteps] <- avg_daily[as.character(amd_missing$interval[amd_misssteps])]

### Histogram showing the total number of steps taken each day.

    new_dailystep <- tapply(amd_missing$steps, amd_missing$date, sum, na.rm=TRUE)

    hist(x=new_dailystep,
         col="blue",
         breaks = 30,
         xlab="Daily Steps",
         ylab="Frequency",
         main="The total number of steps daily (with missing values)")

![](PA1_template_files/figure-markdown_strict/total%20steps%20with%20missing-1.png)
\#\#\#The mean and median of the total number of steps taken per day

    mean(new_dailystep)

    ## [1] 10766.19

    median(new_dailystep)

    ## [1] 10766.19

### From the first part of the assignment which is total number of steps daily without missing values, the mean values is the same with the imputed data set and the median has a small change.

### One possible explanation is that when we fill the missing data for the intervals, we use means for intervals, so we have more data close or identical to the means, and median is shifted and becomes identical to the mean.

### The impact of imputing missing data on the estimates of the total daily number of steps is also clear: now we have higher frquency counts in the histogram at the center region (close to the mean).

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

### Create a new factor variable in the dataset with two levels-"weekday" and "weekend" indicating whether a given date is weekday or weekend day.

    # Create variable with date in correct format
    amd_missing$RealDate <- as.Date(amd_missing$date, format = "%Y-%m-%d")
    # create a variable with weekdays name
    amd_missing$weekday <- weekdays(amd_missing$RealDate)
    # create a new variable indicating weekday or weekend
    amd_missing$DayType <- ifelse(amd_missing$weekday=='Saturday' | amd_missing$weekday=='Sunday', 'weekend','weekday')
    # see first 10 values
    head(amd_missing, n=10)

    ##        steps       date interval   RealDate weekday DayType
    ## 1  1.7169811 2012-10-01        0 2012-10-01  Monday weekday
    ## 2  0.3396226 2012-10-01        5 2012-10-01  Monday weekday
    ## 3  0.1320755 2012-10-01       10 2012-10-01  Monday weekday
    ## 4  0.1509434 2012-10-01       15 2012-10-01  Monday weekday
    ## 5  0.0754717 2012-10-01       20 2012-10-01  Monday weekday
    ## 6  2.0943396 2012-10-01       25 2012-10-01  Monday weekday
    ## 7  0.5283019 2012-10-01       30 2012-10-01  Monday weekday
    ## 8  0.8679245 2012-10-01       35 2012-10-01  Monday weekday
    ## 9  0.0000000 2012-10-01       40 2012-10-01  Monday weekday
    ## 10 1.4716981 2012-10-01       45 2012-10-01  Monday weekday

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

    # create table with steps per time across weekdaydays or weekend days
    stepPerTime <- aggregate(steps~interval+DayType,data=amd_missing,FUN=mean,na.action=na.omit)

    library(lattice)
    xyplot(steps ~ interval | DayType,
           layout = c(1, 2),
           xlab="Interval",
           ylab="Number of steps",
           type="l",
           lty=1,
           data=stepPerTime)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)
