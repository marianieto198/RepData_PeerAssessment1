---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
##Load all the packages needed


```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```
## Loading and preprocessing the data

###1.Loading and reading the data


```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
Data1 <- read.csv('activity.csv', as.is = TRUE)
```

###2.Process the data into a format suitable for analysis (omit NAs)


```r
Data <- na.omit(Data1)
```

## What is mean total number of steps taken per day?

###1.Calculate the total number of steps taken per day


```r
daily <- group_by(Data, date)
daily_steps <- summarise(daily, total = sum(steps))
daily_steps
```

```
## # A tibble: 53 x 2
##    date       total
##  * <chr>      <int>
##  1 2012-10-02   126
##  2 2012-10-03 11352
##  3 2012-10-04 12116
##  4 2012-10-05 13294
##  5 2012-10-06 15420
##  6 2012-10-07 11015
##  7 2012-10-09 12811
##  8 2012-10-10  9900
##  9 2012-10-11 10304
## 10 2012-10-12 17382
## # … with 43 more rows
```

###2.Make a histogram of the total number of steps taken each day


```r
hist(daily_steps$total, main="Histogram of total number of steps per day", 
     xlab="Number of steps taken per day", col="blue")
```

![](PA1_template_files/figure-html/figure 1-1.png)<!-- -->

###3.Calculate the mean and median of the total number of steps taken per day


```r
meansteps <- mean(daily_steps$total)
mediansteps <- median(daily_steps$total)

summary(daily_steps)
```

```
##      date               total      
##  Length:53          Min.   :   41  
##  Class :character   1st Qu.: 8841  
##  Mode  :character   Median :10765  
##                     Mean   :10766  
##                     3rd Qu.:13294  
##                     Max.   :21194
```
* Mean: 1.0766189\times 10^{4}
* Median:  10765

## What is the average daily activity pattern?

###1. Make a time series plot 


```r
steps_interval <- aggregate(steps ~ interval, Data, mean)

plot(steps_interval$interval, steps_interval$steps, type='l', xlab="Intervals", 
     ylab="Average number of steps",  main="Average number of steps 
     across all days", col="red")
```

![](PA1_template_files/figure-html/figure 2-1.png)<!-- -->
  
###2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maximum <- which.max(steps_interval$steps)
maximum1<- steps_interval[maximum, ]$interval
maximum2<- steps_interval[maximum, ]$steps
```

*On average, the maximum number of steps in a 5-minute interval is:206.1698113
*The time interval that contains the maximum number of steps is: 835

## Imputing missing values

###1.Calculate and report the total number of missing values in the dataset 

```r
missing<-sum(is.na(Data1))
```
* Number of missing values: 2304

###2.Devise a strategy for filling in all of the missing values in the dataset. 
####Replacing NA’s with the mean of the 5-minute interval


```r
for (i in 1:nrow(Data1)) {
    if(is.na(Data1$steps[i])) {
      mis <- steps_interval$steps[which(steps_interval$interval == Data1$interval[i])]
        Data1$steps[i] <- mis
    }
}
```

###3.Create a new dataset that is equal to the original dataset.


```r
steps_new <- aggregate(steps ~ date, Data1, sum)
```

###4.Make a histogram of the total number of steps taken each day.


```r
hist(steps_new$steps, xlab="Total number of steps", 
main="Histogram of total number of steps per day", col="steelblue")
```

![](PA1_template_files/figure-html/figure 3-1.png)<!-- -->

```r
new_mean<-mean(steps_new$steps)
new_median<-median(steps_new$steps)
```

*Median of the imputed data:1.0766189\times 10^{4}
*Median of the data without NAs:10765
*Mean of the imputed data: 1.0766189\times 10^{4}
*Mean of the data without NAs: 1.0766189\times 10^{4}

## Are there differences in activity patterns between weekdays and weekends?

###1. Create a new factor variable in the dataset with two levels 

```r
Data1['day'] <- weekdays(as.Date(Data1$date))
Data1$day[Data1$day  %in% c('Saturday','Sunday') ] <- "Weekend"
Data1$day[Data1$day != "Weekend"] <- "Weekday"

Data1$day <- as.factor(Data1$day)
stepsnew2 <- aggregate(steps ~ interval+day, Data1, mean)
```

###2.Make a panel plot containing a time series plot 

```r
finalplot <- ggplot(stepsnew2, aes(interval, steps)) +
  geom_line(stat = "identity", aes(colour = day)) +  labs(x="Intervals", y=expression("Total number of Steps")) + ggtitle("Total of steps per interval by 
day of the week") + facet_grid(day ~ .)
finalplot
```

![](PA1_template_files/figure-html/figure 4-1.png)<!-- -->
