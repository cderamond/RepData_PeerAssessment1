# Reproducible Research: Peer Assessment 1
------------------------------------------


Author:  C. Deramond  
Course:  *repdata-008* 


## Loading and preprocessing the data

For this assignment, we initially load packages, data and make initial transformations:

```r
library(ggplot2, quietly = T, warn.conflicts = F)
library(lubridate, quietly = T, warn.conflicts = F)
library(dplyr, quietly = T, warn.conflicts = F)

data <- read.csv("activity.csv", stringsAsFactors=FALSE) #get the data
data$date <- ymd(data$date) # date parsing              
```


## What is mean total number of steps taken per day?

let's examine code to generate response first:


```r
#summary table
data_hist <- as.tbl(data) %>%
        filter(!is.na(steps)) %>%
        group_by(date) %>%
        summarise(n = n(), total = sum(steps), 
                  mean = mean(steps), 
                  median = median(as.numeric(steps))) 
```
1. **histogram:**

```r
hist <- ggplot(data_hist)
hist + aes(total)  + 
        geom_histogram(fill="steelblue") + 
        ggtitle("Histogram: total steps by day") + 
        labs(x = "Total steps")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

2. **summary table:** 

```
## Source: local data frame [53 x 5]
## 
##          date   n total       mean median
## 1  2012-10-02 288   126  0.4375000      0
## 2  2012-10-03 288 11352 39.4166667      0
## 3  2012-10-04 288 12116 42.0694444      0
## 4  2012-10-05 288 13294 46.1597222      0
## 5  2012-10-06 288 15420 53.5416667      0
## 6  2012-10-07 288 11015 38.2465278      0
## 7  2012-10-09 288 12811 44.4826389      0
## 8  2012-10-10 288  9900 34.3750000      0
## 9  2012-10-11 288 10304 35.7777778      0
## 10 2012-10-12 288 17382 60.3541667      0
## 11 2012-10-13 288 12426 43.1458333      0
## 12 2012-10-14 288 15098 52.4236111      0
## 13 2012-10-15 288 10139 35.2048611      0
## 14 2012-10-16 288 15084 52.3750000      0
## 15 2012-10-17 288 13452 46.7083333      0
## 16 2012-10-18 288 10056 34.9166667      0
## 17 2012-10-19 288 11829 41.0729167      0
## 18 2012-10-20 288 10395 36.0937500      0
## 19 2012-10-21 288  8821 30.6284722      0
## 20 2012-10-22 288 13460 46.7361111      0
## 21 2012-10-23 288  8918 30.9652778      0
## 22 2012-10-24 288  8355 29.0104167      0
## 23 2012-10-25 288  2492  8.6527778      0
## 24 2012-10-26 288  6778 23.5347222      0
## 25 2012-10-27 288 10119 35.1354167      0
## 26 2012-10-28 288 11458 39.7847222      0
## 27 2012-10-29 288  5018 17.4236111      0
## 28 2012-10-30 288  9819 34.0937500      0
## 29 2012-10-31 288 15414 53.5208333      0
## 30 2012-11-02 288 10600 36.8055556      0
## 31 2012-11-03 288 10571 36.7048611      0
## 32 2012-11-05 288 10439 36.2465278      0
## 33 2012-11-06 288  8334 28.9375000      0
## 34 2012-11-07 288 12883 44.7326389      0
## 35 2012-11-08 288  3219 11.1770833      0
## 36 2012-11-11 288 12608 43.7777778      0
## 37 2012-11-12 288 10765 37.3784722      0
## 38 2012-11-13 288  7336 25.4722222      0
## 39 2012-11-15 288    41  0.1423611      0
## 40 2012-11-16 288  5441 18.8923611      0
## 41 2012-11-17 288 14339 49.7881944      0
## 42 2012-11-18 288 15110 52.4652778      0
## 43 2012-11-19 288  8841 30.6979167      0
## 44 2012-11-20 288  4472 15.5277778      0
## 45 2012-11-21 288 12787 44.3993056      0
## 46 2012-11-22 288 20427 70.9270833      0
## 47 2012-11-23 288 21194 73.5902778      0
## 48 2012-11-24 288 14478 50.2708333      0
## 49 2012-11-25 288 11834 41.0902778      0
## 50 2012-11-26 288 11162 38.7569444      0
## 51 2012-11-27 288 13646 47.3819444      0
## 52 2012-11-28 288 10183 35.3576389      0
## 53 2012-11-29 288  7047 24.4687500      0
```




## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)




2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?




## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
