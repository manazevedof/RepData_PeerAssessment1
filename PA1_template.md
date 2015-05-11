# Reproducible Research: Peer Assessment 1

```r
require(data.table)
```

```
## Loading required package: data.table
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:data.table':
## 
##     between, last
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```
## Loading and preprocessing the data


## What is mean total number of steps taken per day?

###1. Calculate the total number of steps taken per day:

```r
spd <- na.omit(data) %>% group_by(date) %>% summarise(n=sum(steps))
spd
```

```
## Source: local data table [53 x 2]
## 
##          date     n
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## ..        ...   ...
```

###2. Make a histogram of the total number of steps taken each day

```r
hist(spd$n,main="Steps taken per day",xlab="steps",col="darkgreen")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

###3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean_spd <- mean(spd$n,na.rm=TRUE)
median_spd <- median(spd$n,na.rm=TRUE)
options(scipen=1, digits=2)
```

The mean of the total number of steps taken per day is **10766.19**.

The median of the total number of steps taken per day is **10765**.

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
