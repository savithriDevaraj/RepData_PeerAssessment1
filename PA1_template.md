# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data. This is my research.

```r
library(plyr)
setwd("C:/Users/Anna/RepData_PeerAssessment1")
data <- read.csv("activity.csv", na.strings=NA)
data1 <- na.omit(data)
```
## What is mean total number of steps taken per day?

```r
newdata <- ddply(data1, .(date), summarise, mSum=sum(steps))
with (newdata, hist(mSum, col="blue", main="Total Steps Taken", xlab="Steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
meanSteps <- mean(newdata$mSum)
median <- median(newdata$mSum)
print (meanSteps)
```

```
## [1] 10766.19
```

```r
print (median)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
newdata2 <- ddply(data1, .(interval), summarize, mMean=mean(steps))
with(newdata2, plot(interval, mMean, type="l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## Inputing missing values

```r
numRowsNA <- nrow(data) - nrow(data1)
print(numRowsNA)
```

```
## [1] 2304
```
#Remove dates from data and use the mMean in newdata2 for the corresponding interval if data has NA


```r
filleddata <- data.frame(interval=data$interval, steps=ifelse(is.na(data$steps),newdata2[match(newdata2$interval, data$interval), 2], data$steps),date=data$date)
```
##Plot the new summary data with NAs replaced by mean for the interval. The new mean and median are as below

```r
newSummaryData <- ddply(filleddata, .(date), summarise, mSum=sum(steps))
with(newSummaryData, hist(mSum, col="blue", main="Total Steps Taken", xlab="Steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
meanSteps <- mean(newSummaryData$mSum)
median <- median(newSummaryData$mSum)
```
## Are there differences in activity patterns between weekdays and weekends? Yes

```r
newSummary <- transform(newSummaryData, date=weekdays(as.Date(newSummaryData$date, "%m/%d/%Y")))
newS <- ddply(newSummary, .(date), summarise, mMean=mean(mSum))
with(newS, plot(as.factor(date), mMean, type="l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 
