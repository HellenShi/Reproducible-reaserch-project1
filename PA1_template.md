---
title: "Course Project 1"
author: "Hellen"
date: "10/27/2019"
output:
  html_document: 
    keep_md: yes
  pdf_document: default
  word_document: default
---



##Set a directory

```r
setwd("/Users/Apple/data/cousera5week2project")
```
##Question1
Code for reading in the dataset and/or processing the data

```r
data_activity<-read.csv("activity.csv")
```
##Question2
Histogram of the total number of steps taken each day
1.Remove missing value

```r
data_activity_rm<-subset(data_activity,data_activity$steps!="NA")
```
2.Calculate the total number of steps taken per day

```r
data_activity_sum<-tapply(data_activity_rm$steps,data_activity_rm$date,sum)
```
3.Reshape the dataset

```r
data_activity_sum<-as.data.frame(data_activity_sum)
```
4.Histogram of the total number of steps taken each day

```r
plot(data_activity_sum,type="h",xlab="day",ylab="steps",col="dark red",lwd=5)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
##Question3
Mean and median number of steps taken each day

```r
data_activity_mean<-tapply(data_activity_rm$steps,data_activity_rm$date,mean)
plot(data_activity_mean,type = "h",xlab="day",ylab="mean steps",col="dark red",lwd=5)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
##Question4
Time series plot of the average number of steps taken

```r
data_activity_mean1<-as.data.frame(data_activity_mean)
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.5.2
```

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

```r
data_activity_mean1$y<-rownames(data_activity_mean1)
data_activity_average<-merge(data_activity_mean1,data_activity,by.x="y",by.y="date")
colnames(data_activity_average)<-c("datetime","mean","step","interval")
data_activity_average$datetime<-as.Date(data_activity_average$datetime)
plot(data_activity_average$datetime,data_activity_average$mean,type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
##Question5
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
data_activity_max<-tapply(data_activity$steps,data_activity$date,max)
data_activity_max<-as.data.frame(data_activity_max)
data_activity_max$datetime<-rownames(data_activity_max)
```
##Question6
Code to describe and show a strategy for imputing missing data
1.Calculate and report the total number of missing values in the dataset

```r
sum(!complete.cases(data_activity))
```

```
## [1] 2304
```
2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.

```r
library(reshape2)
install.packages("Hmisc")
```

```
## 
##   There is a binary version available but the source version is
##   later:
##       binary source needs_compilation
## Hmisc  4.2-0  4.3-0              TRUE
```

```
## installing the source package 'Hmisc'
```

```
## Warning in install.packages("Hmisc"): installation of package 'Hmisc' had
## non-zero exit status
```

```r
library(Hmisc)
```

```
## Warning: package 'Hmisc' was built under R version 3.5.2
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Warning: package 'survival' was built under R version 3.5.2
```

```
## Loading required package: Formula
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.5.2
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     src, summarize
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```

```r
data_activity_rs<-dcast(data_activity,interval~date,value.var="steps")
a<-colnames(data_activity_rs)[2:61]
for (i in a) {impute(data_activity_rs$i,mean)}
```

##Question7
Histogram of the total number of steps taken each day after missing values are imputed

```r
library(reshape2)
data_activity1<-melt(data_activity_rs,id="interval",variable.name = "date",value.name ="steps")
data_activity_total<-tapply(data_activity1$steps, data_activity$date, sum)
data_activity_total<-as.data.frame(data_activity_total)
data_activity_total$time<-rownames(data_activity_total)
ggplot(data = data_activity_total,aes(time,data_activity_total))+geom_bar(stat = "identity",width = 0.3,colour="red",size=0.25)
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

##Question8
Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
1.Create a new factor variable in the dataset with two levels weekday and weekend indicating whether a given date is a weekday or weekend day.
2.Reshape the original data,creat a new one that can compare the same interval in different day

```r
data_activity_inter<-dcast(data_activity,date~interval,value.var="steps")
```
3.Add a new col which indicate whether the date is a weekend

```r
data_activity_inter$week<-weekdays(as.Date(data_activity_inter$date))
week<-data_activity_inter$week=="Saturday"|data_activity_inter$week=="Sunday"
week[which(week=="FALSE")]<-"weekdays"
week[which(week!="weekdays")]<-"weekends"
data_activity_inter$week<-week
```
4.Calulate the average of per 5-minute interval across weekdays and weekends
#remove the "NA" values

```r
data_activity_week<-data_activity_inter[complete.cases(data_activity_inter),]
```
5.Calulate the average of per 5-minute interval

```r
data_activity_week<-aggregate(data_activity_week[,2:289],list(data_activity_week[,290]),mean)
```
6.make a panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
use melt function and dcast function to reshape the data

```r
colnames(data_activity_week)[1]<-"week"
panel_data<-melt(data_activity_week,id<-"week",variable.name = "interval",value.name = "mean_steps")
panel_data1<-dcast(panel_data,interval~week,value.var="mean_steps")
```
make a plot

```r
par(mfrow=c(1,2),mar=c(4,4,2,1))
plot(x=panel_data1$interval,y=panel_data1$weekdays,xlab="interval",ylab="weekdays")
plot(x=panel_data1$interval,y=panel_data1$weekends,xlab="interval",ylab="weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


