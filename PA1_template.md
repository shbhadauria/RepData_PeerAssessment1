---
title: "Reproducible research-Peer assessment1"
output: 
 html_document:
  keep_md: True
---


#1. Code for data processing

```r
data=read.csv("C:/Users/shbhadauria/Desktop/coursera/reproducible research/activity.csv")
data$date=as.Date(data$date)
total_no_steps=aggregate(data$steps, by=list(date=data$date), FUN=sum)
```
#2 Histogram of steps

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.4
```

```
## Warning: Installed Rcpp (0.12.18) different from Rcpp used to build dplyr (0.12.11).
## Please reinstall dplyr to avoid random crashes or undefined behavior.
```

```r
plot1<-ggplot(total_no_steps, aes(x=total_no_steps$date,y=total_no_steps$x))+geom_histogram(stat="Identity")+xlab("Day")+ylab("Total no of steps in a day")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

```r
plot1
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](figure/Histogram-1.png)<!-- -->

#3. Mean And Median

```r
me=mean(total_no_steps$x,na.rm=TRUE)
med=median(total_no_steps$x,na.rm=TRUE)
me
```

```
## [1] 10766.19
```

```r
med
```

```
## [1] 10765
```

#4. Time series plot

```r
#time series plot
library(TTR)
```

```
## Warning: package 'TTR' was built under R version 3.4.4
```

```r
data_ts=na.omit(data)
avg_no_steps=SMA(data_ts$steps,n=1)
data_ts$steps=avg_no_steps
plot2=ggplot(data_ts, aes(interval, steps)) + geom_line() + xlab("Interval") + ylab("Average daily steps")
plot2
```

```
## Warning: Removed 1 rows containing missing values (geom_path).
```

![](figure/Time series plot-1.png)<!-- -->

#5. Interval with maximum steps

```r
intv=data_ts[which.max(data_ts$steps), ]
print(paste0("Interval with maximum steps is ",intv$interval))
```

```
## [1] "Interval with maximum steps is 615"
```

#6. Code for creating new dataset with imputed values for missing values

```r
#missing value imputation
library(Hmisc)
```

```
## Warning: package 'Hmisc' was built under R version 3.4.4
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Warning: package 'Formula' was built under R version 3.4.4
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```

```r
steps_new=impute(data$steps, 0)  # replace with 0
data_new=data
data_new$steps=steps_new
```

#7. Histogram with imputed values

```r
#histogram for imputed data
total_no_steps_new=aggregate(data_new$steps, by=list(date=data_new$date), FUN=sum)
plot3=ggplot(total_no_steps_new, aes(x=total_no_steps_new$date,y=total_no_steps_new$x))+geom_histogram(stat="Identity")+xlab("Day")+ylab("Total no of steps in a day")
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

```r
plot3
```

![](figure/Histogram with imputed values-1.png)<!-- -->

```r
#mean and median comparison
me_new=mean(total_no_steps_new$x,na.rm=TRUE)
med_new=median(total_no_steps_new$x,na.rm=TRUE)
me_new
```

```
## [1] 9354.23
```

```r
med_new
```

```
## [1] 10395
```

```r
if(me_new>me){
  print("Mean with imputed values is greater")
}else {
  print("Mean with non imputed values is greater")
}
```

```
## [1] "Mean with non imputed values is greater"
```

```r
if(med_new>med){
  print("Median with imputed values is greater")
}else {
  print("Median with non imputed values is greater")
}
```

```
## [1] "Median with non imputed values is greater"
```

#8. Panel plot

```r
#Panel Plot
wkdys=weekdays(data_new$date)
data_new$day=wkdys
for(i in 1:length(data_new$day)){
  if(data_new$day[i]=="Saturday"|data_new$day[i]=="Sunday"){
    data_new$daytype[i]="Weekend"
    data_new$Daytype[i]=1
    
    }
  else {
    data_new$daytype[i]="Weekday"
    data_new$Daytype[i]=0
    }
}
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:Hmisc':
## 
##     src, summarize
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
grp_data=group_by(data_new,Daytype)
avg_no_steps=SMA(grp_data$steps,n=1)
grp_data$steps=avg_no_steps

group=unique(grp_data$daytype)

plot4=xyplot(grp_data$steps ~ grp_data$interval| group, data = grp_data,type="l",xlab="5 minute interval",ylab = "Steps on daily basis")
plot4
```

![](figure/Panel Plot-1.png)<!-- -->
