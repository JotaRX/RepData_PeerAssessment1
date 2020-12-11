---
title: "RepData PeerAssessment1"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = T)
```

## usage packages

```{r packages, warning =FALSE, results='hide'}
library(RColorBrewer)
library(dplyr)
library(ggplot2)
```

## Loading and preprocessing the data
### unzip the directory

It's necessary unzip the file and get the file, then we do a review of 
variables
```{r unzip&read}
if (!file.exists('activity.csv')) {
  unzip(zipfile = "repdata_data_activity.zip")
}
activity<-read.csv('activity.csv')
str(activity)
```


## What is mean total number of steps taken per day?

### Histogram of the total number of steps taken each day

```{r histogram}
sumday<-tapply(activity$steps,activity$date,sum)
hist(sumday,main = "Histogram Steps by day",xlab = "Number of steps",
     col = brewer.pal(5,"PRGn"))
```

### Mean and Median

```{r meanandmedian}
meanpday <-as.integer(mean(sumday,na.rm=T))
medianpday <-median(sumday,na.rm=T)
```

The individual take **`r meanpday`** of mean steps per day and his median per day was 
of **`r medianpday`** steps.

## Average daily activity pattern

### Time Series plot 5-minutes interval & averaged across all days

```{r lineplot}

mstep<-aggregate(activity$steps ~ activity$interval,FUN = "mean")
plot(mstep,type="l",col="#c299dc",main="Steps by Interval",
     xlab="interval", ylab="Mean Steps",lwd="2")
```
```{r max}
maxi<-max(mstep$`activity$steps`,na.rm = T)
maxI<-mstep[mstep$`activity$steps`==maxi,1]
```

The max of steps in a interval in mean is **`r maxi`** and the interval is the **`r maxI`**

## Missing Values

```{r lost}
MV<-is.na(activity$steps)
MissingValues<-sum(MV)
```
The number of the missing values in the dataset is **`r MissingValues`**


It's necessary complete the dataset, then we use mstep created above for this 
purpose

```{r complete dataset}
activityComplete<-cbind(activity,MV)
activityComplete$steps<-ifelse(activityComplete$MV==1,
       activityComplete$steps<-as.integer(mstep[mstep$`activity$interval` ==
                                             activityComplete$interval,2]),
       activity$steps)
activityComplete <- select(activityComplete,-"MV")
```

### Histogram of the total number of steps taken each day with the complete dataset

```{r histogramC}
sumdayC<-tapply(activityComplete$steps,activityComplete$date,sum)
hist(sumday,main = "Histogram Steps by day Complete Dataset",xlab = "Number of steps",
     col = brewer.pal(5,"PRGn"))
```

### Mean and Median

```{r meanandmedianC}
meanpdayC<- as.integer(mean(sumdayC,na.rm=T))
medianpdayC<- as.integer(median(sumdayC,na.rm=T))
```

The individual take **`r meanpdayC`** of mean steps per day and his median per day was 
of **`r medianpdayC`** steps.

The impact of imputing missing data in the dataset is a more conffidence DB, but
the change is so little because the differences are **`r meanpday - meanpdayC`**
in the mean and **`r medianpday - medianpdayC`** in the median.

## Weekdays vs Weekends

It's used the days of the week for separate between weekdays and weekends 
```{r Weeks}

activityComplete$day<-as.numeric(strftime(as.Date(activityComplete$date,"%Y-%m-%d"),"%u"))
activityComplete$day<-as.factor(ifelse(activityComplete$day<=5,
       "weekday",
       "Weekend"))
(table(activityComplete$day))        
```
```{r weekdays plot}


mstepW<-aggregate(steps ~ interval +
                        day,activityComplete,FUN = "mean")

ggplot(mstepW,aes(interval,steps, col=day)) + geom_line() +
        facet_grid(day ~ .) + labs(title = "Average steps by day type",
                                   y = "Number of Steps",
                                   x = "interval")

```




