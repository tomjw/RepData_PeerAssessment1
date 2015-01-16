---
title: Reproducible Research- Peer Assessment 1
output: html_document
---


Analysis
------------------------------------

The necessary packages are loaded and he data is read into a dataframe called act:

```{r}
library(ggplot2)
library(plyr)
act <- read.csv('/home/tom/Dropbox/Coursera_Repo_research/activity.csv',
        header = TRUE)
```

The date variable is converted to a Date type.  A column corresponding to the length of the day in minutes is added:

```{r}
act$date <- as.Date(act$date)
minutes <- seq(0,1435,5)
act$minutes <- minutes
```
The amount of NA values are determined and then omitted in new dataframe 'actnoNA'


``` {r}
sapply(act, function(x) sum(is.na(x)))
actnoNA <- na.omit(act) 
```

A histogram is the made of the total number of steps taken on each day for this new dataframe

```{r} 
ggplot(data=actnoNA, aes(date,steps)) + 
              geom_bar(stat="identity") +
              xlab('Day') + ylab('Steps')
```


The mean and median values for each day  of the week are then calculated:

```{r} 
meanactnoNA <- aggregate(actnoNA, by = list(c(weekdays(actnoNA$date))), FUN = 'mean')
meanactnoNA$steps <- round(meanactnoNA$steps * 288)
print(meanactnoNA[,1:2])
medianactnoNA <- aggregate(actnoNA, by = list(c(weekdays(actnoNA$date))), FUN = 'median')
medianactnoNA$steps <- round(medianactnoNA$steps * 288)
print(medianactnoNA[,1:2])
```



The plyr package is used to determine the  number of steps taken for each interval, averaged over the dataset. The resulting time series
is then plotted.  The x-axis shows the length of the day in minutes.

```{r}
plyractnoNA <- ddply(actnoNA, .(interval), summarise,
                  N    = length(steps),
                  mean = round(mean(steps)),
                  median   = median(steps))
plyractnoNA$minutes <- minutes
plot(plyractnoNA$minutes,plyractnoNA$mean, type = 'l',  xlab = 'Time of Day (minutes)', ylab = 'Steps')
```

The maximum number of steps occurs in the interval from 8:35-8:40am.  On average 206 steps are taken in this period.
 
As seen earlier there are 2304 NA values for the steps variable.  The above analysis omits these observations.  Now the NA values will be replaced with the mean value for the appropriate interval:

```{r}
act$mean <-plyractnoNA$mean
ind <-which(is.na(act$steps)) 
act$steps[ind] <- plyractnoNA$mean
```

A histogram is created from the revised dataframe:

```{r} 
ggplot(data=act, aes(date,steps)) + 
              geom_bar(stat="identity") +
              xlab('Day') + ylab('Steps')
```

The mean and median step values for each weekday is calculated:

```{r} 
meanact <- aggregate(act, by = list(c(weekdays(act$date))), FUN = 'mean')
meanact$steps <- round(meanact$steps * 288)
print(meanact[,1:2])
medianact <- aggregate(act, by = list(c(weekdays(act$date))), FUN = 'median')
medianact$steps <- round(medianact$steps * 288)
print(medianact[,1:2])
```

From the histogram and mean/median results it can be seen that the omission of the NAs did not make a large difference to the results.  This is because the for the grouping results their were at least 7 samples for each day and the averaging of these observations reduced the effect of the missing values.


The effect of weekdays vs weekend days is now considered.  A weekday/weekend factor is created in the dataframe:

```{r} 
act$wkwkend <- "weekday"
ind2 <-which(weekdays(act$date) %in% c("Sunday", "Saturday")) 
act$wkwkend[ind2] <- "weekend"
act$wkwkend <- as.factor(act$wkwkend)
```

A plot of the average steps per interval for the two cases is created:

```{r} 
plyract <- ddply(act, .(interval,wkwkend), summarise,
                     N    = length(steps),
                     mean = round(mean(steps)),
                     median   = median(steps))
plyract$minutes <- rep(seq(0,1435,5), each=2)
```



A plot of the average steps per interval for the two cases is created:


```{r}
ggplot(plyract, aes(minutes,mean)) + geom_line(size = 0.5) +facet_grid(wkwkend ~.) +
              theme(legend.position="none")
```              

The weekend case shows the steps are more equally distributed throughout the day, with activity slightly later in the morning and continuing on to later in the day.
