---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data and libraries


```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
## ✔ readr   2.1.3      ✔ forcats 0.5.2 
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(patchwork)
library(chron)

activity <- readr::read_csv(unzip("activity.zip", "activity.csv"))
```

```
## Rows: 17568 Columns: 3
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl  (2): steps, interval
## date (1): date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
activity$hour   <- activity$interval %/% 100
activity$minute <- activity$interval %%  100
activity$DateTime <- strptime(paste(activity$date, activity$hour, activity$minute), format="%Y-%m-%d %H %M")
```


## What is mean total number of steps taken per day?

In a histogram is shown the number of steps per day. For this a new table 
"stepday" is generated. Within this the sum of steps per day is calculated.
The results are shown in figure.

```r
stepday <- activity %>% group_by(date) %>% summarise(sumsteps=sum(steps))

ggplot(stepday, aes(sumsteps))+
      geom_histogram(binwidth = 2500)+
      labs(title ="Steps taken per day")+
      labs(x= "steps per day", y= "count of days")
```

```
## Warning: Removed 8 rows containing non-finite values (`stat_bin()`).
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The mean and median per day are calculated by the following code and given 
result in code chunk.


```r
stepmean <- mean(stepday$sumsteps, na.rm=T)
stepmedian <- median(stepday$sumsteps, na.rm=T)

print("mean steps per day:") 
```

```
## [1] "mean steps per day:"
```

```r
stepmean
```

```
## [1] 10766.19
```

```r
print("median steps per day:") 
```

```
## [1] "median steps per day:"
```

```r
stepmedian
```

```
## [1] 10765
```


## What is the average daily activity pattern?

To answer this question the mean for each interval over the whole observation
per day was calculated. the results are shown in the graph. To 


```r
stepseries <- activity %>% group_by(interval) %>% summarise(meansteps=mean(steps, na.rm=T))


ggplot(stepseries)+
      geom_line(mapping=aes(interval, meansteps))+
      scale_x_continuous(breaks = c(0, 400, 800, 1200, 1600, 2000, 2400))+
      labs(title ="Steps taken per 5 minute interval")+
      labs(x= "time 0 to 24:00", y= "average of steps in this interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
stepseries %>% slice_max(meansteps)
```

```
## # A tibble: 1 × 2
##   interval meansteps
##      <dbl>     <dbl>
## 1      835      206.
```



## Imputing missing values


```r
## Data was copied and seperated into two files with missing and no missing values
activityimpute <- activity
activityempty <- activityimpute[is.na(activityimpute$steps),]
activityfull <- activityimpute[!is.na(activityimpute$steps),]

## Missing values were extracted from values mean per interval and were remerged

activityempty <- merge(x=activityempty, y= stepseries, by= "interval")
activityempty$steps <- round(activityempty$meansteps)
activityempty <- activityempty %>% select(-meansteps)
activityimpute <- rbind(activityempty, activityfull)
rm(activityempty, activityfull)

stepdayimpute <- activityimpute %>% group_by(date) %>% summarise(sumsteps=sum(steps))

## plots are generated and shown side by side

plot1 <- ggplot(stepday, aes(sumsteps))+
      geom_histogram(binwidth = 2500)+
      labs(title ="Steps taken per day")+
      labs(x= "steps per day", y= "count of days")

plot2 <- ggplot(stepdayimpute, aes(sumsteps))+
      geom_histogram(binwidth = 2500)+
      labs(title ="Steps taken per day")+
      labs(x= "steps per day", y= "count of days")

plot1 + plot2
```

```
## Warning: Removed 8 rows containing non-finite values (`stat_bin()`).
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
rm(plot1,plot2)

## mean and median are calculated and printed

stepmeanimpute <- mean(stepdayimpute$sumsteps, na.rm=T)
stepmedianimpute <- median(stepdayimpute$sumsteps, na.rm=T)

print("mean w/o impute")
```

```
## [1] "mean w/o impute"
```

```r
stepmean
```

```
## [1] 10766.19
```

```r
stepmeanimpute
```

```
## [1] 10765.64
```

```r
print("median w/o impute")
```

```
## [1] "median w/o impute"
```

```r
stepmedian
```

```
## [1] 10765
```

```r
stepmedianimpute
```

```
## [1] 10762
```

Mean and median an nearly not impacted by imputed values. Although graphs were showing siginicantly more steps as sum.


## Are there differences in activity patterns between weekdays and weekends?

Weekdays show more steps in the morning, while weekend days show more steps during the day.


```r
## new variable weekday introduced
activityimpute$weekday <- factor(!is.weekend(activityimpute$date), labels=c("weekend", "weekday"))

## group by interval and week day and finally the plot

stepweekday <- activityimpute %>% group_by(weekday, interval) %>% summarise(meansteps=mean(steps, na.rm=T))
```

```
## `summarise()` has grouped output by 'weekday'. You can override using the
## `.groups` argument.
```

```r
ggplot(stepweekday)+
      geom_line(mapping=aes(interval, meansteps))+
      scale_x_continuous(breaks = c(0, 400, 800, 1200, 1600, 2000, 2400))+
      facet_grid(rows =vars(weekday))+
      labs(title ="Steps taken per 5 minute interval")+
      labs(x= "time 0 to 24:00", y= "average of steps in this interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


