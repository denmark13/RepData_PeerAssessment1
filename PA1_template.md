---
title: "RepData_week2"
author: "Denmark Lora"
date: "2/22/2022"
output: html_document
---

```{r ingress, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(readxl)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(plotly)
library(shiny)
library(kableExtra)
library(lubridate)
library(xlsx)
library(DT)
library(readxl)

activity <- read_csv("C:/Users/lorad/Downloads/repdata_data_activity/activity.csv", 
    col_types = cols(steps = col_number(), 
        date = col_date(format = "%Y-%m-%d"), 
        interval = col_number()))

```

## What is mean total number of steps taken per day?

```{r Q1, echo=TRUE}

library(ggplot2)

df<-activity %>% 
  drop_na() %>% #remove all NA values
  filter(steps != "0") %>%  #exclude the days where there is no step recorded
  group_by(date) %>% 
  summarise(sum=sum(steps)) %>% 
  distinct(date, .keep_all=TRUE)


#plot histogram
hist(df$sum, col="red", main="Histogram of the total number of steps per day", 
     xlab="Total number of steps per day", border = "black")

options(scipen=999) #disable scientific notation
mean1 <- mean(df$sum)
median1 <- median(df$sum)


```
The mean is `r mean1` or 10766 and the median is `r median1`.

## What is the average daily activity pattern?

```{r Q2, echo=TRUE}

df<-activity %>% 
  drop_na()
  


dff <- aggregate(df,
                by = list(df$interval),
                FUN = mean)

plot(dff$interval, dff$steps, type='l', col=1, 
     main="Average number of steps averaged across all days", xlab="Interval", 
     ylab="Average number of steps")



# find row id of maximum average number of steps in an interval
max_row_id <- which.max(dff$steps)

# get the interval with maximum average number of steps in an interval
dff [max_row_id, ]
```

The interval 835 has the maximum average number of steps 206.2.

```{r Q3, echo=TRUE}

# get rows with NA's
data_NA <- activity[!complete.cases(activity),]

# number of rows
NArows <- nrow(data_NA)


data_imputed <- activity
for (i in 1:nrow(data_imputed)) {
  if (is.na(data_imputed$steps[i])) {
    interval_value <- data_imputed$interval[i]
    steps_value <- dff[
      dff$interval == interval_value,]
    data_imputed$steps[i] <- steps_value$steps
  }
}

df_imputed_steps_by_day <- aggregate(steps ~ date, data_imputed, sum)
head(df_imputed_steps_by_day)

hist(df_imputed_steps_by_day$steps, main="Histogram of total number of steps per day (imputed)",col="red",
     xlab="Total number of steps in a day")


mean(df_imputed_steps_by_day$steps)

median(df_imputed_steps_by_day$steps)

mean(df$sum)

median(df$sum)

```
The number of rows with NAs is `r NArows`

```{r Q4, echo=TRUE}

data_imputed['type_of_day'] <- weekdays(as.Date(data_imputed$date))
data_imputed$type_of_day[data_imputed$type_of_day  %in% c('Saturday','Sunday') ] <- "weekend"
data_imputed$type_of_day[data_imputed$type_of_day != "weekend"] <- "weekday"

# convert type_of_day from character to factor
data_imputed$type_of_day <- as.factor(data_imputed$type_of_day)

# calculate average steps by interval across all days

library(knitr)
library(dplyr)
# df_imputed_steps_by_interval <- aggregate(data_imputed,steps ~ interval + type_of_day, fun=mean)
# 
# df_imputed_steps_by_interval <- aggregate(data_imputed,
#                 by = list(data_imputed$interval, data_imputed$type_of_day),
#                 FUN = mean,
#                 na.rm=TRUE)

df_imputed_steps_by_interval<- group_by(data_imputed, interval, type_of_day) %>% summarise(mean = mean(steps))

# df_imputed_steps_by_interval <- aggregate(steps ~ interval +type_of_day, data_imputed, FUN=average)

# creat a plot
qplot(interval, 
      mean, 
      data = df_imputed_steps_by_interval, 
      type = 'l', 
      geom=c("line"),
      xlab = "Interval", 
      ylab = "Number of steps", 
      main = "") +
  facet_wrap(~ type_of_day, ncol = 1)

```
