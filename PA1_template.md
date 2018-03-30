---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
#Environment Setup
install.packages("tidyr")
install.packages("readr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("data.table")
library(tidyr) 
library(readr)
library(dplyr)
library(lubridate)
library(data.table)


## Loading and preprocessing the data
url1<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url1, destfile= "dataset.zip", method= "curl")
unzip("dataset.zip")->data1
read.csv(data1)-> actdata
head(actdata)
str(actdata)

actdata$date<- as.Date(as.character(actdata$date))
actdata$interval<- factor(actdata$interval)
str(actdata)

## What is mean total number of steps taken per day?

stepsperday<- aggregate(steps~date,actdata, FUN= sum )
hist(stepsperday$steps, xlab = "Sum of Steps per Day", ylab = "Frequency", main = "Total Steps per Day")

meandata<- round(mean(stepsperday$steps))
print(paste0(("The mean total number of steps taken per day is : "), meandata))

mediandata<- round(median(stepsperday$steps))
print(paste0(("The median total number of steps taken per day is : "), mediandata))


## What is the average daily activity pattern?
advtsep<- tapply(actdata$steps, actdata$interval, mean, na.rm= TRUE)
plot(advtsep, type = "l", xlab= " Time", ylab = " Steps", main = "Average Aaily Activity Pattern")

## Imputing missing values

actdata1<- actdata[!is.na(as.character(actdata$steps)),]
print(head(actdata1))
print(summary(actdata1))

## Are there differences in activity patterns between weekdays and weekends?

actdata1$Weekday<- wday(actdata1$date)
weekday<-subset(actdata1, actdata1$Weekday>=3)
weekend<-subset(actdata1, actdata1$Weekday>=2)
weekday$week<- "Week Day"
weekend$week<- "Week End"
Both<- rbind(weekday, weekend)

ggplot(Both, aes(interval,steps, fill=week))+geom_bar(stat = "identity" )+facet_grid(.~week) +labs(x= "Time", y="Steps", title= "Average Activity Pattern During the Week")

