---
title: 'Reproducible Research: PA1'
author: "SunjayM"
date: "March 29, 2016"
output: word_document
---

#Loading and preprocessing the data
#####Load the data (i.e. read.csv())

```{r}
setwd("~/R/Data Sets/Reproducible Research") #Set working Directory with the data set
dir() # to verify the files located in the directory and that it has the accurate data set
```

##### Load the necessary packages that will be useful throughtout analysis and plotting
```{r}
library(ggplot2)
library(plyr)
```
####Read in the Data from directory and classify the data to create consistency. 
```{r}
activity <- read.csv("activity.csv", colClass=c('integer', 'Date', 'integer'))
str(activity) ##To see the data set's variables, observations, and first few values in the columns.
```
####Process/transform the data (if necessary) into a format suitable for your analysis
```{r}
activity$date <- as.Date(activity$date) #convert the date colum
activity$Day <- activity$Day <-weekdays(as.Date(activity$date)) #To seperate out dataset into difderent days according to the dates for analysis down the road. 
activity$DateTime <- as.POSIXct(activity$date, format = "%Y-%m-%d")
CleanSteps <- activity[!is.na(activity$steps), ] ##Helps to remove the NA values in the steps column
```
## What is mean total number of steps taken per day?
#### Calculate the total number of steps taken per day
```{r}
SumSteps <- aggregate(activity$steps ~ activity$date, FUN = sum, )
colnames(SumSteps) <- c("Date", "Steps") #Renaming columns in new subset date to create histogram
```
#### Plotting the Histogram to display thee total steps per day 
```{r, echo=TRUE}
hist(SumSteps$Steps, xlab = "Steps", main = "Total Steps / Day")
```
![](/images/TotalSteps.png)
####To identify the mean of steps taken per day, just run the calculation for mean in R

```{r}
mean(SumSteps$Steps)
[1] 10766.19
as.integer(mean(SumSteps$Steps)) #this is to provide a whole number of steps taken per day.
[1] 10766
```
####To identify the median of steps taken per day, just run the calculation for median in R
```{r}
median(SumSteps$Steps)
[1] 10765
as.integer(median(SumSteps$Steps))
[1] 10765
```
# What is the average daily activity pattern?
##### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
StepsInter <- aggregate(steps ~ interval, activity, mean)
plot(StepsInter, type= "l", col="orange", main= "Average Daily Activity Pattern")
```
![](/images/AvgDailyActivityPattern.png)
##### Calculate 5-minute interval containing the maximum number of steps

```{r}
StepsInter$interval[which.max(StepsInter$steps)]
```
[1] 835
#Imputing missing values

#####Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r}
sum(is.na(activity$steps))
[1] 2304
```
##### Devise a strategy for filling in all of the missing values in the dataset.

```{r}
avgTable <- ddply(CleanSteps, .(interval, Day), summarize, Avg = mean(steps))
```
##### Create dataset with all NAs for substitution
```{r}
NAdata<- activity[is.na(activity$steps),]
```
##### Merge NA data with average weekday interval for substitution
```{r}
NewData<-merge(NAdata, avgTable, by=c("interval", "Day"))
```
##### Reorder the new substituded data in the same format as clean data set
```{r}
NewData2<- NewData[,c(6,4,1,2,5)]
colnames(NewData2)<- c("steps", "date", "interval", "Day", "DateTime")
```
##### Merge the NA averages and non NA data together
```{r}
MergeData <- rbind(CleanSteps, NewData2)
```
##### Create sum of steps per date to compare with step 1
```{r}
SumSteps2 <- aggregate(MergeData$steps ~ MergeData$date, FUN=sum, )
colnames(SumSteps2)<- c("Date", "Steps")
```
#####  Mean number of Steps with NA data taken care of and returning a whole number
```{r}
as.integer(mean(SumSteps2$Steps))

[1] 10821

```
#####  Median of Steps with NA data taken care of and returning a whole number
```{r}
as.integer(median(SumSteps2$Steps))
[1] 11015

```
####  Creating the histogram of total steps per day, categorized by data set to show impact of including the missing values
```{r, echo=TRUE}
hist(SumSteps2$Steps, breaks=5, xlab="Steps", main = "Total Steps / Day with NAs Fixed", col="Blue")
hist(SumSteps$Steps, breaks=5, xlab="Steps", main = "Total Steps / Day with NAs Fixed", col="Grey", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("blue", "grey"))
```
![](/images/TotalStepsperDayNAs.png)
#Are there differences in activity patterns between weekdays and weekends?

##### Create a new column to create a Weekday or Weekend depending on day of week
```{r}
MergeData$TypofDay <- ifelse(MergeData$Day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
```
```{r}
library(lattice)
```
##### Create new dataset to consolidate the data by the interval and the type of day in the week
```{r}
StepsInter2 <- ddply(MergeData, .(interval, TypofDay), summarize, Avg = mean(steps))
```
##### Plot 
```{r, echo=TRUE}
xyplot(Avg~interval | TypofDay, data= StepsInter2, type = "l", layout = c(1,2), col = "red", grid = TRUE,
       main= "Avg. Steps per Inerval by Type of Day",
       ylab= "Avg. Number of Steps", 
       xlab= "Intervals")
```
![](/images/AvgDailyActivityPattern.png)
