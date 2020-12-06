## 
## Reproducible Data - Project 1
##
library(dplyr)
library(ggplot2)
library(knitr)
opts_chunk$set(echo = TRUE)

##
## Read in raw data from activity.csv file
##
rawdata <- read.csv("activity.csv",header=TRUE,na.strings="NA")

##
## Transform the data convert date attribute to valid Date - NEW DATASET
##
data <- rawdata %>% 
        mutate(date=as.Date(date,format = "%Y-%m-%d"))

##
## Transform the data replacing NA steps with the average number of steps in the same 5-min interval
## from the date transformed data, use tapply for filling in the missing values  - NEW DATASET
##
datafill <- data
nas <- is.na(datafill$steps)
avgintvl <- tapply(datafill$steps, datafill$interval, mean, na.rm=TRUE, simplify=TRUE)
datafill$steps[nas] <- avgintvl[as.character(datafill$interval[nas])]
checkdata <- sum(is.na(datafill$steps))
    print(checkdata)

##
## Transform the step NA filled data, add factor variable for weekday or weekend - NEW DATASET
##
datalevels <- mutate(datafill, weektype = ifelse(weekdays(datafill$date) == "Saturday" | weekdays(datafill$date) == "Sunday", "weekend", "weekday"))
    datafill$weektype <- as.factor(datailll$weektype)
    print(datalevels)
    
##
## Data filtered/grouped summary and calculations
##

## Data filtered and grouped by date for a summary of steps per day
dailysteps <- data %>%
    filter(!is.na(steps)) %>%
    group_by(date) %>%
    summarize(steps = sum(steps)) %>%
    print(dailysteps)

## Calculation of mean steps taken each day.
meansteps <- mean(dailysteps$steps, na.rm=TRUE)
    print(meansteps)

## Calculation of median steps taken each day.
medsteps <- median(dailysteps$steps, na.rm=TRUE)
    print(medsteps)
    
## Data filtered and grouped by interval for mean of steps per day
intvlsteps <- data %>%
    filter(!is.na(steps)) %>%
    group_by(interval) %>%
    summarize(steps = mean(steps))
    print(intvlsteps)

## Data evaluated to determine interval with maximum steps, on average, across all the days
maxintvl <- intvlsteps[which.max(intvlsteps$steps),]
    print(maxintvl)
        
## Summarize total missing values in the dataset
nodata <- sum(is.na(data$steps))
    print(nodata)
    
## Data evaluated to calculate the number of steps taken in each 5-minute interval per day
stepsfill <- datafill %>%
    filter(!is.na(steps)) %>%
    group_by(date) %>%
    summarize(steps = sum(steps)) %>%
    print(stepsfill) 

## Calculation of mean steps taken each day on the NA filled data.
meanstepsfill <- mean(stepsfill$steps, na.rm = TRUE)
    print(meanstepsfill)
    
## Calculation of median steps taken each day on the NA filled data.    
medianstepsfill <- median(stepsfill$steps, na.rm = TRUE)
    print(medianstepsfill)
    
## Data evaluated to calculate the average steps in the 5-minute interval for weekday and weekend
intervallevels <- datalevels %>%
    group_by(interval, weektype) %>%
    summarize(steps = mean(steps))
    print(intervallevels)
    
##
## Graph and plot the transformed data
##
# Basic histogram of summarized steps by day
ggplot(dailysteps, aes(x=steps)) + 
    geom_histogram(binwidth=1000, color="blue", fill="white") +
    labs(title = "Histogram of Steps per day", x = "Steps", y = "Frequency") +
    theme(plot.title.position = 'plot',plot.title = element_text(hjust = 0.5))

# Basic time series of mean steps per day
ggplot(intvlsteps, aes(x=interval, y=steps)) +
    geom_line(color = "blue") +
    labs(title = "Time Series of Steps per Interval", x = "Interval", y = "Steps") +
    theme(plot.title.position = 'plot',plot.title = element_text(hjust = 0.5))

# Basic histogram of summarized steps by day with NA values replaced with interval average data
ggplot(stepsfill, aes(x = steps)) +
    geom_histogram(binwidth = 1000, color="blue", fill = "white") +
    labs(title = "Histogram of Steps per Day - Missing Data Replaced", x = "Steps", y = "Frequency") +
    theme(plot.title.position = 'plot',plot.title = element_text(hjust = 0.5))

# Basic time series of mean steps per day weekday vs weekend
ggplot(intervallevels, aes(x=interval, y=steps, color = weektype)) +
    geom_line() +
    facet_wrap(~weektype, ncol = 1, nrow=2) +
    labs(title = "Time Series of Steps per Interval by Weektype", x = "Interval", y = "Steps") +
    theme(plot.title.position = 'plot',plot.title = element_text(hjust = 0.33))
