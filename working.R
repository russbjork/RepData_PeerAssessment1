## 
## Reproducible Data - Project 1
##
library(dplyr)
library(ggplot2)

##
## Read in raw data from activity.csv file
##
rawdata <- read.csv("activity.csv",header=TRUE,na.strings="NA")

##
## Transform the data, set NA = 0 from steps and convert date to valid Date
##
data <- rawdata %>% 
        # removed as instructed to ignore, this would impact mean calculation
        # mutate(steps = coalesce(steps, 0)) %>% 
        mutate(date=as.Date(date,format = "%Y-%m-%d"))

##
## Data summary and calculations
##
#summary(data)
dailysteps <- data %>%
    filter(!is.na(steps)) %>%
    group_by(date) %>%
    summarize(steps = sum(steps)) %>%
    print(dailysteps)
meansteps <- mean(dailysteps$steps, na.rm=TRUE)
    print(meansteps)
medsteps <- median(dailysteps$steps, na.rm=TRUE)
    print(medsteps)
##
## Graph and plot the transformed data
##
# Basic histogram
ggplot(dailysteps, aes(x=steps)) + 
    geom_histogram(binwidth=1000, color="blue", fill="white") +
    labs(title = "Histogram of Steps per day", x = "Steps", y = "Frequency") +
    theme(plot.title.position = 'plot',plot.title = element_text(hjust = 0.5))