setwd("C:/Coursera/RepData_PeerAssessment1")


########## Loading and preprocessing the data  ###########

data <- read.csv("activity.csv",na.strings = "NA", header = TRUE)
dim(data)
head(data)


######## Mean total number of steps taken per day ########

steps_per_day <-aggregate(data$steps, by=list(data$date), FUN=sum, na.rm=TRUE)
dim(steps_per_day)
head(steps_per_day)
names(steps_per_day) <- c("Date","Total_number_of_steps")

hist(steps_per_day$Total_number_of_steps,10)

mean(steps_per_day$Total_number_of_steps) #9354.23
median(steps_per_day$Total_number_of_steps) #10395


############# Average daily activity pattern #############

avg_steps_per_interval <- aggregate(data$steps, by=list(data$interval), FUN=mean, na.rm=TRUE)
names(avg_steps_per_interval) <- c("Interval","Average_number_of_steps")

plot(avg_steps_per_interval,type="l")
title(main="Average daily activity pattern")

avg_steps_per_interval[which(avg_steps_per_interval$Average_number_of_steps==max(avg_steps_per_interval$Average_number_of_steps)),]
#835 (206.1698)


################ Imputing missing values #################

dim(data[!complete.cases(data), ]) #2304

data_with_missings_filled_in       <- data
data_with_missings_filled_in$steps <- ifelse(is.na(data_with_missings_filled_in$steps), 
                                             avg_steps_per_interval[which(avg_steps_per_interval$Interval
                                                                          ==data_with_missings_filled_in$interval),2],
                                             data_with_missings_filled_in$steps)

steps_per_day_new <-aggregate(data_with_missings_filled_in$steps, by=list(data_with_missings_filled_in$date), FUN=sum, na.rm=TRUE)
dim(steps_per_day_new)
head(steps_per_day_new)
names(steps_per_day_new) <- c("Date","Total_number_of_steps")

hist(steps_per_day_new$Total_number_of_steps,10)

mean(steps_per_day_new$Total_number_of_steps)   #9530.724 (9354.23)
median(steps_per_day_new$Total_number_of_steps) #10439    (10395)


########### Differences in activity patterns #############
############# between weekdays and weekends ##############












