library(ggplot2, quietly = T, warn.conflicts = F)
library(lubridate, quietly = T, warn.conflicts = F)
library(dplyr, quietly = T, warn.conflicts = F)

data <- read.csv("activity.csv", stringsAsFactors=FALSE) #get the data
data$date <- ymd(data$date) # date parsing  

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
data_ts <- data[!is.na(steps), list(n = .N, 
                                    total_steps = sum(steps), 
                                    mean_steps = mean(steps)), 
                by = list(interval)]
ts <- ggplot(data = data_ts, aes(interval, mean_steps))
ts + geom_line() + 
        geom_point(data = data_ts[max(mean_steps) == mean_steps,] , size = 3, col = "red") +
        ggtitle("Average steps across days by interval")


#useless: ts2 <- ggplot(data = data[!is.na(steps), list(n = .N, 
#                                              total_steps = sum(steps), 
#                                              mean_steps = mean(steps)), 
#                          by = list(date, interval)])
# ts2 + aes(interval, mean_steps) + geom_point(alpha = .5) 

# 2. Which 5-minute interval, 
# on average across all the days in the dataset, contains the maximum number of steps?