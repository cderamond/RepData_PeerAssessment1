library(ggplot2, quietly = T, warn.conflicts = F)
library(lubridate, quietly = T, warn.conflicts = F)
library(dplyr, quietly = T, warn.conflicts = F)
library(data.table)

data <- read.csv("activity.csv", stringsAsFactors=FALSE) #get the data
data$date <- ymd(data$date) # date parsing  

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
data <- as.data.table(data)
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

# imputation

DataFixed <- left_join(data[is.na(steps),], 
                       select(data_ts, interval, steps = mean_steps), 
                       by = "interval") %>%
        select(steps = steps.y, 3, 1) %>%
        arrange(date, interval)

colSums(is.na(DataFixed))

# merging data set
#         dataMerge <- union(DataFixed, data[!is.na(steps),]) %>%
#                 arrange(date, interval)

dataMerge <- rbind_all(list(DataFixed, data[!is.na(steps),])) %>%
        mutate(date = ymd(date)) %>%
        arrange(date, interval)
        #print(as.data.table())

# 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median 
# total number of steps taken per day. Do these values differ from the estimates from the first part of the
# assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

# we will add a factor to identify sets before merge
dataMerge$setID <- "Fixed set"
data$setID <- "Original set"

dataComplete <- rbind_all(list(dataMerge, data)) %>%
        mutate(date = ymd(date), setID = as.factor(setID)) %>%
        arrange(setID, date, interval)

hist_complete <- dataComplete %>%
        filter(!is.na(steps)) %>%
        group_by( date, setID) %>%
        summarise(n = n(), 
                  total = sum(steps), 
                  mean = mean(steps, na.rm = T), 
                  median = median(total)) %>%
        arrange(date, setID)
qplot(total, data = hist_complete, fll = setID)
# 
qplot(total, data = hist_complete, 
      fill = setID,
      main = "Total steps comparison over data sets")
