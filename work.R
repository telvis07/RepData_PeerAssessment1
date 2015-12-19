library(lubridate)
library(dplyr)
library(ggplot2)


getdata <- function(){
  df <- read.csv("activity.csv", na.strings = "NA", stringsAsFactors = FALSE)
  print(sprintf("Number of na steps : %d", sum(is.na(df$steps))))
  print(sprintf("Number of na intervals : %d", sum(is.na(df$interval))))
  df <- subset(df, !is.na(steps))
  df <- transform(df, date=ymd(date))
  df
}

steps_per_day <- function(df=NULL){
  if (is.null(df)){
    df <- getdata()
  }
  by_day <- group_by(df, date)
  sum_by_day <- summarize(by_day, steps=sum(steps, na.rm=TRUE))
  sum_by_day <- arrange(as.data.frame(sum_by_day), desc(steps))
  # histogram
  hist(sum_by_day$steps, col="blue", main="Histogram of steps taken per day")
  print(sprintf("Mean is %s", mean(sum_by_day$steps)))
  print(sprintf("Median is %s",median(sum_by_day$steps)))

  sum_by_day
}

average_per_day <- function(df){
  # combine the date and minutes to make a date/timestamp
  steps_by_five_mins <- mutate(df, date = date + minutes(interval))
  
  # plot time-series over the five minute intervals
  plot(steps_by_five_mins$date, steps_by_five_mins$steps, type="l")
  
  # plot a horizontal line of the mean() steps for a 5-minute interval over all the days
  abline(h = mean(steps_by_five_mins$steps), lwd=5, lty=1)
  
  # Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
  steps_by_five_mins[which.max(steps_by_five_mins$steps),]
}


fix_missing_values <- function(){
  df <- read.csv("activity.csv", na.strings = "NA", stringsAsFactors = FALSE)
  print(sprintf("Number of na steps : %d", sum(is.na(df$steps))))
  
  # get mean value per row
  mean_by_interval <- summarize(group_by(df, interval), steps=mean(steps, na.rm=TRUE))
  
  # update rows with NA for steps. use mean_by_interval for the 'interval'
  for (i in 1:nrow(df)){
    if (is.na(df[i,"steps"])){
      df[i,"steps"] <- mean_by_interval[which(mean_by_interval$interval==df[i,"interval"]), "steps"]
    }
  }
  steps_per_day(df=df)
}

weekend_trends <- function(df) {
  # boolean 'TRUE' is Sat or Sun. Else FALSE.
  df <- transform(df,is_weekend=weekdays(df$date) %in% c("Saturday", "Sunday"))
  
  # Create factor values based on TRUE/FALSE
  df <- mutate(df, is_weekend=factor(df$is_weekend, levels=c(FALSE, TRUE), 
                                     labels=c("weekday", "weekend")))
  
  # calculate mean per interval for 'weekdays'
  mean_by_interval_weekday <- summarize(group_by(subset(df, is_weekend=='weekday'), interval), 
                                        steps=mean(steps, na.rm=TRUE))
  
  # calculate mean per interval for 'weekends'
  mean_by_interval_weekend <- summarize(group_by(subset(df, is_weekend=='weekend'), interval), 
                                        steps=mean(steps, na.rm=TRUE))
  
  # combine data.frames with means summmaries
  mean_by_interval_weekday$is_weekend='weekday'
  mean_by_interval_weekend$is_weekend='weekend'
  mean_by_interval <- rbind(mean_by_interval_weekday, mean_by_interval_weekend)
  
  # panel plot with facet on 'is_weekend'
  qplot(interval, steps, data=mean_by_interval, geom="line", facets = is_weekend ~.)

  # Both the median and max for weekdays is greater than on weekends
  
  # weekday summary stats 
  summary(mean_by_interval_weekday)
  summary(mean_by_interval_weekend)
}