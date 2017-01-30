install.packages("tidyverse")
install.packages("lubridate")
install.packages("stringr")
library(tidyverse)
library(lubridate)
library(stringr)
calls <- read_csv("mpd-calls2014.csv", col_types = cols(CALL_ENTRY_DATE = col_character(), Call_Entry_Time = col_character(), CALL_DISPATCH_DATE = col_character(), Call_Dispatch_Time = col_character(), CALL_ONSCENE_DATE = col_character(), Call_Onscene_Time = col_character(), CALL_CLOSE_DATE = col_character(), Call_Close_Time = col_character()))

badTimeToGoodTime <- function(string) {
  if(str_detect(string, "[0-9]{6}")) {
    values = substring(string, seq(1, 5, 2), seq(2, 6, 2))
    hour = strtoi(values[1])
    minutes = strtoi(values[2])
    seconds = strtoi(values[3])

    am_pm = if(hour <= 12) {
      "AM"
    } else {
      "PM"
    }

    hour = if(hour == 0) {
      12
    } else if(hour >= 13) {
      hour - 12
    } else {
      hour
    }

    hour = str_pad(hour, 2, pad = "0")
    minutes = str_pad(minutes, 2, pad = "0")
    seconds = str_pad(seconds, 2, pad = "0")

    return(paste("", hour, ":", minutes, ":", seconds, " ", am_pm, sep = ""))
  } else {
    return(string)
  }
}

calls$Call_Entry_Time <- badTimeToGoodTime(calls$Call_Entry_Time)
calls$Call_Dispatch_Time <- badTimeToGoodTime(calls$Call_Dispatch_Time)
calls$Call_Onscene_Time <- badTimeToGoodTime(calls$Call_Onscene_Time)
calls$Call_Close_Time <- badTimeToGoodTime(calls$Call_Close_Time)

calls <- unite(calls, call_time, CALL_ENTRY_DATE, Call_Entry_Time, sep = " ") %>% unite(dispatch_time, CALL_DISPATCH_DATE, Call_Dispatch_Time, sep = " ") %>% unite(onscene_time, CALL_ONSCENE_DATE, Call_Onscene_Time, sep = " ") %>% unite(close_time, CALL_CLOSE_DATE, Call_Close_Time, sep = " ") %>% mutate(call_time = mdy_hms(call_time)) %>% mutate(onscene_time = mdy_hms(onscene_time)) %>% mutate(dispatch_time = mdy_hms(dispatch_time)) %>% mutate(close_time = mdy_hms(close_time))

calls <- filter(calls, !is.na(onscene_time)) %>% filter(!is.na(close_time)) %>% mutate(time_to_arrive = onscene_time - call_time)

normal_calls <- mutate(calls, time_to_arrive = as.numeric(time_to_arrive)/60) %>% filter(time_to_arrive < 180 & time_to_arrive > 1)

group_by(normal_calls, CALL_TYPE_FINAL_D) %>% summarize(median = quantile(time_to_arrive, c(.75)), count = n()) %>% arrange(desc(median)) %>% filter(count > 40) %>% print(n = 800)

ggplot(normal_calls) + geom_freqpoly(aes(time_to_arrive), bins = 100)

filter(normal_calls, CALL_TYPE_FINAL_D %in% c("THEFT VEHICLE", "ALTERED CURRENCY", "SHOTSPOTTER", "SUSP PKG/DEVICE", "GRAFFITI", "BB GUN COMPLNT")) %>% ggplot() + geom_freqpoly(aes(x = time_to_arrive, y = ..density.., color = CALL_TYPE_FINAL_D), bins = 20)

# to make png
# png("file.png")
# filter(normal_calls, CALL_TYPE_FINAL_D %in% c("THEFT VEHICLE", "ALTERED CURRENCY", "SHOTSPOTTER", "SUSP PKG/DEVICE", "GRAFFITI", "BB GUN COMPLNT")) %>% ggplot() + geom_freqpoly(aes(x = time_to_arrive, y = ..density.., color = CALL_TYPE_FINAL_D), bins = 20)
# dev.off()
