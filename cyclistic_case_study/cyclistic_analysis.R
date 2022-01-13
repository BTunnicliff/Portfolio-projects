#load necessary packages.
library(tidyverse)
library(lubridate)
library(broom)
library(scales)

#import data
all_trips <- read.csv('all_trips.csv')

#checking structure of all_trips, then changing data types
str(all_trips)

#remove x, ride id = chr, rideable_type = chr, started_at = datetime, ended_at = datetime, date = date,
#month = chr, day = chr, year = chr, day_of_Week = chr

all_trips <- all_trips %>%
  mutate(ride_id = as.character(ride_id),
         rideable_type = as.character(rideable_type),
         started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at),
         date = as.Date(date),
         month = as.character(month),
         day = as.character(day),
         year = as.character(year),
         day_of_week = as.character(day_of_week)) %>%
  select(!(X))

head(all_trips)
str(all_trips)
summary(all_trips)

#doing further data cleaning to remove unreasonable ride lengths
#(assuming more than a day, less than 30 seconds would be considered unreasonable)
all_trips %>% summarise(more_than_one_day = sum(ride_length>=86400),
                        less_than_30_secs = sum(ride_length<=30),
                        percentage_removed = percent((more_than_one_day+less_than_30_secs)/nrow(all_trips)))

all_trips_v2 <- all_trips %>%
  filter(!(ride_length<=30 | ride_length >= 86400))

all_trips_v2 %>% summarise(more_than_one_day = sum(ride_length>=86400), less_than_30_secs = sum(ride_length<=30))

#data should now be clean enough to begin the analysis

#comparing members and casual users
all_trips_v2 %>%
  group_by(member_casual) %>%
  summarise(num_members_casual = n(),
            mean = mean(ride_length), 
            median = median(ride_length),
            max = max(ride_length),
            min = min(ride_length))

#change the days of the week to be in order.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

#average ride time by day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)

#visualise number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title = "Number of rides for casual riders vs members for each day of the week",
       caption = "Data from: Divvy bikes data, Motivate International Inc; Jan 1 2020 to Dec 31 2020",
       x = "Day of the week",
       y = "Number of bike rides",
       fill = "Member type")

#Create a visualisation for average ride length.
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average duration of bike rides for each day of the week, by member type",
       caption = "Data from: Divvy bikes data, Motivate International Inc; Jan 1 2020 to Dec 31 2020",
       x = "Day of the week",
       y = "Average ride duration (seconds)",
       fill = "Member type")

#creating a new dataset with a weekend true/false column
weekend_or_weekday <- all_trips_v2 %>%
  mutate(weekend_tf = day_of_week %in% c("Sun", "Sat"))

#splitting casual data between weekdays and weekends
casual_weekend <- weekend_or_weekday %>%
  filter(member_casual == "casual" & weekend_tf == "TRUE") %>%
  select(ride_length)
casual_weekday <- weekend_or_weekday %>%
  filter(member_casual == "casual" & weekend_tf == "FALSE") %>%
  select(ride_length)

#plotting average ride length on weekends vs weekdays
weekend_or_weekday %>%
  filter(member_casual == 'casual') %>%
  group_by(weekend_tf) %>%
  summarise(average_duration = mean(ride_length)) %>%
  ggplot(aes(x= weekend_tf, y = average_duration)) +
  geom_col(width = 0.6) +
  scale_x_discrete(labels = c('FALSE' = 'Weekday', 'TRUE'= 'Weekend')) +
  labs(title = "Average duration of bike rides for casual riders on weekdays and weekends",
       caption = "Data from: Divvy bikes data, Motivate International Inc; Jan 1 2020 to Dec 31 2020",
       x = "",
       y = "Average ride duration (seconds)")

#testing significance of the difference between weedays and weekends
t.test(casual_weekend, casual_weekday, alternative = 'greater')

#there is a significant difference between the two, could possibly advertise more on weekends.
#weekday usage could possibly be something to look at, start times of rides could indicate what people are using the service
#for which could provide further insight. otherwise looking at seasonal change could be a good idea.


#Rider behaviour by month
all_trips_v2$month <- ordered(all_trips_v2$month, levels = c("January", "February", "March", "April",
                                                             "May", "June", "July", "August", "September", "October",
                                                             "November", "December"))


#Creating visualisations for number of rides and average ride length by month
all_trips_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n()) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Number of rides for each month, by member type",
       caption = "Data from: Divvy bikes data, Motivate International Inc; Jan 1 2020 to Dec 31 2020",
       x = "Month",
       y = "Number of rides",
       fill = "Member type")

all_trips_v2 %>%
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title = "Average ride duration for each month, by member type",
       caption = "Data from: Divvy bikes data, Motivate International Inc; Jan 1 2020 to Dec 31 2020",
       x = "Month",
       y = "Average ride duration (seconds)",
       fill = "Member type")

#casual riders ride for longer throughout the year but use drops heavily in colder seasons. spikes in summer.
#number of rides is low in colder seasons but ride durations stays relatively consistent.

#creating a plot of number of rides by hour

all_trips_v2 %>%
  group_by(hour_of_day = hour(round_date(started_at, 'hour'))) %>%
  group_by(hour_of_day, member_casual) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = hour_of_day, y = number_of_rides, fill = member_casual)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Number of rides for each hour of the day, by member type",
       caption = "Data from: Divvy bikes data, Motivate International Inc; Jan 1 2020 to Dec 31 2020",
       x = "Time of day (Hour)",
       y = "Number of rides",
       fill = "Member type")

#members tend to use the service during commuting hours, could possibly check if this is still the case on weekdays and maybe
#have using the service for commuting as a way to get casual riders to join.

#plot of start times on weekdays only.
weekend_or_weekday %>%
  filter(weekend_tf == 'FALSE') %>%
  group_by(hour_of_day = hour(round_date(started_at, 'hour'))) %>%
  group_by(hour_of_day, member_casual) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  ggplot(aes(x = hour_of_day, y = number_of_rides, fill = member_casual)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Number of rides for each hour of the day, by member type (Weekdays only)",
       caption = "Data from: Divvy bikes data, Motivate International Inc; Jan 1 2020 to Dec 31 2020",
       x = "Time of day (Hour)",
       y = "Number of rides",
       fill = "Member type")

#definitely could make commuting a selling point.

#finding the most popular start and end stations for casual riders
all_trips_v2 %>%
  group_by(start_station_name, member_casual) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  filter(start_station_name != "", member_casual == "casual") %>%
  arrange(-number_of_rides) %>%
  head(n = 10) %>%
  ggplot(aes(x = reorder(start_station_name, number_of_rides), y = number_of_rides)) +
  geom_col() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  coord_flip() +
  labs(title = "Top 10 start stations for casual riders",
       caption = "Data from: Divvy bikes data, Motivate International Inc; Jan 1 2020 to Dec 31 2020",
       x = "Station name",
       y = "Number of rides")

all_trips_v2 %>%
  group_by(end_station_name, member_casual) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  filter(end_station_name != "", member_casual == "casual") %>%
  arrange(-number_of_rides) %>%
  head(n = 10) %>%
  ggplot(aes(x = reorder(end_station_name, number_of_rides), y = number_of_rides)) +
  geom_col() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  coord_flip() +
  labs(title = "Top 10 end stations for casual riders",
       caption = "Data from: Divvy bikes data, Motivate International Inc; Jan 1 2020 to Dec 31 2020",
       x = "Station name",
       y = "Number of rides")
