#load necessary packages.
library(tidyverse)
library(lubridate)

#Importing data.
divvy202007 <- read.csv("202007-divvy-tripdata.csv")
divvy202008 <- read.csv("202008-divvy-tripdata.csv")
divvy202009 <- read.csv("202009-divvy-tripdata.csv")
divvy202010 <- read.csv("202010-divvy-tripdata.csv")
divvy202011 <- read.csv("202011-divvy-tripdata.csv")
divvy202012 <- read.csv("202012-divvy-tripdata.csv")
divvy202101 <- read.csv("202101-divvy-tripdata.csv")
divvy202102 <- read.csv("202102-divvy-tripdata.csv")
divvy202103 <- read.csv("202103-divvy-tripdata.csv")
divvy202104 <- read.csv("202104-divvy-tripdata.csv")
divvy202105 <- read.csv("202105-divvy-tripdata.csv")
divvy202106 <- read.csv("202106-divvy-tripdata.csv")

#Wrangling data and combining into a single file.
colnames(divvy202007)
colnames(divvy202008)
colnames(divvy202009)
colnames(divvy202010)
colnames(divvy202011)
colnames(divvy202012)
colnames(divvy202101)
colnames(divvy202102)
colnames(divvy202103)
colnames(divvy202104)
colnames(divvy202105)
colnames(divvy202106)

#All column names match, checking if the structure of each data frame matches.
str(divvy202007)
str(divvy202008)
str(divvy202009)
str(divvy202010)
str(divvy202011)
str(divvy202012)
str(divvy202101)
str(divvy202102)
str(divvy202103)
str(divvy202104)
str(divvy202105)
str(divvy202106)

#converting ride_id and rideable_type to character so that they stack correctly. also converting start and end station ids to int for 202012 onward.
divvy202007 <- mutate(divvy202007, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
divvy202008 <- mutate(divvy202008, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
divvy202009 <- mutate(divvy202009, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
divvy202010 <- mutate(divvy202010, ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
divvy202011 <- divvy202011 %>% mutate(ride_id = as.character(ride_id), rideable_type = as.character(rideable_type))
divvy202012 <- divvy202012 %>% mutate(ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.integer(start_station_id), end_station_id = as.integer(end_station_id))
divvy202101 <- divvy202101 %>% mutate(ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.integer(start_station_id), end_station_id = as.integer(end_station_id))
divvy202102 <- divvy202102 %>% mutate(ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.integer(start_station_id), end_station_id = as.integer(end_station_id))
divvy202103 <- divvy202103 %>% mutate(ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.integer(start_station_id), end_station_id = as.integer(end_station_id))
divvy202104 <- divvy202104 %>% mutate(ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.integer(start_station_id), end_station_id = as.integer(end_station_id))
divvy202105 <- divvy202105 %>% mutate(ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.integer(start_station_id), end_station_id = as.integer(end_station_id))
divvy202106 <- divvy202106 %>% mutate(ride_id = as.character(ride_id), rideable_type = as.character(rideable_type), start_station_id = as.integer(start_station_id), end_station_id = as.integer(end_station_id))

#Stack individual month's data frames into one large data frame.
all_trips <- bind_rows(divvy202007, divvy202008, divvy202009, divvy202010, divvy202011, divvy202012, divvy202101,
                       divvy202102, divvy202103, divvy202104, divvy202105, divvy202106)


all_trips <- all_trips %>% select(-c(start_lat, start_lng, end_lat, end_lng))
colnames(all_trips)

#cleaning and adding new data.
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

#adding columns to list the day, month, and year of each ride.
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%B")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%a")

#adding a ride length column (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#checking the structure of the added columns.
str(all_trips)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
str(all_trips)

#removing "bad" data, where bikes were taken out of docks for quality checks, or ride_length was negative.
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0),]

#adding a start time column for further analysis
all_trips_v2$start_time <- as.POSIXct(all_trips_v2$started_at, "%Y-%m-%d %H:%M:%S")
str(all_trips_v2)

#Getting descriptive statistics.
all_trips_v2 %>% summarise(mean_ride_length = mean(ride_length))
table(all_trips_v2$member_casual)

#comparing members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

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

#creating a plot of number of rides by hour

all_trips_v2 %>%
  group_by(hour_of_day = hour(round_date(start_time, 'hour'))) %>%
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

# Creating a plot of popular start stations for members and casual riders.
all_trips_v2 %>%
  group_by(start_station_name, member_casual) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  filter(start_station_name != "", member_casual == "member") %>%
  arrange(-number_of_rides) %>%
  head(n = 20) %>%
  ggplot(aes(x = reorder(start_station_name, number_of_rides), y = number_of_rides)) +
  geom_col() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  coord_flip() +
  labs(title = "Top 20 start stations for members",
       caption = "Data from: Divvy bikes data, Motivate International Inc; Jan 1 2020 to Dec 31 2020",
       x = "Station name",
       y = "Number of rides")

all_trips_v2 %>%
  group_by(start_station_name, member_casual) %>%
  summarise(number_of_rides = n(), .groups = 'drop') %>%
  filter(start_station_name != "", member_casual == "casual") %>%
  arrange(-number_of_rides) %>%
  head(n = 20) %>%
  ggplot(aes(x = reorder(start_station_name, number_of_rides), y = number_of_rides)) +
  geom_col() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  coord_flip() +
  labs(title = "Top 20 start stations for casual riders",
       caption = "Data from: Divvy bikes data, Motivate International Inc; Jan 1 2020 to Dec 31 2020",
       x = "Station name",
       y = "Number of rides")

#Plotting usage of different rideable types for members and casual riders.
all_trips_v2 %>%
  group_by(rideable_type, member_casual) %>%
  summarise(number_of_rides = n(), .group = 'drop') %>%
  ggplot(aes(x = member_casual, y = number_of_rides, fill = member_casual)) +
  geom_col(position = 'dodge') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  facet_wrap(~rideable_type) +
  labs(title = "Usage of different rideable types: Members vs. Casual riders",
       caption = "Data from: Divvy bikes data, Motivate International Inc; Jan 1 2020 to Dec 31 2020",
       x = "",
       y = "Number of rides",
       fill = 'Member type')
