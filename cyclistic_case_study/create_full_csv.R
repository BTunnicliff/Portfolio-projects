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

#checking column names match
df_list <- list(colnames(divvy202007), colnames(divvy202008), colnames(divvy202009),
                colnames(divvy202010), colnames(divvy202011), colnames(divvy202012),
                colnames(divvy202101), colnames(divvy202102), colnames(divvy202103),
                colnames(divvy202104), colnames(divvy202105), colnames(divvy202106))
length(unique(df_list))==1

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

#Stack individual month's data frames into one large data frame and removing unnecessary columns
all_trips <- bind_rows(divvy202007, divvy202008, divvy202009, divvy202010, divvy202011, divvy202012, divvy202101,
                       divvy202102, divvy202103, divvy202104, divvy202105, divvy202106)

all_trips <- all_trips %>% select(-c(start_lat, start_lng, end_lat, end_lng))

#cleaning and adding new data.
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

#adding columns to list the day, month, and year of each ride. Also converting started_at and ended_at to datetime
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date), "%B")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%a")
all_trips$started_at <- as_datetime(all_trips$started_at)
all_trips$ended_at <- as_datetime(all_trips$ended_at)

#adding a ride length column (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#checking the structure of the added columns.
str(all_trips)

all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
str(all_trips)

#checking for bad data i.e ride length less than zero or test rides
all_trips %>% summarise(test = sum(start_station_name == "test"), negative_ride_length = sum(ride_length <= 0))

#removing bad data and creating a new data frame
all_trips_v2 <- all_trips %>% filter(!(ride_length <= 0))

#getting a summary of new data frame
summary(all_trips_v2)
str(all_trips_v2)

#write a full .csv file for easier use
write.csv(all_trips_v2, '../cyclistic_case_study/all_trips.csv')
