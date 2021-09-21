# Loading necessary packages.
library(tidyverse)
library(lubridate)

#reading data.
crash_data <- read.csv("../data/airplane_crashes_dataset_since_1908.csv")
crash_data[crash_data == "?"] <- NA
str(crash_data)

#finding and removing missing values
colSums(is.na(crash_data))

crash_data_v2 <- crash_data %>%
  select(-c(time, flight_no, summary)) %>%
  drop_na()

str(crash_data_v2)
head(crash_data_v2)
colSums(crash_data_v2 == "?")
colSums(is.na(crash_data_v2))

#Changing data formats to ease calculations/analysis
#(had to change to character before numeric otherwise something goes wrong)
crash_data_v2 <- crash_data_v2 %>%
  mutate(date = as.Date(date, format = "%B %d, %Y"),
         all_aboard = as.numeric(as.character(all_aboard)),
         passengers_aboard = as.numeric(as.character(passengers_aboard)),
         crew_aboard = as.numeric(as.character(crew_aboard)),
         all_fatalities = as.numeric(as.character(all_fatalities)),
         passenger_fatalities = as.numeric(as.character(passenger_fatalities)),
         crew_fatalities = as.numeric(as.character(crew_fatalities)),
         ground = as.numeric(as.character(ground))
         )

str(crash_data_v2)
head(crash_data_v2)
tail(crash_data_v2)
summary(crash_data_v2)

#checking for logical issues (more deaths than passengers, more or less total deaths than expected etc.)
crash_data_v2 %>% summarise(fatalities_total_wrong = sum(all_fatalities != passenger_fatalities+crew_fatalities),
                            all_aboard_total_wrong = sum(all_aboard != passengers_aboard + crew_aboard),
                            fatalities_more_than_aboard = sum(all_fatalities > all_aboard))

crash_data_v2 %>%
  filter(all_fatalities != passenger_fatalities + crew_fatalities)

crash_data_v2 %>%
  filter(all_aboard != crew_aboard + passengers_aboard)
#correcting bad data
#correct dec 29 1974, tikal guatemala and april 30 1983, jacksonville florida as there is incorrectly entered data
#in the passenger fatalities column
which(crash_data_v2$date == "1974-12-29" & crash_data_v2$location == "Tikal, Guatemala")
crash_data_v2[2002,] <- crash_data_v2[2002,] %>% 
  mutate(passenger_fatalities = 21)

which(crash_data_v2$date == "1983-04-30" & crash_data_v2$location == "Jacksonville, Florida")
crash_data_v2[2344,] <- crash_data_v2[2344,] %>%
  mutate(passenger_fatalities = 9)

#assume all others have all_fatalities equal to passenger + crew fatalities due to website having incorrect data
crash_data_v2$all_fatalities <- if_else(crash_data_v2$all_fatalities != crash_data_v2$passenger_fatalities + crash_data_v2$crew_fatalities, 
                                        crash_data_v2$passenger_fatalities + crash_data_v2$crew_fatalities, crash_data_v2$all_fatalities)
crash_data_v2$all_aboard <- if_else(crash_data_v2$all_aboard != crash_data_v2$crew_aboard + crash_data_v2$passengers_aboard,
                                    crash_data_v2$crew_aboard + crash_data_v2$passengers_aboard, crash_data_v2$all_aboard)

crash_data_v2 %>% summarise(fatalities_total_wrong = sum(all_fatalities != passenger_fatalities+crew_fatalities),
                            all_aboard_total_wrong = sum(all_aboard != passengers_aboard + crew_aboard),
                            fatalities_more_than_aboard = sum(all_fatalities > all_aboard))

#beginning analysis by adding fatality and survival rate columns
crash_data_v2 <- crash_data_v2 %>%
  mutate(fatality_rate = all_fatalities/all_aboard, survival_rate = 1-fatality_rate) %>%
  mutate(fatality_rate = round(fatality_rate, digits = 3), survival_rate = round(survival_rate, digits = 3)) %>%
  mutate(survival_rate = replace(survival_rate, is.nan(survival_rate), 0), fatality_rate = replace(fatality_rate, is.nan(fatality_rate), 0))

#plotting number of accidents by year
crash_data_v2 %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  arrange(year) %>%
  ggplot(aes(x = year, y = count)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1925, 2025, 20)) + 
  labs(title = "Count of plane crashes by year",
       y = "Count of crashes",
       x = "Year")

#plotting average survival rate by year
crash_data_v2 %>%
  mutate(year = year(date), survival_rate = survival_rate*100) %>%
  group_by(year) %>%
  summarise(survival_mean = mean(survival_rate)) %>%
  arrange(year) %>%
  ggplot(aes(x = year, y = survival_mean)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1925, 2025, 20)) +
  labs(title = "Average plane crash survival rate by year",
       x = "Year",
       y = "Average survival rate (%)")

#plotting average passengers and crew aboard for each year
crash_data_v2 %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(mean_aboard = mean(all_aboard)) %>%
  arrange(year) %>%
  ggplot(aes(x = year, y = mean_aboard)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1925, 2025, 20)) +
  labs(title = "Average number of people aboard by year",
       y = "Mean aboard",
       x = "Year")

#plotting aircraft with highest crash count
crash_data_v2 %>%
  group_by(ac_type) %>%
  summarise(number_of_crashes = n()) %>%
  arrange(-number_of_crashes) %>%
  head(n = 10) %>%
  ggplot(aes(x = reorder(ac_type, number_of_crashes), y = number_of_crashes, fill = factor(ifelse(ac_type == "Douglas DC-3", "Highlighted", "Normal")))) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("red", "grey50")) +
  coord_flip() +
  labs(x = "Aircraft type",
       y = "Number of crashes",
       title = "Top 10 largest crash-count aircraft")

crash_data_v2 %>%
  group_by(ac_type) %>%
  summarise(number_of_crashes = n()) %>%
  arrange(-number_of_crashes) %>%
  head(n = 10)
#Douglas DC-3 crash count is 377% higher than the next highest aircraft

#plotting average survival rate for top 10 highest crash count aircraft
crash_data_v2 %>%
  group_by(ac_type) %>%
  summarise(number_of_crashes = n(), mean_survival = (mean(survival_rate))*100) %>%
  arrange(-number_of_crashes) %>%
  head(n = 10) %>%
  ggplot(aes(x = reorder(ac_type, number_of_crashes), y = mean_survival, fill = factor(ifelse(ac_type == "Douglas DC-3", "Highlighted", "Normal")))) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("red", "grey50")) +
  coord_flip() +
  labs(x = "Aircraft type (ordered by crash count)",
       y = "Average survival rate (%)",
       title = "Average survival rate for top 10 largest crash-count aircraft")
#Average survival rate is about the same as other top 10s

#Why does the Douglas DC-3 have so many crashes?
# investigating service use

#checking range of years between first and last crash
crash_data_v2 %>%
  group_by(ac_type) %>%
  summarise(num_crashes = n(), min_year = min(year(date)), max_year = max(year(date)), year_diff = max_year-min_year) %>%
  arrange(-year_diff) %>%
  head(n = 10)

# As far as my research can tell, there were thousands of dc-3s built and in active flight, so you could expect it to have a higher total amount of crashes
#especially seeing as it has been in service since the  1930s, however I cannot find anything in the data that
#would indicate that it is an unsafe plane given that the survival rate is somewhat consistent with other planes

#checking if there is a large deviation from the mean survival rate for dc-3s (data not normally distributed)
crash_survival <- crash_data_v2 %>%
  summarise(survival_rate)
dc3_survival <- crash_data_v2 %>%
  filter(ac_type == "Douglas DC-3") %>%
  summarise(survival_rate)
mean_survival_rate <- mean(crash_survival$survival_rate)
dc3_mean_survival_rate <- mean(dc3_survival$survival_rate)
standard_deviation <- sd(crash_survival$survival_rate)
dc3_deviation <- dc3_mean_survival_rate - mean_survival_rate

df <- data.frame(mean_survival_rate, dc3_mean_survival_rate, standard_deviation, dc3_deviation)
df

#it is clear that the average survival rate for the dc-3 is not much different from the average survival rate.
#and is well within one standard deviation.

#Looking at survival rate for training routes
crash_data_v2 %>%
  group_by(route) %>%
  summarise(count_route = n()) %>%
  arrange(-count_route) %>%
  head(n = 6) %>%
  ggplot(aes(x = reorder(route, count_route), y = count_route, fill = factor(ifelse(route=="Training", "Highlighted", "Normal")))) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("red", "grey50")) +
  labs(x = "Route",
       y = "Count of route",
       title = "Flight routes with the largest number of crashes")

# Looking at the training flights
crash_data_v2 %>%
  filter(route == "Training") %>%
  summarise(training_survival = mean(survival_rate), mean_survival_rate, deviation = training_survival - mean_survival_rate)

#training flights are hard to compare to other routes because the other routes just don't have many crashes, however with
#a survival rate of 7% after 78 accidents you can probably conclude that training flights are pretty dangerous, it
#would be good to see what percentage of training flights actually crash but the data does not enable that.

#Investigating the presumed safety of planes "introduced" after the year 2000. due to the limitations of the data
#I can look at planes that had their first crash after the year 2000, which is an ok substitute.
#getting average survival rate for planes introduced after the year 2000

crash_data_v2 %>%
  group_by(ac_type) %>%
  filter(min(year(date)) >= 2000) %>%
  ungroup() %>%
  summarise("Average survival rate (after year 2000)" = mean(survival_rate),
            "Average survival_rate" = mean_survival_rate,
            "Deviation from mean" = mean(survival_rate) - mean_survival_rate)

average_survival_rate_2000 <- crash_data_v2 %>%
  group_by(ac_type) %>%
  filter(min(year(date)) >= 2000) %>%
  ungroup() %>%
  summarise(average_survival_rate = mean(survival_rate))
  
df2 <- data.frame(rate = c("Average survival rate (year >= 2000)", "Average survival rate"),
                  nums = c(average_survival_rate_2000$average_survival_rate*100, mean_survival_rate*100))
df2 %>% ggplot(aes(x = rate, y = nums)) + 
  geom_col(width = 0.5) +
  ylim(0, 25) +
  labs(title = "Average survival rate after the year 2000 vs Overall average survival rate",
       x = " ",
       y = "Average survival rate (%)")
