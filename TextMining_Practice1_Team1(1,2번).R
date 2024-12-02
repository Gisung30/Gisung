### Course: Text Mining                #####
### Subject: 2024-2                    #####
### Title: Practice 01                 #####
### TEAM: 01                           #####    
### Member: 22000028 Juha Gwak         #####
###         22000283 Yejun Park        #####
###         22000383 GiSung Shin       #####    
############################################

install.packages("babynames")
library(babynames)

babynames<-na.omit(babynames)

###1-1

#Count the number of unique names
length(unique(babynames$name))

# There are 97,310 names. 

###1-2

# Count the number of unique names by gender
library(dplyr)

babynames %>%
  group_by(sex) %>%
  summarise(unique_names = n_distinct(name))

# The number of female names is higher.

###1-3

### Find the most popular male name in the year 2000
babynames %>%
  filter(year == 2000, sex == "M") %>%
  arrange(desc(n)) %>%
  head(1)

# The name "Jacob" is the most popular.

###1-4

# Find the least popular female name in the year 2000 and sort in descending alphabetical order
babynames %>%
  filter(year == 2000, sex == "F") %>%
  arrange(n, desc(name)) %>%
  head(1)

# The name "Zyra" is the least popular.

###1-5

# Code to display the most popular name for both males and females in each year
library(dplyr)

# Male
popular_male_names <- babynames %>%
  filter(sex == "M") %>%
  group_by(year) %>%
  filter(n == max(n)) %>%
  select(year, sex, name, n) %>%
  arrange(year)

# Female
popular_female_names <- babynames %>%
  filter(sex == "F") %>%
  group_by(year) %>%
  filter(n == max(n)) %>%
  select(year, sex, name, n) %>%
  arrange(year)

# Merge the male and female data frames
popular_names <- full_join(popular_male_names, popular_female_names, by = "year", suffix = c("_male", "_female"))

# Display the result
popular_names

###1-6

# Create a bar plot that compares the number of male and female babies by year using chain operations
library(ggplot2)

babynames %>%
  group_by(year, sex) %>%
  summarise(total = sum(n)) %>%
  ggplot(aes(x = year, y = total, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Number of Male and Female Babies by Year", x = "Year", y = "Number of Babies")

# Before 1930, there were more females, but after that, there are more males.

###1-7

# Create a bar plot that compares the number of male and female babies by century

babynames %>%
  mutate(decade = floor(year / 100) * 100) %>%
  group_by(decade, sex) %>%
  summarise(total = sum(n)) %>%
  ggplot(aes(x = as.factor(decade), y = total, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of Number of Male and Female Babies by Decade", x = "Decade", y = "Number of Babies")

# Looking at this graph, there were more females in the 1800s, but after that, there are more males.

###1-8

# Create a line plot comparing the usage trends of John and Jacob by year
babynames %>%
  filter(name %in% c("John", "Jacob"), sex == "M") %>%
  group_by(year, name) %>%
  summarise(total = sum(n)) %>%
  ggplot(aes(x = year, y = total, color = name)) +
  geom_line() +
  labs(title = "Comparison of Usage Trends of John and Jacob", x = "Year", y = "Number of Babies")

# John was used a lot in the past but is decreasing, while Jacob became popular before the 2000s and then started decreasing again.


### 2-1

# Install and load necessary packages
install.packages("dplyr")
library(dplyr)

# Load and merge files
file_paths <- list.files(path = ".", pattern = "uber-raw-data-.*\\.csv", full.names = TRUE)

uber.set <- file_paths %>%
  lapply(read.csv) %>%
  bind_rows() %>%
  filter(Date.Time != 0)  # Remove cases where Date.Time is 0

# Check the first 6 rows
head(uber.set)

### 2-2

# Set locale to English to match the example
Sys.setlocale("LC_TIME", "English")

# Check the current locale setting
Sys.getlocale("LC_TIME")

# Load necessary package
library(lubridate)

uber.set <- uber.set %>%
  mutate(
    # Keep the original Date.Time, and temporarily handle the converted value
    Date.Time.Converted = mdy_hms(Date.Time),                   # Convert Date.Time to date-time format
    Date = as.Date(Date.Time.Converted),                        # Create Date column
    Time = paste0(minute(Date.Time.Converted), "M ", second(Date.Time.Converted), "S"), # Create Time column
    year = year(Date.Time.Converted),                           # Create year column
    month = month(Date.Time.Converted, label = TRUE, abbr = TRUE),  # Create month column
    day = day(Date.Time.Converted),                             # Create day column
    dayday = wday(Date.Time.Converted, label = TRUE, abbr = TRUE),  # Create day of the week in abbreviated English
    hour = hour(Date.Time.Converted),                           # Create hour column
    minute = minute(Date.Time.Converted),                       # Create minute column
    second = second(Date.Time.Converted)                        # Create second column
  ) %>%
  select(-Date.Time.Converted)  # Remove temporary column

# Check the result
head(uber.set)

### 2-3

# Calculate Uber usage frequency by hour
uber_hourly <- uber.set %>%
  group_by(hour) %>%
  summarise(count = n())

# Create a graph
ggplot(uber_hourly, aes(x = hour, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Hour-wise Uber Usage", x = "Hour", y = "Number of Trips") +
  scale_y_continuous(labels = scales::comma) +  # Y-axis scaling
  theme_minimal()

# Uber is most frequently used between 4 PM and 9 PM in the evening.
# It is likely because people are either going home or enjoying leisure activities after work.

### 2-4

# Calculate Uber usage frequency by day of the week
uber_day <- uber.set %>%
  group_by(dayday) %>%
  summarise(count = n())

# Create a graph (difference by day of the week)
ggplot(uber_day, aes(x = dayday, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Uber Usage by Day of Week", x = "Day of Week", y = "Number of Trips") +
  scale_y_continuous(labels = scales::comma) +  # Y-axis scaling (add comma for thousands separator)+
  theme_minimal()

# Uber is most frequently used on Thursday or Friday, and less on Sunday.

# Calculate Uber usage frequency by day of the week and month
uber_day_month <- uber.set %>%
  group_by(dayday, month) %>%
  summarise(count = n())

# Create a graph (difference by day of the week and month)
ggplot(uber_day_month, aes(x = dayday, y = count, fill = month)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Uber Usage by Day of Week and Month", x = "Day of Week", y = "Number of Trips") +
  theme_minimal()

# Although the order of most frequent days varies by month, Sundays generally have the lowest frequency.

### 2-5

# Install and load necessary packages
install.packages("mapview")
library(mapview)

# Calculate the top 100 most visited locations
top_spots <- uber.set %>%
  group_by(Lat, Lon) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(100)

# Create an interactive map of the top 100 coordinates
map <- mapview(top_spots, xcol = "Lon", ycol = "Lat", crs = 4269, grid = FALSE)

# Display the map
map
