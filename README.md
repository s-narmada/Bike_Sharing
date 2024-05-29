---
title: "Cyclistic Bike_Share"
author: "Narmada Singam"
date: "2023-12-05"
output:
  html_document: default
  pdf_document: default
---

# Overview

This case-study is a capstone project of the Google Data Analytics course offered by Coursera.

The goal of this project is to derive actionable insights to convert casual riders of the fictional Cyclistic Bike-Share company into annual members.


# Introduction
Cyclistic developed a successful bike-share program in 2016. Since then, the initiative has grown to include a fleet of 5,824 bicycles that are geotracked and locked into 692 stations throughout Chicago. Bikes can be unlocked at any station and returned to any other in the system at any time.

The director of marketing has a specific goal in mind: develop marketing methods that will convert casual riders into annual members. To do so, the marketing analyst team will need to learn more about the differences between yearly members and casual riders, why casual riders would buy a membership, and how digital media might alter their marketing strategies.

As a Data Analyst, the director has asked me to look at the Cyclistic historical bike trip data to see if there are any trends in the data that could inform the company’s marketing strategy.


#Business Task
Analyze Cyclistic’s historical bike trip data to spot trends, then apply what has been learned to figure out how annual members and casual riders use the bikes differently.



```{r}
# Set up R packages
install.packages("tidyverse")
library(tidyverse)  # Helps wrangle data
install.packages("lubridate")
library(lubridate)  # Helps wrangle date attributes
install.packages("ggplot2")
library(ggplot2)    # Helps visualize data
install.packages("ggthemes")
library(ggthemes)   # Helps configure dataviz theme
```


#Data Background
The dataset was acquired from https://divvytripdata.s3.amazonaws.com/index.html. Motivate International Inc made the data available under this license. For this project, I downloaded data for eleven months (January to November 2023). The zipped CSVs were downloaded and unzipped into a folder.

The chosen sample size contained 5,495,804 rows and 13 columns.


```{r}
#=====================
# STEP 1: COLLECT DATA
#=====================

td2301 <- read_csv("202301-divvy-tripdata.csv")
td2302 <- read_csv("202302-divvy-tripdata.csv")
td2303 <- read_csv("202303-divvy-tripdata.csv")
td2304 <- read_csv("202304-divvy-tripdata.csv")
td2305 <- read_csv("202305-divvy-tripdata.csv")
td2306 <- read_csv("202306-divvy-tripdata.csv")
td2307 <- read_csv("202307-divvy-tripdata.csv")
td2308 <- read_csv("202308-divvy-tripdata.csv")
td2309 <- read_csv("202309-divvy-tripdata.csv")
td2310 <- read_csv("202310-divvy-tripdata.csv")
td2311 <- read_csv("202311-divvy-tripdata.csv")

```


```{r}
#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================

# It appears that all datasets have the same column name
colnames(td2301)
colnames(td2302)
colnames(td2303)
colnames(td2304)
colnames(td2305)
colnames(td2306)
colnames(td2307)
colnames(td2308)
colnames(td2309)
colnames(td2310)
colnames(td2311)

# Inspect the dataframes and look for incongruencies
# It appears that all data types of each variable are consistent
str(td2301)
str(td2302)
str(td2303)
str(td2304)
str(td2305)
str(td2306)
str(td2307)
str(td2308)
str(td2309)
str(td2310)
str(td2311)

```


```{r}
# Stack individual monthly data frames into one big data frame
all_trips <- bind_rows(td2301, td2302, td2303, td2304, td2305, td2306, td2307, td2308, td2309, td2310,td2311)

# Remove lat, long, birthyear, and gender fields
all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

# Statistical summary of the data
summary(all_trips) 

# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)


```


Due to the size of the dataset, regular spreadsheet packages (Microsoft Excel or Google Sheets) would fail as they are limited to fewer number of rows.

Therefore, to analyse this data efficiently, R was chosen.


```{r}

#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(qs_raw)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# Drop duplicate data
all_trips <- all_trips[!duplicated(all_trips$ride_id),]

# Drop NA values by user type
all_trips <- drop_na(all_trips, member_casual)

# Inspect the structure of the cleaned data frame
str(all_trips)

```


```{r}
# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year
all_trips$date <- as.Date(all_trips$started_at)                # The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")       # Add a "month" column
all_trips$day <- format(as.Date(all_trips$date), "%d")         # Add a "day" column
all_trips$year <- format(as.Date(all_trips$date), "%Y")        # Add a "year" column
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A") # Add a "day of week" column

# Add a "ride_length" calculation to all_trips (in seconds)
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

```


```{r}
# Convert "ride_length" to numeric so we can run calculations on the data
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

```


```{r}
# Remove bad data when ride_length was negative
all_trips_v2 <- all_trips[!(all_trips$ride_length<0),]

# Drop NA values for ride_length
all_trips_v2 <- drop_na(all_trips_v2, ride_length)

# Inspect the structure of the newly cleaned data frame
str(all_trips_v2)

```



```{r}
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

# Descriptive analysis on ride_length (all figures in seconds)
summary(all_trips_v2$ride_length) # Find the shortest, midpoint, average, and longest trip

# Compare key statistics of casual users VS members
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

```



```{r}
# Summarise the average ride time by day for casual users VS members
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

```


```{r}
# analyse ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%                  #groups by usertype and weekday
  summarise(number_of_rides = n()                       #calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>%  # calculates the average duration
  arrange(member_casual, weekday)                       # sorts    

```


```{r}
# analyse ridership data by type and month
all_trips_v2 %>% 
  mutate(month_name = month(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, month_name) %>%                  #groups by usertype and weekday
  summarise(number_of_rides = n()                       #calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>%  # calculates the average duration
  arrange(member_casual, month_name)                       # sorts

```


```{r}
# Visualise the number of rides by day of the week
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
                    group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
    geom_col(position = "dodge") +
    labs(title = "Average number of rides per day", subtitle = "Jan - Nov 2023",
       x = "Day of the week", y = "Number of rides") +
    scale_fill_brewer(palette="Dark2") +
    theme_minimal()

```


```{r}
# Visualise the average trip duration by day of the week
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
    geom_col(position = "dodge") +
    labs(title = "Average trip duration per day", subtitle = "Jan - Nov 2023",
       x = "Day of the week", y = "Avg duration") +
    scale_fill_brewer(palette="Paired") +
    theme_minimal()

```



```{r}
# Visualise the number of rides by month
all_trips_v2 %>% 
  mutate(month_name = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month_name) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month_name)  %>% 
  ggplot(aes(x = month_name, y = number_of_rides, fill = member_casual)) +
    geom_col(position = "dodge") +
    labs(title = "Number of rides by month", subtitle = "Jan - Nov 2023",
       x = "Month", y = "Number of rides") +
    scale_fill_brewer(palette="Dark2") +
    theme_solarized()

```


```{r}
# Visualise the average trip duration by month
all_trips_v2 %>% 
  mutate(month_name = month(started_at, label = TRUE)) %>% 
  group_by(member_casual, month_name) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, month_name)  %>% 
  ggplot(aes(x = month_name, y = average_duration, fill = member_casual)) +
    geom_col(position = "dodge") +
    labs(title = "Average trip duration by month", subtitle = "Jan - Nov 2023",
       x = "Month", y = "Avg duration") +
    scale_fill_brewer(palette="Paired") +
    theme_solarized()

```

# Key Findings

1. Generally, annual members use Cyclistic more than casual riders.

2. There is significant usage of Cyclistic services in the summer by both casual and annual members.

3. Casual riders make use of Cyclistc mostly on weekends (more on Saturday) while annual members ride evenly for each day of the week.

4. Casual riders take longer rides than annual members.

5. Casual riders ride longer distances than annual members.

6. Both casual riders and members prefer classic bikes.


# Recommendations

1. To convert casual riders into annual members, marketing campaigns should be targeted towards summer when riders are more likely to patronise Cyclistic services.

2. The marketing team should also consider reaching out to riders during the weekends, as casual riders are most likely using Cyclistic’s services for recreational services while annual members use it for their daily commute.

