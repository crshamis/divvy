# STEP 0: LOAD PACKAGES AND SET WORKING DIRECTORY
# Load packages
library(tidyverse)  #helps import and wrangle data
library(lubridate)  #helps wrangle date attributes
library(dplyr) #helps with data manipulation
library(ggplot2)  #helps visualize data

# Set local working directory
getwd() #display working directory
setwd("~/Library/CloudStorage/GoogleDrive-crshamis@gmail.com/My Drive/1. Data Analyst/Coursera/Module 8/Cyclistic Case Study/R Directory") #set working directory to simplify calls to data


# STEP 1: COLLECT DATA
# Upload Divvy datasets (csv files) here
tripdata_202204 <- read.csv("202204-divvy-tripdata.csv")
tripdata_202205 <- read.csv("202205-divvy-tripdata.csv")
tripdata_202206 <- read.csv("202206-divvy-tripdata.csv")
tripdata_202207 <- read.csv("202207-divvy-tripdata.csv")
tripdata_202208 <- read.csv("202208-divvy-tripdata.csv")
tripdata_202209 <- read.csv("202209-divvy-tripdata.csv")
tripdata_202210 <- read.csv("202210-divvy-tripdata.csv")
tripdata_202211 <- read.csv("202211-divvy-tripdata.csv")
tripdata_202212 <- read.csv("202212-divvy-tripdata.csv")
tripdata_202301 <- read.csv("202301-divvy-tripdata.csv")
tripdata_202302 <- read.csv("202302-divvy-tripdata.csv")
tripdata_202303 <- read.csv("202303-divvy-tripdata.csv")


# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
# Before creating a master data frame, ensuring consistent column naming conventions
# Creating vectors of column names from each file
cols_202204 <- colnames(tripdata_202204)
cols_202205 <- colnames(tripdata_202205)
cols_202206 <- colnames(tripdata_202206)
cols_202207 <- colnames(tripdata_202207)
cols_202208 <- colnames(tripdata_202208)
cols_202209 <- colnames(tripdata_202209)
cols_202210 <- colnames(tripdata_202210)
cols_202211 <- colnames(tripdata_202211)
cols_202212 <- colnames(tripdata_202212)
cols_202301 <- colnames(tripdata_202301)
cols_202302 <- colnames(tripdata_202302)
cols_202303 <- colnames(tripdata_202303)

# Creating a data frame of column names to audit for consistency
compare_cols <- data.frame(
  td_202204 = cols_202204, 
  td_202205 = cols_202205, 
  td_202206 = cols_202206,
  td_202207 = cols_202207,
  td_202208 = cols_202208,
  td_202209 = cols_202209,
  td_202210 = cols_202210,
  td_202211 = cols_202211,
  td_202212 = cols_202212,
  td_202301 = cols_202301,
  td_202302 = cols_202302,
  td_202303 = cols_202303
  )
View(compare_cols)
# NOTE: Column names have been confirmed for consistency

# Stack individual month's data frames into a single data frame
ltm_trips <- bind_rows(
  tripdata_202204,
  tripdata_202205,
  tripdata_202206,
  tripdata_202207,
  tripdata_202208,
  tripdata_202209,
  tripdata_202210,
  tripdata_202211,
  tripdata_202212,
  tripdata_202301,
  tripdata_202302,
  tripdata_202303
)


# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
# Inspecting the new table that has been created
colnames(ltm_trips) # list of column names 
dim(ltm_trips)  # dimensions of data frame (#columns & #rows)
head(ltm_trips)  # inspecting first 6 rows
str(ltm_trips)  # inspecting structure of data frame (column names & data type)
summary(ltm_trips)  # summary statistics
## NOTE: There are 5855 Null values for end_lat and end_lng as well as observations with a value of 0

# Determining how many distinct values exist for each variable
distinct_counts <- sapply(ltm_trips, function(x) length(unique(x)))
result_counts <- data.frame(variable = names(distinct_counts), distinct_values = distinct_counts)
View(result_counts)
## NOTE: There are 65-70K more distinct values for starting coordinates than 
## ending coordinates, which may require further investigation

# Removing rows where starting & ending lats are = 0 or Null
# Dropping the start station and end station name and ID columns, irrelevant to analysis
ltm_trips_clean <- ltm_trips %>%
  filter(!is.na(end_lat) & !is.na(end_lng) & end_lat != 0 & end_lng != 0) %>%
  select(-start_station_name, -start_station_id, -end_station_name, -end_station_id)

# Adding columns for month, day and year to aggregating data for analysis
ltm_trips_clean$date <- as.Date(ltm_trips_clean$started_at) #The default format is yyyy-mm-dd
ltm_trips_clean$month <- format(as.Date(ltm_trips_clean$date), "%m")
ltm_trips_clean$day <- format(as.Date(ltm_trips_clean$date), "%d")
ltm_trips_clean$year <- format(as.Date(ltm_trips_clean$date), "%Y")
ltm_trips_clean$day_of_week <- format(as.Date(ltm_trips_clean$date), "%A")

# Adding calculated field for trip_duration & removing values less than or equal to zero
ltm_trips_clean$trip_duration <- difftime(ltm_trips_clean$ended_at,ltm_trips_clean$started_at, units = "mins")

# Inspecting the structure of the data frame
str(ltm_trips_clean)

# Converting "trip_duration" and "distance_miles" from Factor to numeric so we can run calculations on the data
ltm_trips_clean$trip_duration <- as.numeric(as.character(ltm_trips_clean$trip_duration))
is.numeric(ltm_trips_clean$trip_duration)

# Running summary statistics to determine any possible exclusions
summary(ltm_trips_clean)

#Note:  Some problems stand out from the summary statistics
# (1) Min trip_duration is negative - remove all negative observations
# (2) Min distance_miles is zero - remove all observations = 0

ltm_trips_v2 <- ltm_trips_clean %>% 
  filter(trip_duration > 0)

nrow(ltm_trips_clean) - nrow(ltm_trips_v2)
# Note: 310,819 observations were removed by this filter

summary(ltm_trips_v2)

#################################
# STEP 4: DESCRIPTIVE ANALYSIS
#################################
# Descriptive analysis on ride_length (all figures in minutes)
mean(ltm_trips_v2$trip_duration) #straight average (total ride length / rides)
median(ltm_trips_v2$trip_duration) #midpoint number in the ascending array of ride lengths
max(ltm_trips_v2$trip_duration) #longest ride
min(ltm_trips_v2$trip_duration) #shortest ride

# Summary stats for trip_duration
summary(ltm_trips_v2$trip_duration)

# Summary stats for distance_miles
summary(ltm_trips_v2$distance_miles)

# Comparing members and casual users for trip_duration
aggregate(ltm_trips_v2$trip_duration ~ ltm_trips_v2$member_casual, FUN = mean)
aggregate(ltm_trips_v2$trip_duration ~ ltm_trips_v2$member_casual, FUN = median)
aggregate(ltm_trips_v2$trip_duration ~ ltm_trips_v2$member_casual, FUN = max)
aggregate(ltm_trips_v2$trip_duration ~ ltm_trips_v2$member_casual, FUN = min)

# Comparing members and casual users for distance_miles
aggregate(ltm_trips_v2$distance_miles ~ ltm_trips_v2$member_casual, FUN = mean)
aggregate(ltm_trips_v2$distance_miles ~ ltm_trips_v2$member_casual, FUN = median)
aggregate(ltm_trips_v2$distance_miles ~ ltm_trips_v2$member_casual, FUN = min)
aggregate(ltm_trips_v2$distance_miles ~ ltm_trips_v2$member_casual, FUN = max)

# See the average ride time by each day for members vs casual users
ltm_trips_v2$day_of_week <- ordered(ltm_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(ltm_trips_v2$trip_duration ~ ltm_trips_v2$member_casual + ltm_trips_v2$day_of_week, FUN = mean)

# Visualizing the number of rides by rider type for each day of the week
ltm_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarize(number_of_rides = n()
            ,average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Visualizing the average duration for each rider type for each day of the week
ltm_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# Visualizing number of rides by membership type arranged by month
ltm_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarize(number_of_rides = n()
            ,average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Visualizing average trip duration by membership type arranged by month 
ltm_trips_v2 %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(trip_duration)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
# Exporting to CSV for replicating visualization in Excel or Tableau
# Average trip duration for each day of the week 
dow_averages <- aggregate(ltm_trips_v2$trip_duration ~ ltm_trips_v2$member_casual + ltm_trips_v2$day_of_week, FUN = mean)
write.csv(dow_averages, file = 'ltm_trip_dow_avg.csv')

# Total number of rides for each day of the week
dow_sums <- aggregate(ltm_trips_v2$trip_duration ~ ltm_trips_v2$member_casual + ltm_trips_v2$day_of_week, FUN = length)
write.csv(dow_sums, file = 'ltm_trip_dow_sums.csv')

# Average trip duration for each month of the year
monthly_averages <- aggregate(ltm_trips_v2$trip_duration ~ ltm_trips_v2$member_casual + ltm_trips_v2$month, FUN = mean)
write.csv(monthly_averages, file = 'ltm_trip_monthly_avg.csv')

# Total number of rides for each month of the year
montly_sums <- aggregate(ltm_trips_v2$trip_duration ~ ltm_trips_v2$member_casual + ltm_trips_v2$month, FUN = length)
write.csv(montly_sums, file = 'ltm_trip_monthly_sums.csv')