### 1.) Import important packages
library(ggplot2) # for data visualization
library(ggthemes) # an add-on for ggplot2
library(lubridate) # make dealing with Dates a little easier
library(dplyr) # for data manipulation
library(tidyr) # to tidy the data
library(DT) # provides an R interface to the JavaScript library
library(scales) # to map the data to the correct scales 

### 2.) Create vector colors to be implemented
colors <- c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840",
            "#0683c9", "#e075b0")

### 3.) Reading the Data into their designated variables
apr_data <- read.csv("~/Data Science/Data/UBER/uber-raw-data-apr14.csv")
may_data <- read.csv("~/Data Science/Data/UBER/uber-raw-data-may14.csv")
jun_data <- read.csv("~/Data Science/Data/UBER/uber-raw-data-jun14.csv")
jul_data <- read.csv("~/Data Science/Data/UBER/uber-raw-data-jul14.csv")
aug_data <- read.csv("~/Data Science/Data/UBER/uber-raw-data-aug14.csv")
sep_data <- read.csv("~/Data Science/Data/UBER/uber-raw-data-sep14.csv")

data_2014 <- rbind(apr_data, may_data, jun_data, jul_data,
                   aug_data, sep_data)

# converting the format of the Date.Time column to a specific type
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, 
                                  format = "%m/%d/%Y %H:%M:%S")


data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, 
                                    format = "%m/%d/%Y %H:%M:%S"),
                         format = "%H:%M:%S")

# Parse dates that have hours, minutes, or seconds elements
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label=TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label=TRUE))
data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

### 4.) Plotting the trips by the hours of the day

# The ggplot function will be used to plot the number of trips that the 
# passengers had made in a day. dplyr will be use to aggregate the data.
hour_data <- data_2014 %>% group_by(hour) %>% dplyr::summarize(Total = n())
datatable(hour_data)

ggplot(hour_data, aes(hour, Total)) + 
  geom_bar(stat="identity", fill="steelblue", color="red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels=comma)

month_hour <- data_2014 %>% 
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

ggplot(month_hour, aes(hour, Total, fill=month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels=comma)

### 5.) PLotting data by trips during every day of the month

day_group <- data_2014 %>% 
  group_by(day) %>%
  dplyr::summarize(Total = n())
datatable(day_group)

ggplot(day_group, aes(day, Total)) +
  geom_bar(stat="identity", fill='blue') +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels=comma)

day_month_group <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())

ggplot(day_month_group, aes(day, Total, fill=month)) +
  geom_bar(stat="identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values=colors)

### 6.) Number of trips taking place during months in a year
# Visualize the number of trips that are taking place each month 
# of the year. 

month_group <- data_2014 %>%
  group_by(month) %>%
  dplyr::summarize(Total = n())
datatable(month_group)

ggplot(month_group, aes(month, Total, fill=month)) +
  geom_bar(stat="identity") +
  ggtitle("Trips by Month") +
  theme(legend.position = "none") +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values=colors)

month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarise(Total = n())

ggplot(month_weekday, aes(month, Total, fill=dayofweek)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels=comma) +
  scale_fill_manual(values=colors)

### 7.) Finding the number of Trips by bases
# Plotting the number of trips that have been taken by the 
# passengers from each of the base.
ggplot(data_2014, aes(Base)) +
  geom_bar(fill='darkred') +
  scale_y_continuous(labels=comma) +
  ggtitle("Trips by Bases")

ggplot(data_2014, aes(Base, fill=month)) +
  geom_bar(position="dodge") +
  scale_y_continuous(labels=comma) +
  ggtitle("Trips by Bases and Month") +
  scale_fill_manual(values=colors)

ggplot(data_2014, aes(Base, fill=dayofweek)) +
  geom_bar(position="dodge") +
  scale_y_continuous(labels=comma) +
  ggtitle("Trips by Bases and DayofWeek") +
  scale_fill_manual(values=colors)

### 8.) Creating a Heat Map visualization of day, hour, & month

day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarise(Total= n())
datatable(day_and_hour)

ggplot(day_and_hour, aes(day, hour, fill=Total)) +
  geom_tile(color="white") +
  ggtitle("Heat Map by Hour and Day")

ggplot(day_month_group, aes(day, month, fill=Total)) +
  geom_tile(color="white") +
  ggtitle("Heat Map by Month and Day")

ggplot(month_weekday, aes(dayofweek, month, fill=Total)) +
  geom_tile(color="white")+
  ggtitle("Heat Map by Month and Day of Week")

month_base <- data_2014 %>%
  group_by(Base, month) %>%
  dplyr::summarise(Total=n())

dayofweek_bases <- data_2014 %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarise(Total =n())

ggplot(month_base, aes(Base, month, fill=Total)) +
  geom_tile(color="white") +
  ggtitle("Heat Map by Month and Bases")

ggplot(dayofweek_bases, aes(Base, dayofweek, fill=Total)) +
  geom_tile(color="white") +
  ggtitle("Heat Map by Bases and Day of Week")

### 9.) Creating a map visualization or ides in NY
# Visualize the rides in NYC by creating a geo-plot during 2014
# and the bases in the same period.

min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(data_2014, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")
