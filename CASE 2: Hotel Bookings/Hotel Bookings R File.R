### 1.) Import important libraries
library(dplyr)

### 2.) Reading the Data and the size of the dataset
hotel_demand <- read.csv("~/Data Science/Data/hotel_bookings.csv")
dim(hotel_demand)

### 3.) Getting the first 5 rows of the data
head(hotel_demand)

### 4.) Checking for NULL values.
is.null(hotel_demand)
# Since there is no NULL in the entire data frame, no need to check each column.

### 5.) Statistical Measures 
summary(hotel_demand)

### 6.) Outlier Detection
leadTime_summ <- summary(hotel_demand$lead_time)
leadTime_summ
boxplot(hotel_demand$lead_time, main="Lead-Time Boxplot", xlab="Lead-Time",
        ylab="Lead-Time", col='blue')

### 6.1) Outliers in Lead-Time:

# define a function to find outliers
FindOutliers <- function(data) {
  lowerq = quantile(data)[2]
  upperq = quantile(data)[4]
  iqr = upperq - lowerq 
  # we identify extreme outliers
  extreme.threshold.upper = (iqr * 1.5) + upperq
  extreme.threshold.lower = lowerq - (iqr * 1.5)
  result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
}

# use the function to find the outliers
lead_outliers <- FindOutliers(hotel_demand$lead_time)
lead_len <- length(lead_outliers)
lead_len

# What is this percentage as compared to the total number of values?
(lead_len/length(hotel_demand$lead_time))*100

### 6.2)  Compute summary statistic of the 'total_of_special_requests' column
spec_req_summ <- summary(hotel_demand$total_of_special_requests)
spec_req_summ

# Whisker PLot
boxplot(hotel_demand$total_of_special_requests, 
        main="Total of Special Requests Boxplot", col="red")
