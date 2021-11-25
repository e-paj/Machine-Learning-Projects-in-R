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
summary(hotel_demand$lead_time)
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
summary(hotel_demand$total_of_special_requests)
boxplot(hotel_demand$total_of_special_requests, 
        main="Total of Special Requests Boxplot", col="red")

# use function to find the outliers
spec_outliers <- FindOutliers(hotel_demand$total_of_special_requests)
spec_len <- length(spec_outliers)
spec_len

# Percentage out of total length
(spec_len/length(hotel_demand$total_of_special_requests))*100

### 6.3) Compute summary statistic of the 'stays_in_weekend_nights' column
summary(hotel_demand$stays_in_weekend_nights)
boxplot(hotel_demand$stays_in_weekend_nights, 
        main="Stays in Weekend Nights Boxplot", col="green")

# use function to find outliers
weekend_outliers<- FindOutliers(hotel_demand$stays_in_weekend_nights)
weekend_len <- length(weekend_outliers)
weekend_len

# Percentage out of toal length
(weekend_len/length(hotel_demand$stays_in_weekend_nights))*100
# Since this has low percentage of outliers, we can rid of the rows that are outliers
index <- vector(mode="list", length=265)
for (i in weekend_outliers){
  index[i] <- which(hotel_demand$stays_in_weekend_nights == weekend_outliers[i])
}
index
  
