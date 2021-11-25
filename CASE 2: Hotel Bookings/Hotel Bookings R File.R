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
