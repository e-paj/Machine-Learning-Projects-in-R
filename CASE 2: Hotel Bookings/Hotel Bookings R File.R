### 1.) Import important libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(DT)
library(reshape2)

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
meltData <- melt(hotel_demand)
ggplot(meltData, aes(factor(variable), value)) + 
  geom_boxplot() + facet_wrap(~variable, scale="free")

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

# Percentage out of total length
(weekend_len/length(hotel_demand$stays_in_weekend_nights))*100
# Since this has low percentage of outliers, we can rid of the rows that are outliers
# first save the outliers in a vector
outliers <- boxplot(hotel_demand$stays_in_weekend_nights, plot=FALSE)$out
new_hotel_bookings <- 
  hotel_demand[-which(hotel_demand$stays_in_weekend_nights %in% outliers),]
dim(new_hotel_bookings)

# Visualize 'stays_in_weekend_nights' column again

boxplot(new_hotel_bookings$stays_in_weekend_nights,
        main="New Stay in Weekend Nights Boxplot w/o Outliers", col='purple')
# Have we removed all outliers?
outliers_weekend <- FindOutliers(new_hotel_bookings$stays_in_weekend_nights)
length(outliers_weekend)

### 7.) DATA VISUALIZATION

### 7.1) How much in advance are the customers booking before their arrival?
ggplot(new_hotel_bookings, aes(x=arrival_date_year, y=lead_time, 
                               fill=arrival_date_year)) + 
  geom_bar(stat="identity") + ggtitle("Arrival by Year and Lead Time")
 
### 7.2) The above visualization does not give us a lot of information, so we can 
# divide the x-axis into months instead of years 
by_months <- new_hotel_bookings %>% 
  group_by(arrival_date_month, arrival_date_year) %>%
  dplyr::summarise(across(c(adults, children), sum), .groups = "keep")
by_month <- paste(by_months$arrival_date_month, 
                  by_months$arrival_date_year, sep=",")
by_months$Date <- by_month
by_months <- subset(by_months, select = c("Date", "adults","children"))
by_months.long <- melt(by_months)
ggplot(by_months.long, aes(Date, value, fill=variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Adults/Children per Month, Year") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  
### 7.3) Contrast the number of booking changes in the city hotel 
# to that in the resort hotel 
by_hotel <- new_hotel_bookings %>% group_by(hotel) %>%
  dplyr::summarise(across(c(required_car_parking_spaces), sum))
ggplot(by_hotel, aes(x="",y=required_car_parking_spaces, fill=hotel)) +
  geom_bar(stat="identity",width = 1)+
  ggtitle("Required Car Parking Spaces")+
  coord_polar("y", start=0)

### 7.4) Contrast the arrival timings of adults with children or babies with 
# that of adults without children or babies

# Finding out the values rows with and without children
without_children <- subset(new_hotel_bookings, children == 0 & babies == 0)
with_children <- subset(new_hotel_bookings, children !=0 & babies != 0)

# Calculating what percentage of the total values they form
without_children_count<- dplyr::count(without_children, arrival_date_month, 
                                      sort = FALSE)
without_children_perc <- 
  (without_children_count$n/sum(without_children_count$n))*100
with_children_count <- dplyr::count(with_children, arrival_date_month, 
                                    sort = FALSE)
with_children_perc <- 
  (with_children_count$n/sum(with_children_count$n))*100

# Plotting the two bar plots
without_children_data <- data.frame(without_children_count$arrival_date_month, 
                                without_children_perc)
colnames(without_children_data) <- c("Month", "Percentage")
with_children_data <- data.frame(with_children_count$arrival_date_month, 
                                 with_children_perc)
colnames(with_children_data) <- c("Month", "Percentage")
ggplot(without_children_data, aes(Month, Percentage, fill=Month)) +
  geom_bar(stat='identity')+
  ggtitle("Without Children")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggplot(with_children_data, aes(Month, Percentage, fill=Month)) +
  geom_bar(stat='identity')+
  ggtitle("With Children")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

### 7.5) Plot the number of guests arriving from different countries
by_country <- new_hotel_bookings %>% group_by(country) %>%
  dplyr::summarise(Total = n()) %>% arrange(desc(Total), .by_group=TRUE)
ggplot(by_country, aes(reorder(country, -Total), Total, fill=country))+
  geom_bar(stat="identity")+
  ggtitle("Bar Plot of Countries")

# Make a pie chart instead of top 20 countries
by_country_20 <- by_country[1:20,]
ggplot(by_country_20, aes(country, Total, fill=country))+
  geom_bar(stat="identity")+
  coord_polar(theta="y") +
  ggtitle("Pie Chart of Top 20 Countries")

### 7.6) Plot the mean of lead_time bookings that have been canceled
lead_cancel_data_10 <- new_hotel_bookings %>% group_by(lead_time) %>%
  summarise(as_tibble(rbind(summary(is_canceled))))
datatable(lead_cancel_data_10)
x <- lead_cancel_data_10$lead_time
y <- (lead_cancel_data_10$Mean)*100
plot(x, y, main="Eff", xlab="lead_time", ylab="Mean*100", pch=19)
abline(lm(y~x), col="blue")
