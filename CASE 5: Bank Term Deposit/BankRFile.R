### 1.) Import important packages
library(ggplot2)
library(dplyr)

### 2.) Upload Data
bank_data <- read.csv("~/Data Science/Data/bank.csv")
bank_data

### 3.) Dimensions of the data
dim(bank_data)

### 4.) Information about the data
str(bank_data)

### 5.) Checking for null values in the data
colSums(is.na(bank_data))

### 6.) Statistical Measures
summary(bank_data)

### 7.) Visualizing Age Feature
ggplot(bank_data, aes(age)) + geom_bar(fill="blue") +
  ggtitle("Histogram of Customers' Age")
ggplot(bank_data, aes(age, fill=loan)) + geom_bar() +
  ggtitle("Distribution of Customers with Loans bases on Age")

