### 1.) Import Libraries
library(ggplot2)
library(dplyr)
library(DT)

### 2.) Upload Data
titanic_data <- read.csv("~/Data Science/Data/titanic.csv")
titanic_data

### 3.) Dimensions of the data
dim(titanic_data)
# there are 887 entries and 8 attributes

### 4.) Information about the data
str(titanic_data)

### 5.) Checking for null values in the data
colSums(is.na(titanic_data))

### 6.) Statistical Measures
summary(titanic_data)

### 7.) Visualizing Distribution
ggplot(titanic_data, aes(Survived)) + geom_bar(aes(fill=Survived)) +
  ggtitle("Passengers Survived") 
ggplot(titanic_data, aes(Age)) + geom_bar(aes(fill=Age)) +
  ggtitle("Distribution of Age of Passengers")
ggplot(titanic_data, aes(Pclass)) + geom_bar(aes(fill=Pclass)) +
  ggtitle("Distribution of Class") 
ggplot(titanic_data, aes(Sex)) + geom_bar() +
  ggtitle("Number of Females/Males")
