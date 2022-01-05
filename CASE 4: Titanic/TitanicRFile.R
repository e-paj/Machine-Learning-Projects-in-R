### 1.) Import Libraries


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
