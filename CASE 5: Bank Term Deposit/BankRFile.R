### 1.) Import important packages

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
