### 1.) Import important packages
library(ggplot2)
library(dplyr)
library(DT)

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

### 8.) Visualizing Pdays Attribute
ggplot(bank_data, aes(pdays)) + geom_bar(fill='red') +
  ggtitle("Histogram of Pdays")
data = subset(bank_data, pdays <= 400)
ggplot(data, aes(pdays)) + geom_bar(fill='red') +
  ggtitle("New Histogram of Pdays ")

### 9.) Visualizing Duration Attribute
ggplot(bank_data, aes(duration)) + geom_bar(fill='purple') +
  ggtitle("Histogram of Duration")
ggplot(bank_data, aes(duration, fill=contact)) + geom_bar() +
  xlim(0, 2500) + ggtitle("Histogram of Contact based on Duration") 

### 10.) Contact Attribute
ggplot(bank_data, aes(contact, fill=contact)) + geom_bar() +
  scale_fill_manual("legend", 
                    values = c("cellular" = "red", "telephone" = "orange", 
                               "unknown" = "blue")) +
  ggtitle("Tally of Different Types of Contact")

### 11.) Previous Attribute
ggplot(bank_data, aes(previous, fill=contact)) + geom_bar() +
  ggtitle("Histogram of Previous based on Contact")
dplyr::count(bank_data, previous, sort= TRUE)

### 12.) Job Attribute
jobData <- bank_data %>% group_by(job) %>% 
  summarize(number=n())
datatable(jobData)
ggplot(jobData, aes(x=reorder(job,desc(number)), y=number, fill=job)) + 
  geom_bar(stat='identity') + ggtitle("Tally of the Jobs of the Customers") +
  theme(axis.text.x = element_text(angle=90))

### 13.) Education Attribute
eduData <- bank_data %>% group_by(education) %>%
  summarise(Total = n())
datatable(eduData)
ggplot(eduData, aes(x=reorder(education, desc(Total)), y=Total, 
                    fill=education)) + geom_bar(stat="identity") +
  ggtitle("Tally of the Level of Education Each Customer Have") +
  theme(axis.text.x = element_text(angle = 45))

eduDef <- bank_data %>% group_by(education, default) %>%
  summarise(Total=n(), .groups="drop")
datatable(eduDef)
ggplot(eduDef, aes(x=reorder(education, desc(Total)), y=Total,fill= default)) + 
         geom_bar(position='dodge',stat="identity") +
  ggtitle("Tally of Customers that Defaulted Based on Education") 

### 14.) Job Attribute
jobDef <- bank_data %>% group_by(job, default) %>%
  summarise(Total=n(), .groups='drop') 
datatable(jobDef)  
ggplot(jobDef, aes(x=reorder(job, desc(Total)), y= Total, fill=default)) +
  geom_bar(position = 'dodge', stat='identity') +
  ggtitle("Tally of Customers that Defaulted Based on Job") +
  theme(axis.text.x = element_text(angle = 45))

### 15.) Default Attribute
ggplot(bank_data, aes(default, fill=default)) + geom_bar() +
  scale_fill_manual("legend", values = c("no" = "red", "yes" = "orange")) +
  ggtitle("Tally of Default")
