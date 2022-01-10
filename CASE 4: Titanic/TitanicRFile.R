### 1.) Import Libraries
library(ggplot2)
library(dplyr)
library(DT)

### 2.) Upload Data
titanic_data <- read.csv("~/Data Science/Data/titanic.csv")
titanic_data

### 2.5) Changing Age attribute to adults = 1, minors = 0
s <- matrix(nrow=nrow(titanic_data), ncol=1)
for (i in 1:nrow(titanic_data)) {
  if (titanic_data$Age[i] > 18) s[i,] <- 1
  else s[i] <- 0
}

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

### 8.) Calculate the total number of passengers:
total <- tally(titanic_data)

### 8.1) Calculate the total proportion of passengers surviving.
Survivors <- subset(titanic_data, Survived == 1)
totalSurvivors <- tally(Survivors)
(totalSurvivors/total)*100

### 8.2) Calculate the proportion of passengers surviving for 
# each class of passenger.
classSurv <- titanic_data %>% group_by(Pclass, Survived) %>%
  summarize(number = n(), .groups="drop") %>%
  mutate(perc = (number / sum(number))*100)
datatable(classSurv)

### 8.3) Calculate the proportion of passengers surviving for 
# each sex category. Which sex had the highest survival rate?
sexSurv <- titanic_data %>% group_by(Sex, Survived) %>%
  summarise(number = n(), .groups = "drop") %>%
  mutate(perc = (number/sum(number))*100)
datatable(sexSurv)

### 8.4) Calculate the proportion of passengers surviving for 
# each age category. Which age had the lowest survival rate?
titanic_data$age <- s
ageSurv <- titanic_data %>% group_by(age, Survived) %>%
  summarise(number= n(), .groups="drop") %>%
  mutate(perc = (number/sum(number))*100)
datatable(ageSurv)

### 8.5) Calculate the proportion of passengers surviving for 
# each age/sex category (i.e., for adult males, child males, 
# adult females, child females). Which group was most likely 
# to survive? Least likely?
agesexSurv <- titanic_data %>% group_by(age, Sex, Survived) %>%
  summarise(number= n(), .groups = 'drop') %>%
  mutate(perc = (number/sum(number))*100)
datatable(agesexSurv)

### 8.6) Calculate the proportion of passengers surviving for each 
# age/sex/class category. Which group had the highest mortality in 
# this disaster. Why?
survivors1 <- filter(titanic_data, Pclass == 1)
agesexClass1 <- survivors1 %>% group_by(age, Sex, Survived) %>%
  summarise(number=n(), .groups = "drop") %>%
  mutate(perc = (number/sum(number))*100)
datatable(agesexClass1)

survivors2 <- filter(titanic_data, Pclass == 2)
agesexClass2 <- survivors2 %>% group_by(age, Sex, Survived) %>%
  summarise(number=n(), .groups = "drop") %>%
  mutate(perc = (number/sum(number))*100)
datatable(agesexClass2)

survivors3 <- filter(titanic_data, Pclass == 3)
agesexClass3 <- survivors3 %>% group_by(age, Sex, Survived) %>%
  summarise(number=n(), .groups = "drop") %>%
  mutate(perc = (number/sum(number))*100)
datatable(agesexClass3)
