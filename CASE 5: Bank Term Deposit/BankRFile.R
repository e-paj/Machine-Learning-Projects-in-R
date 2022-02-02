### 1.) Import important packages
library(ggplot2)
library(dplyr)
library(DT)
library(fastDummies)
library(cluster)
library(factoextra)
library(corrplot)
library(gmodels)

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

### 16.) Deposit Attribute
month_dep <- bank_data %>% group_by(month, deposit) %>%
  summarise(Total = n(),.groups="drop") %>%
  mutate(perc=(Total/sum(Total))*100)
datatable(month_dep)
ggplot(month_dep, aes(x=month, y=Total, fill=deposit)) +
  geom_bar(position = "dodge", stat="identity") +
  ggtitle("Tally of Customers that Made a Deposit Each Month")

### 17.) Month & Age Attribute

# Graph Employee Satisfaction
ggplot(bank_data, aes(month)) + 
  geom_bar(fill="coral") +
  ggtitle('Months of Marketing Activity Distribution') +
  labs(y='Potential Clients Count', x= "Month")

# Graph Employee Evaluation
ggplot(bank_data, aes(age)) + geom_bar(fill="sky blue") +
  ggtitle("Age of Potentical Clients Distribution") + 
  labs(y='Potential Clients Count')

# Campaigns
ggplot(bank_data, aes(campaign)) + geom_bar(fill="gray") +
  ggtitle('Calls Received in the Marketing Campaign') +
  labs(y='Potential Clients Count')

### 18.) Different Aspects of the Analysis
months_example <- CrossTable(bank_data$deposit, bank_data$month)

# Gives the proportion of subscribed term deposit
months_example$prop.col

# Gives the amount of suscribed vs non-suscribed term deposits accounts per month.
months_example

# We need to take the values into a list format 
nodeposit <- c(202, 335, 28, 346, 1899, 676, 887, 831, 50, 69, 540, 10)
deposit <- c(142, 441, 248, 577, 925, 546, 627, 688, 269, 323, 403, 100)
all <- c(344, 776, 276, 923, 2824, 1222, 1514, 1519, 319, 392, 943, 110)

labels <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
          'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
values <- data.frame(labels, nodeposit, deposit, all)

ggplot(values, aes(x="",y=all, fill=labels)) + geom_bar(stat='identity') + 
  ggtitle("# of Potential Clients Targeted per Month") +
  coord_polar("y",start=0) +
  geom_text(aes(label=all), position = position_stack(vjust = 0.5))

### 18.) Get Dummies for Attributes
bank_new <- dummy_cols(bank_data, select_columns = c("job","marital","education",
                                                     "default","housing","loan",
                                                     "contact","month","poutcome",
                                                     "deposit"))
new_bank <- bank_new %>% select(-c("job","marital","education","default",
                                      "housing","loan","contact","month",
                                      "poutcome","deposit"))
dim(new_bank)

### 19.) Scaling new data
Sc_bank <- scale(new_bank)

### 20.) Correlation between Attributes
corr <- cor(Sc_bank, method="pearson")
corrplot(corr, method="color", order="alphabet")

### 20.) Optimal # of Clusters
set.seed(123)
fviz_nbclust(Sc_bank, kmeans, method = "wss")
# Elbow is at 2.

### 21.) KMeans Algorithm with k=2
set.seed(123)
Md1 <- kmeans(Sc_bank, 2, nstart=25)
print(Md1)
fviz_cluster(Md1, data=Sc_bank)
