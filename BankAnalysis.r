#Read in the data
library(readr)
library(MASS)

install.packages("gdata")
library(gdata)

data <- read_csv("bank_accounts_test.csv")
head(data)
str(data)

class(data)
sapply(data, class)
list(data)

#######################################
# Data cleaning and preparation
#######################################

data <- unknownToNA(data, unknown = "Unknown")
colMeans(is.na(data))*100  # Percentage of Null values in data

# Categorical => Factor
data$Gender <- factor(data$Gender)
data$Education_Level <- factor(data$Education_Level)
data$Marital_Status  <- factor(data$Marital_Status)
data$Card_Category   <- factor(data$Card_Category )


#######################################
# Describe the data, measure and visualize the most relevant relations
#######################################

barplot(table(data$Gender))
barplot(table(data$Education_Level))
barplot(table(data$Marital_Status))
barplot(table(data$Card_Category))

summary(data)

qplot(Months_on_book, data = data, geom = "histogram",fill=Education_Level , bins = 30)
qplot(Credit_Limit, data = data, geom = "histogram",fill=Marital_Status)
qplot(Months_on_book, data = data, geom = "density",col=Education_Level)
qplot(Credit_Limit, data = data, geom = "density",col=Card_Category)
