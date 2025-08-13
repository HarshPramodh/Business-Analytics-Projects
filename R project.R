# install and load packages
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
library(readr)
library(ggplot2)
library(dplyr)

#1: Load the dataset "titanic_train" in R and inspect
train_data <- read_csv("C:/Users/Harsh Pramod/titanic_train.csv")

#2 Identify variables and check any missing values
str(train_data)
summary(train_data)
colSums(is.na(train_data))
train_data$Age[is.na(train_data$Age)] <- mean(train_data$Age, na.rm = TRUE)
train_data$Fare[is.na(train_data$Fare)] <- median(train_data$Fare, na.rm = TRUE)
most_common_embarked <- names(sort(table(train_data$Embarked), decreasing = TRUE))[1]
train_data$Embarked[is.na(train_data$Embarked)] <- most_common_embarked
colSums(is.na(train_data))
#3 Finding Descriptive Statistics
mean(train_data$Fare)
median(train_data$Pclass)
sd(train_data$PassengerId)
# Visualization
ggplot(train_data, aes(x = factor(Pclass))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Distribution of Pclass", x = "Pclass", y = "Count") 

ggplot(train_data, aes(x =factor(Survived))) +
  geom_bar( fill = "green", color = "black") +
  labs(title = "Distribution of fare rates", x = "Survived", y = "Count") 

ggplot(train_data, aes(x= reorder(Gender,-Fare), y=Fare))+
  geom_boxplot(fill="orange") +
  labs(title = "Total Fare by gender", x="Gender",y="Fare")
t_test_result <- t.test(Fare ~ Gender, data = train_data)
print(t_test_result)

