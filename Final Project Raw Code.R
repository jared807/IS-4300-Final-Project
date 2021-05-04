
library(tidyverse)
library(ggthemes)
library(rpart)


Spam <- "https://vincentarelbundock.github.io/Rdatasets/csv/DAAG/spam7.csv"
# Get the Spam Data from GitHub
spam <- read.csv(url(Spam))


## Introduction

names(spam) <- c("Email_No", "CAPS", "DollarSign", "ExclamationPoints", "Money", "TripleZeros", "Make", "SPAM")
# Here we renamed the variables in the spam dataset.
head(spam)
# Here we look at the top rows in the dataset.
spam <- as.data.frame(spam)
# This is done to prevent any possible errors in the future.

## Model 1 Visualization

plot1 <- spam %>% ggplot()
# Allows us to plot the data.
plot1 + geom_point(aes(ExclamationPoints,CAPS,col=SPAM),size = 0.5) + 
  ylim(0,2000) + 
  xlim(0, 1.5) +
  geom_hline(yintercept = 105, col= "black", size=.5) +
  geom_vline(xintercept = .15, col= "black", size=.5) +
  labs(title="Plot of Varibles ExclamationPoints & CAPS",
       x = "Number of ! Symbols Used",y="Total Lenght of Capital Words") +
  theme_clean()
# Here we plotted the variables ExclamationPoints and CAPS colored by SPAM with the splits that we see value in for a model.



## Model 1 Results

spam$predicted <- ifelse(spam$CAPS < 105,"n", 
                         ifelse(spam$ExclamationPoints < 0.15,"n",
                                "y"))
# Used an ifelse function to predict the classification based off of the ExclamationPoints variable.
T <- table(Predict = spam$predicted,Actual = spam$SPAM)
# Create a table showing the models results and the actual results side by side.
T


T <- as.vector(T)
# Accuracy of the prediction is (num. correctly predicted)/(total)
accuracy <- (T[1]+T[4])/(T[1]+T[2]+T[3]+T[4]) # correct/total
# Sensitivity is Pr(predict 1 given actually 1) = 
sensitivity <- T[4]/(T[3]+T[4])
# Specificity is Pr(predict 0 given actually 0) = 
specificity <- T[1]/(T[1]+T[2])

metric <- c("Accuracy","Sensitivity","Specificity")
value <- c(accuracy,sensitivity,specificity)


data.frame(Metric = metric,Value = round(value,3))
# Shows the metrics previously created.


## Model 2 Visualization 1


fit <- rpart(SPAM ~ CAPS + DollarSign + ExclamationPoints + Money + TripleZeros + Make, data = spam)
# This function pulls the variables and is able to make splits in the data that are useful in classification.
plot(fit,margin = .1)
text(fit,cex = .70,use.n = TRUE)
# These two function build the decision tree that was created in the rpart function.


## Model 2 Visualization 2

plot2 <- spam %>% ggplot()
# Allows us to plot the data.
plot2 + geom_point(aes(DollarSign,ExclamationPoints,col=SPAM),size = 1) + 
  geom_hline(yintercept = .0915, col= "black", size=.5) + 
  geom_vline(xintercept = .0555, col= "black", size=.5) +
  ylim(0,5) + 
  xlim(0,1.5) +
  labs(title =  "Plot of Varibles DollarSigns & CAPS",
       x = "Number of $ Symbols Used",y="Number of ! Symbols Used") +
  theme_clean()
# Here we plotted the DollarSign and ExclamationPoints variables colored by SPAM with the splits from the rpart function. This allows us to visualize what the function determined was best for creating a model.



## Model 2 Visualization 3

plot3 <- spam %>% ggplot()
# Allows us to plot the data.
plot3 + geom_point(aes(CAPS,ExclamationPoints,col=SPAM),size = 1) + 
  geom_hline(yintercept = .0915, col= "black", size=.5) + 
  geom_vline(xintercept = 85.5, col= "black", size=.5) +
  ylim(0,3) + 
  xlim(0,4000) +
  labs(title="Plot of Varibles CAPS & ExclamationPoints",
       y = "Number of ! Symbols Used", x="Total Lenght of Capital Words") +
  theme_clean()
# Here we plotted the CAPS and ExclamationPoints variables colored by SPAM with the splits from the rpart function. This allows us to visualize what the function determined was best for creating a model.



## Model 2 Results

spam$predicted <- ifelse(spam$DollarSign > .0555,"y",ifelse(spam$ExclamationPoints < .0915,"n",
                                                            ifelse(spam$CAPS < 85.5,"n", 
                                                                   "y")))
# Used an ifelse function to predict the classification based off of the DollarSign, ExclamationPoints, and CAPS variables.
T <- table(Predict = spam$predicted,Actual = spam$SPAM)
# Create a table showing the models results and the actual results side by side.
T


T <- as.vector(T)
# Accuracy of the prediction is (num. correctly predicted)/(total)
accuracy <- (T[1]+T[4])/(T[1]+T[2]+T[3]+T[4]) # correct/total
# Sensitivity is Pr(predict 1 given actually 1) = 
sensitivity <- T[4]/(T[3]+T[4])
# Specificity is Pr(predict 0 given actually 0) = 
specificity <- T[1]/(T[1]+T[2])

metric <- c("Accuracy","Sensitivity","Specificity")
value <- c(accuracy,sensitivity,specificity)


data.frame(Metric = metric,Value = round(value,3))
# Shows the metrics previously created.


## Conclusion


