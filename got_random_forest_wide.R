#import libraries
library(tidyverse)
library(ggplot2)
library(randomForest)


#import csv
got_data_wide_predictive <- read.csv("got_data_wide_predictive.csv", header = TRUE)

#Primitive predictive model with data in tall form
training_data_wide <- got_data_wide_predictive[1:41,]
test_data_wide <- got_data_wide_predictive[42:67, ]

got_RF_wide <- randomForest(overall_rating ~ . , data = training_data_wide, type = "classification")

plot(got_RF_wide)

round(importance(got_RF_wide), 2)

pred<-predict(got_RF_wide,test_data_wide) #Predictions on Test 
pred
plot(pred) 

pred.df <- as.data.frame(pred)
actual <- test_data_wide$overall_rating
episode_n <- data.frame(id = 42:67)
actual_vs_pred <- cbind.data.frame(episode_n, actual, pred.df)

ggplot(data = actual_vs_pred) + 
  geom_point(aes(x = id,  y=actual), color = "green", size = 5) +
  geom_point(aes(x = id, y=pred), color = "red", size = 5) +
  labs(title = "Actual vs. Predicted Ratings",
       x = "Episode Number",
       y = "Rating") +
  theme(plot.title = element_text(hjust = 0.5))
  