#import libraries
library(tidyverse)
library(ggplot2)
library(randomForest)


#import csv
got_data <- read.csv("data.csv", header = TRUE)

summary(got_data)

got_data <- got_data %>% unite(episode_id, "season", "episode", remove = FALSE)

#norm function
my_norm <- function(x){(x-min(x))/(max(x)-min(x))}


#cummulative screentime
cummulative_screentime <- got_data %>%
  group_by(character) %>%
  summarise(sum = sum(screen.time))

#changing variable structure
got_data$character <- as.character(got_data$character)
got_data$episode_id <- as.character(got_data$episode_id)
cummulative_screentime$character <- as.character(cummulative_screentime$character)

#joining cummulative screen time with overall data by character
got_data_2 <- left_join(got_data, cummulative_screentime, by = "character")

#creating a new variable that is screen time and rating multipled together
got_data_2 <- got_data_2 %>%
  mutate(rating_st = got_data_2$overall_rating * got_data_2$screen.time* got_data_2$sum)

#new column with normalized values per episode
got_data_2 <- got_data_2 %>%
  group_by(episode_id) %>%
  mutate(rating_norme = my_norm(rating_st))

#Creating new Weighted rating
got_data_2 <- got_data_2 %>%
  mutate(weighted_rating = rating_norme*10)

#new data frame with charater and weighted_rating averaged
mean_rating_weighted <- got_data_2 %>%
  group_by(character) %>%
  summarise(avr = mean(weighted_rating))

got_data_2 <- got_data_2 %>%
  group_by(character) %>%
  mutate(avr = mean(weighted_rating))

#getting top 35 character
top_characters <- filter(mean_rating_weighted, mean_rating_weighted$avr > "0.23")

#plotting weighted ratings averages
  ggplot(top_characters) + geom_col(aes( x = character, y = avr, fill = avr), color = "grey45")+
  coord_flip() + 
  labs(title = "Top 35 Characters and Normalized Average Episode Rating",
       y = "Weighted Average Rating",
       x= "") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.5))


#getting Charcterid 
numbers <- data.frame(id = 1:555)
characters <- data.frame(character = unique(got_data$character))
character_id <- cbind(numbers, characters)
got_data_2 <- inner_join(got_data_2,character_id, by = "character")

#tall_predictive
got_data_tall_predictive <- subset(got_data_2, select=c("overall_rating", 
                                                        "screen.time","id", "total_votes", "weighted_rating"))

got_data_tall_predictive <- as.data.frame(got_data_tall_predictive)

#Primitive predictive model with data in tall form
training_data <- got_data_tall_predictive[1:1783,]
test_data <- got_data_tall_predictive[1784:3017,]

got_RF <- randomForest(overall_rating ~ . , data = training_data, type = "classification")
got_RF
plot(got_RF)  

round(importance(got_RF), 2)

pred<-predict(got_RF,test_data) #Predictions on Test 
pred


test.df <- data.frame("screen.time"= c(10), "id"= c(404), "avr" = (0.01289660228)) 

pred<-predict(got_RF,test.df) #Predictions on Test for Wun being in Episode for 10 
#predicts that show will receive a rating 9.5


round(importance(got_RF), 2)

pred<-predict(got_RF,test_data) #Predictions on Test 
pred

actual_vs_pred <- cbind(pred, test_data)


ggplot(data = actual_vs_pred) + 
  geom_point(aes(x = id,  y=overall_rating), color = "green", size = 5) +
  geom_point(aes(x = id, y=pred), color = "red", size = 5) +
  labs(title = "Actual vs. Predicted Ratings",
       x = "Episode Number",
       y = "Rating") +
  theme(plot.title = element_text(hjust = 0.5))

# "season", "episode", "overall_rating","screen.time", "weighted_rating", "id", "total_votes"
#   20         3             --               1000             --             2,     --
test.df <- data.frame("screen.time"= c(10), "id"= c(404))

pred<-predict(got_RF,test.df) #Predictions on Test 
