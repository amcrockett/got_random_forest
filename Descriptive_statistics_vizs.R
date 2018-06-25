#import libraries
library(tidyverse)
library(ggplot2) 


#impoort csv
got_data <- read.csv("data.csv", header = TRUE)

summary(got_data)

got_data <- got_data %>% unite(episode_id, "season", "episode", remove = FALSE)

#creating data frame with just ratings data
got_data_ratings <- select(got_data, -character)
got_data_ratings <- select(got_data_ratings, - screen.time)

#Removing duplicates from dataframe
got_data_ratings <- distinct(got_data_ratings)

season1 <- filter(got_data, got_data$season == "1")
##season1 <- filter(season1, season1$screen.time > "400")


numbers <- data.frame(1:67)
colnames(numbers) <- c("episode_number")
episode_id <- unique(got_data$episode_id)
numbers <- cbind(numbers, episode_id)
numbers$episode_id <- as.character(numbers$episode_id)
got_data <- got_data %>%
  left_join(numbers, got_data, by = "episode_id")

ggplot(data = got_data) + 
  geom_smooth(aes(x = episode_number, y = overall_rating), color = "darkred", size = 3) +
  labs(title = "Overall Rating by Episode",
       x = "All Episodes",
       y = "OVerall Rating") +
  theme(plot.title = element_text(hjust = 0.5))


#plot episode ratings curve
ggplot(data = got_data_ratings) + geom_smooth(aes(x=episode,y = overall_rating, color = season)) +
  labs(title = "Episode Ratings Curve",
       x = "Episodes Collapsed",
       y = "Overall Rating") +
  theme(plot.title = element_text(hjust = 0.5))

#scatter plot
ggplot(data = got_data_ratings) + geom_jitter(aes(x=episode, y = overall_rating, color = factor(season), size = total_votes)) +
  scale_color_manual(values = c("seagreen", "gold", "grey45", "darkred", "darkblue", "purple", "chocolate")) +
  guides(color=guide_legend(title= "Season"), size = guide_legend(title = "Total Votes on IMDB")) +
  labs(title = "Episode and Ratings Scatter Plot",
       x = "All Episodes",
       y = "Overall Rating") +
  theme(plot.title = element_text(hjust = 0.5))


#Peason correlation test
cor.test(got_data_ratings$overall_rating, got_data_ratings$episode, method = "pearson")

got_data$season <- as.factor(got_data$season)
#bar graph of episodes by number votes and ratings
ggplot(data = got_data) + geom_col(aes(x=episode_number,y = total_votes, fill = overall_rating)) +
  labs(title = "Episodes by Number Votes and Ratings",
       x = "All Episodes",
       y = "Number of Votes") +
  guides(fill=guide_legend(title= "Overall Rating")) +
           theme(plot.title = element_text(hjust = 0.5))

#Filter to create a data frame of data for Arya
Arya <- filter(got_data, got_data$character == "Arya Stark")

#Arya screentime by episode and rating
ggplot(data = Arya) + geom_col(aes(x = episode_number, y = screen.time, fill = overall_rating), color = "grey45") +
  labs(title = "Arya Stark's Screentime with Overall Episode Ratings",
       x = "All Episodes",
       y = "Duration of Screentime (in Seconds)") +
  guides(fill=guide_legend(title= "Overall Rating")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = got_data) + geom_bar(aes(character))

#cummulative screentime
cummulative_screentime <- got_data %>%
  group_by(character) %>%
  summarise(sum = sum(screen.time))

cummulative_screentime


#using cummulative screentime df to find characters with the most screen time
top_characters <- filter(cummulative_screentime, cummulative_screentime$sum > 10000)
top_characters <- top_characters[order(top_characters$sum), ]


#creating a filtering vector for top characters
vector_character <- as.vector(top_characters$character)


#Using the vector_character vector to filter through all game of thrones data
got_data_top_34 <- subset(got_data, got_data$character %in% vector_character)
got_data_top_34$character <- factor(got_data_top_34$character, levels = top_characters$character)

ggplot(got_data_top_34) + 
  geom_tile(mapping = aes(x = episode_number, y = character,  fill = overall_rating),  col = "black") + 
  theme(strip.text.y = element_text(size = 10, angle = 180)) + 
  scale_fill_gradient(low = "red", high = "green") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  labs(title = "Game of Thrones Characters with Most Screen Time and Episode Ratings",
       x = "Episode",
       y = "Character",
       fill = "Rating")  +
  theme(plot.title = element_text(hjust = 0.5))

mean_rating <- got_data_top_34 %>%
  group_by(character) %>%
  summarise(avr = mean(overall_rating))

my_norm <- function(x){(x-min(x))/(max(x)-min(x))}
mean_rating <- mean_rating %>%
  mutate(norm = my_norm(mean_rating$avr))

mean_rating$norm <- as.factor(mean_rating$norm)
mean_rating$norm1 <- strtrim(mean_rating$norm, 4)
mean_rating$house <- c("Lannister", "Stark", "Targaryen", "Lannister", "Stark", "Stark", "Lannister", 
                       "Mormont", "Greyjoy", "Baelish", "No House", "Tarly", "No House", "Tarth", "Stark", 
                       "No House", "Cleagan", "No House", "Stark", "Baratheon", "No House", "Tyrell", "No House", 
                       "No House", "Lannister", "Stark", "No House", "Tollett", "Payne", "No House",
                       "Baratheon", "Targaryen", "Baratheon", "Stark")
mean_rating$house <- rev(mean_rating$house)

ggplot(mean_rating) + geom_col(aes( x = character, y = norm1, group = 1, fill = house), color = "grey45") + 
  scale_fill_manual(values = c("seagreen1", "gold", "darkgoldenrod1", "black", "darkred",  "darkgreen", "paleturquoise",
                    "purple1", "grey75", "red", "darkolivegreen", "navy", "grey50", "olivedrab1")) +
  coord_flip() + 
  labs(title = "Top 34 Characters and Normalized Average Episode Rating",
       y = "Normalized Average Rating",
       x= "",
       fill = "House") +
 theme(axis.text.x=element_blank(),
      axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.5))

got_data$character <- as.character(got_data$character)
got_data$episode_id <- as.character(got_data$episode_id)
cummulative_screentime$character <- as.character(cummulative_screentime$character)

got_data_2 <- left_join(got_data, cummulative_screentime, by = "character")

got_data_2 <- got_data_2 %>%
  mutate(rating_st = got_data_2$overall_rating * got_data_2$screen.time* got_data_2$sum)

got_data_2 <- got_data_2 %>%
  group_by(episode_id) %>%
  mutate(rating_norme = my_norm(rating_st))

got_data_2 <- got_data_2 %>%
  mutate(weighted_rating = rating_norme*10)

mean_rating_weighted <- got_data_2 %>%
  group_by(character) %>%
  summarise(avr = mean(weighted_rating))

mean_rating_weighted <- left_join(cummulative_screentime, mean_rating_weighted, by = "character")



top_characters <- filter(mean_rating_weighted, mean_rating_weighted$avr > "0.23")

top_characters$character <- factor(top_characters$character , levels = top_characters$character [order(top_characters$avr)])

ggplot(top_characters) + geom_col(aes( x = character, y = avr, fill = sum), color = "grey45")+
  coord_flip() + 
  labs(title = "Top Rated Characters with Weighted Average Episode Rating",
       y = "Weighted Average Rating",
       x= "") +
  guides(fill = guide_legend(title = "Cummulative Screen Time")) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.5))



