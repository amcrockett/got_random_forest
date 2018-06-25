set.seed(1234567890)
library(neuralnet)
library(ggplot2)

denormalize <- function(predicted, actual) {
  d <- predicted * (max(actual)-min(actual))+min(actual)
  return(d)
}
#i.e. denormalize(got_data_2$rating_norme, got_data_2$overall_rating)

data <- read.csv("./got_data_wide_predictive.csv", header = TRUE)
norm <- function(x){(x-min(x))/(max(x)-min(x))}
wide <- as.data.frame(lapply(data, norm))
wide$episode <- data$X
wide$original_rating <- data$overall_rating
l <- nrow(wide)
randIndex <- sample(1:l)
# take 75% as training (train more data)
cut <- floor(3*l/4)
trainData <- wide[randIndex[1:cut],]
testData <- wide[randIndex[cut:l],]
c <- paste(colnames(trainData)[3:555], collapse="+")
x <- as.formula(paste("overall_rating ~ ",c,sep = ""))
layers = c(10) #0,100,10) 
cnet <- neuralnet(x, trainData,hidden=layers, linear.output=TRUE, learningrate=.01,
                  rep=10,
                  algorithm="backprop") 
pred <- neuralnet::compute(cnet,testData[,3:555])
results <- data.frame(actual=testData$overall_rating, prediction = pred$net.result)
results$id <- testData$episode
results$rating <- testData$original_rating
results$pred <- denormalize(results$prediction, results$rating) #results$prediction * attr(rs, 'scaled:scale') + attr(rs, 'scaled:center')
# here would like to take the prediction and make it on the 0-10 scale
mspe <- mean((results$actual - results$prediction) ^ 2)
mspe <- mspe * 10
t <- sprintf("Scaled Actual vs. Predicted Ratings (mspe) %s %%",round(mspe, 2))
g <- ggplot(results) +
  geom_point(aes(x = id, y=actual), alpha=1, size=5, color="red") +
  geom_point(aes(x= id, y=prediction), alpha=0.5, size=4, color="green") +
  labs(title = t,
       x = "Episode Number",
       y = "Rating")  +
  scale_shape_discrete(solid=F) +
  theme(plot.title = element_text(hjust = 1.0)) +
  theme(legend.position="none") 

t <- sprintf("Actual vs. Predicted Ratings (mspe) %s %%",round(mspe, 2))
h <- ggplot(results) +
  geom_point(aes(x = id, y=rating), alpha=1.0, size=5, color="green") +
  geom_point(aes(x= id, y=pred), alpha=0.8, size=4, color="red") +
  labs(title = t,
       x = "Episode Number",
       y = "Rating")  +
  scale_shape_discrete(solid=F) +
  theme(plot.title = element_text(hjust = 1.0)) +
  theme(legend.position="none") 
grid.arrange(h,g)


results <- results %>%
  group_by(id) %>%
  mutate(error = actual - prediction)

results_2$error <- abs(results_2$error)

results_2 <- results %>% 
  gather(key= neural_net, "rating", "pred", value = value)
results_2

cc <- scales::gradient_n_pal("green", "red", "Lab")(seq(0,1,length.out=100))

results_2_plot <- ggplot(data = results_2, aes(x = id, y = value, label = id)) +
  geom_line(aes(group = id, color = error), size = 2) +
  geom_label(aes(fill = factor(neural_net)), colour = "white", fontface = "bold") +
  guides(fill=guide_legend(title="Neural Network")) + 
  scale_fill_discrete(labels = c("Prediction", "Actual")) +
  labs(title = "Actual vs. Predicted Ratings (mspe) 1.37 %",
       x = "Episode Number",
       y = "Rating") +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5)) 
  

results_2_plot

cnet_2 <- neuralnet(x, trainData, hidden=layers, linear.output=TRUE, learningrate=0.01,
                  rep=2,
                  algorithm="backprop") 

plot(cnet_2, rep="best")
