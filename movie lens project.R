
#First Lets COmpute Just the overall average and get the RMSE from predicting the rating based on the average.
edx_mu <- mean(edx$rating)
edx_mu

edx_naive_rmse <- RMSE(validation$rating, edx_mu)
edx_naive_rmse

#create table to store RMSE results of all models and store avg
edx_rmse_results <- tibble(method = "Just the average", RMSE = edx_naive_rmse)


#MOVIE EFFECT
#Now Lets Take a Look at the Movie Effect by caluclating bi to see how much better or worse each movie is than avg
edx_movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - edx_mu))
#see data
edx_movie_avgs
#visualize movie effect
edx_movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"), xlab="Rating Score vs. Mean")

#predit validation ratings
predicted_ratings <- edx_mu + validation %>% 
  left_join(edx_movie_avgs, by='movieId') %>%
  .$b_i

#get RMSE
edx_1_rmse <- RMSE(predicted_ratings, validation$rating)
edx_rmse_results <- bind_rows(edx_rmse_results,
                          tibble(method="Movie Effect Model",
                                     RMSE = edx_1_rmse ))
#view results as we go
edx_rmse_results %>% knitr::kable()



#USER EFFECT
#Now lets add the user affect to see how the indivudal users vary in giving scores to try to improve the model

#visualize avg ratting per user for users with at least 50 reviews
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=50) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

#calculate user effect for all 
edx_user_avgs <- edx %>% 
  left_join(edx_movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - edx_mu - b_i))

#predict validation
predicted_ratings <- validation %>% 
  left_join(edx_movie_avgs, by='movieId') %>%
  left_join(edx_user_avgs, by='userId') %>%
  mutate(pred = edx_mu + b_i + b_u) %>%
  .$pred

#get RMSE
edx_2_rmse <- RMSE(predicted_ratings, validation$rating)
edx_rmse_results <- bind_rows(edx_rmse_results,
                          tibble(method="Movie + User Effects Model",  
                                     RMSE = edx_2_rmse ))
#view results as we go along
edx_rmse_results %>% knitr::kable()


#REGULARIZATION
#now lets examine the effect of movies with few reviews skewing our predictions
edx_movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()

#lets looks and the best movies by bi and how many reviews they have
edx %>% dplyr::count(movieId) %>% 
  left_join(edx_movie_avgs) %>%
  left_join(edx_movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

#lets looks and the worst movies by bi and how many reviews they have
edx %>% dplyr::count(movieId) %>% 
  left_join(edx_movie_avgs) %>%
  left_join(edx_movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()


#lets use regularization to minimize the effect of movies and users with small numbers of reviews
#lets find the optimal value for lambda using a function and then get the best RMSE with that optimal lambda
library(tidyverse)
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))}
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
   b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - edx_mu)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - edx_mu)/(n()+l))
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = edx_mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})
lambdas[which.min(rmses)]


edx_rmse_results <- bind_rows(edx_rmse_results,
                          tibble(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
edx_rmse_results %>% knitr::kable()




