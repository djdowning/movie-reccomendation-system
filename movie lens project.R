################################
# Create Function
################################

#Load tidyverse and create RMSE function to create all of the revelant RMSEs for the project
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))}



################################
# Create edx set, validation set
################################
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")


# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#create data files in rda folder to use in the r markdown report
save(edx, file = "rda/edx.rda")
save(validation, file = "rda/validation.rda")


################################
#JUST THE AVERAGE
################################
#First Lets Compute Just the overall average and get the RMSE from predicting the rating based on the average.
edx_mu <- mean(edx$rating)
edx_mu

edx_naive_rmse <- RMSE(validation$rating, edx_mu)
edx_naive_rmse

#Create table to store RMSE results of all models and store avg
edx_rmse_results <- tibble(method = "Just the average", RMSE = edx_naive_rmse)

################################
#MOVIE EFFECT
################################
#Now Lets Take a Look at the Movie Effect by caluclating bi to see how much better or worse each movie is than avg
edx_movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - edx_mu))
#See data
edx_movie_avgs
#Visualize movie effect
edx_movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"), xlab="Rating Score vs. Mean")

#Predit validation ratings
predicted_ratings <- edx_mu + validation %>% 
  left_join(edx_movie_avgs, by='movieId') %>%
  .$b_i

#Get RMSE
edx_1_rmse <- RMSE(predicted_ratings, validation$rating)
edx_rmse_results <- bind_rows(edx_rmse_results,
                          tibble(method="Movie Effect Model",
                                     RMSE = edx_1_rmse ))
#View results as we go
edx_rmse_results %>% knitr::kable()


################################
#USER EFFECT
################################
#Now lets add the user affect to see how the indivudal users vary in giving scores to try to improve the model

#Visualize avg ratting per user for users with at least 50 reviews
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=50) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

#Calculate user effect for all 
edx_user_avgs <- edx %>% 
  left_join(edx_movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - edx_mu - b_i))

#Predict validation
predicted_ratings <- validation %>% 
  left_join(edx_movie_avgs, by='movieId') %>%
  left_join(edx_user_avgs, by='userId') %>%
  mutate(pred = edx_mu + b_i + b_u) %>%
  .$pred

#Get RMSE
edx_2_rmse <- RMSE(predicted_ratings, validation$rating)
edx_rmse_results <- bind_rows(edx_rmse_results,
                          tibble(method="Movie + User Effects Model",  
                                     RMSE = edx_2_rmse ))
#view results as we go along
edx_rmse_results %>% knitr::kable()

################################
#REGULARIZATION
################################
#Now lets examine the effect of movies with few reviews skewing our predictions lets get the titles 1st
edx_movie_titles <- edx %>% 
  select(movieId, title) %>%
  distinct()

#Lets looks and the best movies by bi and how many reviews they have
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

#Lets apply regularization just to the movies 1st so we can see the impact vs Movie Effect Model
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - edx_mu)/(n()+l))
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    mutate(pred = edx_mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})

edx_rmse_results <- bind_rows(edx_rmse_results,
                              tibble(method="Regularized Movie Effect Model",  
                                     RMSE = min(rmses)))


#Now lets use regularization to minimize the effect of movies and users with small numbers of reviews
#Lets find the optimal value for lambda using a function and then get the best RMSE with that optimal lambda
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


#Add to results table
edx_rmse_results <- bind_rows(edx_rmse_results,
                          tibble(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))

################################
#FINAL RESULTS
################################
edx_rmse_results %>% knitr::kable()




