
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

#Packages that are needed to resolve the MovieLens Project

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggpubr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# In the code below, we are mutating movies data frame, adding movieId, title and genres with the correct format to be manipulated.
# Due to R 3.6.3 is used, this writing method is needed.
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
#Matching columns ratings and movies by movieId to create a complete data table with the correct format.
movielens <- left_join(ratings, movies, by = "movieId")


# Due to R 3.6.3 is used, we add sample.kind argument to set.seed function.
set.seed(1, sample.kind="Rounding")

# Edx set will be 90% of MovieLens data
# Validation set will be 10% of MovieLens data
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

## Data exploration
# 1.- Number of rows & columns

#Edx

nrow(edx) #9000055
ncol(edx) #6

#Validation

nrow(validation) #999999
ncol(validation) #6

# 2.- Name of the variables in both datasets 
  
#Same variables in both cases
colnames(edx) # "userId"  "movieId"   "rating"  "timestamp"   "title"   "genres"

# 3.- Summary stadistics

#Edx

summary(edx)

#Validation

summary(validation)

# 4.- How many different movies and users are in both datasets

#Edx

n_distinct(edx$userId) #69878
n_distinct(edx$movieId) #10677

#Validation

n_distinct(validation$userId) #68534
n_distinct(validation$movieId) #9809

## Data cleaning and Influence of variables on rating

# 1.- Timestamp & Ratings

# In order to do a complete analysis of the influence of Timestamp on Ratings, 3 graphs are plotted:Timestamp rounded to week, Timestamp rounded to month and Timestamp rounded to year.

# Firstable, we are converting Timestamp variable to a date. 
# Then, we are rounding it by week (date_week), month (date_month) and year (date_year) to see the relationship between these new variables and Ratings

edx <- edx %>% mutate(Datetime = as_datetime(timestamp)) %>% mutate(date_week = round_date(Datetime, unit = "week"),date_month = round_date(Datetime, unit = "month"),date_year = round_date(Datetime, unit = "year")) 
Timestamp_week <- edx %>% group_by(date_week) %>% summarize(rating = mean(rating)) %>% ggplot(aes(date_week,rating)) + geom_point() + geom_smooth() + ggtitle("Timestamp rounded to week & Ratings")
Timestamp_week

Timestamp_month <- edx %>% group_by(date_month) %>% summarize(rating = mean(rating)) %>% ggplot(aes(date_month,rating)) + geom_point() + geom_smooth() + ggtitle("Timestamp rounded to month & Ratings")
Timestamp_month

Timestamp_year <- edx %>% group_by(date_year) %>% summarize(rating = mean(rating)) %>% ggplot(aes(date_year,rating)) + geom_point() + geom_smooth() + ggtitle("Timestamp rounded to year & Ratings")
Timestamp_year


# Conclusion 1.-: There is some evidence of a time effect on average rating.

# 2.- Year & Ratings

# The first thing we have to do is to extract the year of the movie that is located in the Title column
# In the line 124, we are extracting the date with parenthesis

year_extract_with_p <- str_extract(edx$title,'\\(\\d{3,4}\\)')

# In the line 127, we are extracting just the year of the movie. Then, we are including these years to a new column in the edx dataset
year_out <- as.numeric(str_extract(year_extract_with_p,'\\d{3,4}'))
edx <- edx %>% mutate(year = year_out)


edx %>% group_by(year) %>% summarize(rating = mean(rating)) %>% ggplot(aes(year,rating)) + geom_point() + geom_smooth()

# Conclusion 2.-: There is strong evidence of a Year effect on average rating. We can see that the films broadcast between 1930 and 1970 have much better score than the current ones.

# 3.- UserId & Ratings

# Because of there are many differents users (>65000), in this case we are plotting an histogram that represent the relation between Mean Rating and Number of users
# Note that we are applying a filter to select the users that have voted, at least, 50 times.

edx %>%
  group_by(userId) %>%
  summarize(rating = mean(rating)) %>%
  filter(n()>=50) %>%
  ggplot(aes(rating)) +
  geom_histogram(bins = 30, color = "black", fill = "darkseagreen1") + ggtitle("UserId & Ratings") + xlab("Mean rating") + ylab("Number of users")

# Conclusion 3.-: There is strong evidence of a UserId effect on average rating. Most users rate movies with a score of around 3.5.

# 4.- MovieId & Ratings

# As the previous analysis, because of there are many differents movies (>9000), in this case we are plotting an histogram that represent the relation between Mean Rating and Number of movies
# Note that we are applying a filter to select the movies that have been voted, at least, 15 times.

edx %>%
  group_by(movieId) %>%
  summarize(rating = mean(rating)) %>%
  filter(n()>=15) %>%
  ggplot(aes(rating)) +
  geom_histogram(bins = 30, color = "black", fill = "darkslategray1") + ggtitle("MovieId & Ratings") + xlab("Mean rating") + ylab("Number of movies")

# Conclusion 4.-: There is strong evidence of a MovieId effect on average rating. Most films have been rated with a score between 3 and 4.

# 5.- Genre & Ratings

# In the lines below, we are checking how many genre types are in the datasets
ul <- unlist(str_split(edx$genres,"\\|"))
genres_types <- unique(ul) 
genres_types # There are 20 different genres 

# Due to we have datasets with more than 9M of observations, createDataPartition function is needed.
edx_partition <- createDataPartition(edx$rating,times = 1, p = 0.1, list = FALSE)

# Edx_10 set will be 10% of MovieLens data
edx_10 <- edx[edx_partition,]

# In most cases, there are more than one genre in a movie, so, we have to separate this genres to facilitate the analysis.
genres_separates <- edx_10 %>% separate_rows(genres, sep ="\\|")

genre_ratings <- genres_separates %>%  group_by(genres) %>%
  summarize(genre_rating = mean(rating)) %>% arrange(-genre_rating)

genre_ratings %>% 
  ggplot(aes(reorder(genres, genre_rating), genre_rating, fill= genre_rating)) +
  geom_bar(stat = "identity")+ coord_flip() +
  scale_fill_distiller(palette = "YlOrRd") + labs(y = "Mean Rating", x = "Genre") +
  ggtitle("Influence of genre in ratings")

# Conclusion 5.-: There is strong evidence of a Genre effect on average rating.

## Results

# Training process

set.seed(2020,sample.kind = "Rounding")
options(digits = 5)
edx_test_index <- createDataPartition(edx$rating,times = 1, p = 0.1, list = FALSE)

edx_train_set <- edx[-edx_test_index,]
edx_test_set_temp <- edx[edx_test_index,]

edx_test_set <- edx_test_set_temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

#Just the average
mu_hat <- mean(edx_train_set$rating)

naive_rmse <- RMSE(edx_test_set$rating,mu_hat)

options(pillar.sigfig = 5)
rmse_results <- tibble(Model = "Just the average", RMSE = naive_rmse)
rmse_results 

#Date effect
date_avgs <- edx_train_set %>%
  group_by(date_month) %>%
  summarize(b_d = mean(rating-mu_hat))

predicted_ratings <- mu_hat + edx_test_set %>%
  left_join(date_avgs, by='date_month') %>%
  pull(b_d)
Date_model <- RMSE(predicted_ratings, edx_test_set$rating,na.rm=TRUE)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Date Effect Model",  
                                     RMSE =Date_model))
rmse_results 

#Year effect
year_avgs <- edx_train_set %>%
  group_by(year) %>%
  summarize(b_y = mean(rating-mu_hat))

predicted_ratings <- mu_hat + edx_test_set %>%
  left_join(year_avgs, by='year') %>%
  pull(b_y)
Year_model <- RMSE(predicted_ratings, edx_test_set$rating,na.rm=TRUE)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Year Effect Model",  
                                     RMSE =Year_model))
rmse_results 

#Movie effect
mu <- mean(edx_train_set$rating)
movie_avgs <- edx_train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + edx_test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

Movie_model <- RMSE(predicted_ratings, edx_test_set$rating,na.rm=TRUE)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Movie Effect Model",  
                                     RMSE =Movie_model))
rmse_results 

#User effect

user_avgs <- edx_train_set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu))

predicted_ratings <- mu + edx_test_set %>%
  left_join(user_avgs, by='userId') %>%
  pull(b_u)

User_model <- RMSE(predicted_ratings, edx_test_set$rating,na.rm=TRUE)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="User Effect Model",  
                                     RMSE =User_model))
rmse_results 

#User + Movie effect
user_avgs <- edx_train_set %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- edx_test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

User_plus_Movie_model <- RMSE(predicted_ratings, edx_test_set$rating,na.rm=TRUE)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="User + Movie Effects Model",  
                                     RMSE =User_plus_Movie_model))
rmse_results 

#Regularization with User effect

lambdas_1 <- seq(0, 10, 0.1)
rmses_1 <- sapply(lambdas_1, function(l){
  mu <- mean(edx_train_set$rating)
  b_u <- edx_train_set %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu)/(n()+l))
  
  predicted_ratings <-
    edx_test_set %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx_test_set$rating,na.rm = TRUE))
})
qplot(lambdas_1,rmses_1,main = "Lambda vs RMSE | Regularization with User effect",xlab = "Lambda",ylab = "RMSE")
Reg_User_model <- min(rmses_1)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized User Effect Model",  
                                     RMSE =Reg_User_model))
rmse_results 

#Regularization with Movie effect

lambdas_2 <- seq(0, 10, 0.1)
rmses_2 <- sapply(lambdas_2, function(l){
  mu <- mean(edx_train_set$rating)
  b_i <- edx_train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  predicted_ratings <-
    edx_test_set %>%
    left_join(b_i, by = "movieId") %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx_test_set$rating,na.rm = TRUE))
})
qplot(lambdas_2,rmses_2,main = "Lambda vs RMSE | Regularization with Movie effect",xlab = "Lambda",ylab = "RMSE")
Reg_Movie_model <- min(rmses_2)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Movie Effect Model",  
                                     RMSE =Reg_Movie_model))
rmse_results 

#Regularization with User + Movie effect

lambdas_3 <- seq(0, 10, 0.1)
rmses_3 <- sapply(lambdas_3, function(l){
  mu <- mean(edx_train_set$rating)
  b_i <- edx_train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <-
    edx_test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx_test_set$rating,na.rm = TRUE))
})
qplot(lambdas_3,rmses_3,main = "Lambda vs RMSE | Regularization with User + Movie effect",xlab = "Lambda",ylab = "RMSE")
lambda <- lambdas_3[which.min(rmses_3)]
Reg_User_plus_Movie_model <- min(rmses_3)
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized User + Movie Effects Model",  
                                     RMSE =Reg_User_plus_Movie_model))
rmse_results 

#Validation process

mu_val <- mean(edx$rating)
b_i_val <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_val)/(n()+lambda))
b_u_val <- edx %>% 
  left_join(b_i_val, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu_val)/(n()+lambda))
predicted_ratings_val<- 
  validation %>% 
  left_join(b_i_val, by = "movieId") %>%
  left_join(b_u_val, by = "userId") %>%
  mutate(pred = mu_val + b_i + b_u) %>%
  pull(pred)

Validation_model <- RMSE(predicted_ratings_val, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Validation Model",  
                                     RMSE =Validation_model ))

rmse_results 

#RSME
Validation_model

