##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this code has been modified
# Note: this process could take a couple of minutes

# install and load needed libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("GridExtra", repos = "http://cran.us.r-project.org")
if(!require(raster)) install.packages("raster", repos = "http://cran.us.r-project.org")
if(!require(float)) install.packages("float", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(knitr)
library(gridExtra)
library(raster)
library(float)

#set options
options(timeout = 120, digits = 4, scipen = 999)

# download data files
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

# turn ratings_file into a dataframe
ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

# turn movies_file into a dataframe
movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

# join ratings and movies dataframes into one called "movielens"
movielens <- left_join(ratings, movies, by = "movieId")

# Create training set (edx) and validation set(final_holdout_test) 
# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Ensure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

# clean up environment by removing temp files
rm(dl, ratings, movies, test_index, temp, removed)

##########################################################
# Data Exploration and Analysis
##########################################################
head(edx) %>%
  knitr::kable(caption="First 5 rows of edx dataset", format.args=list(scientific=FALSE))
str(edx)
edx %>%
  summarize(n_ratings = nrow(edx),# number of ratings
            n_users = n_distinct(userId),# number of unique users
            n_movies = n_distinct(movieId))# number of unique movies

#RATING exploration
min(edx$rating)
max(edx$rating)
mean_rating <- mean(edx$rating)

#rating distribution
edx %>% ggplot(aes(rating)) + 
  geom_histogram(binwidth = .5, color = "black") +
  geom_vline(aes(xintercept = mean_rating), color = "red", linetype = "dashed", size=1) +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000)))

#MOVIE EXPLORATION
#movie distribution by avg rating
edx %>%
  group_by(movieId) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(bins = 25, color = "black")+
  geom_vline(aes(xintercept = mean_rating), color = "red", linetype = "dashed", size=1) +
  xlab("Average rating") +
  ylab("n_movies")

#Number of ratings per movie
edx%>% count(movieId) %>% summarize(round(mean(n),0.5))

edx %>% count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 25, color = "black") +
  scale_x_log10() +
  xlab("n_ratings") +
  ylab("n_movies")

n_ratings <- edx %>% group_by(title) %>% 
  summarize(n_ratings = n(), rating = mean(rating)) %>%
  arrange(desc(n_ratings))

min(n_ratings$n_ratings)
max(n_ratings$n_ratings)

head(n_ratings) %>% knitr::kable(caption = "Average rating for movies arranged by highest number of ratings")

#Movies rated once
one_rating <- edx %>% group_by(movieId) %>% summarize(count = n()) %>%
  filter(count == 1)
count(one_rating)

p <-  edx %>%
  group_by(movieId) %>%
  summarize(n_ratings = n()) %>%
  filter(n_ratings ==1) %>%
  left_join(edx, by="movieId") %>%
  group_by(title) %>%
  summarize(n_ratings, rating = rating) 
knitr::kable(p[1:10,], caption = "Average rating for movies with one rating")

#USER EXPLORATION
#distribution of users by average rating
edx %>%
  group_by(userId) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(bins = 25, color = "black")+
  geom_vline(aes(xintercept = mean_rating), color = "red", linetype = "dashed", size=1) +
  xlab("Average rating") +
  ylab("n_users")

#Number of ratings per user
edx%>% count(userId) %>% summarize(round(mean(n), 0.5))

edx %>% count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 25, color = "black") +
  scale_x_log10() +
  xlab("n_ratings") +
  ylab("n_users")

n_ratings <- edx %>% group_by(userId) %>% 
  summarize(count = n(), rating = mean(rating)) %>%
  arrange(desc(count))

min(n_ratings$count)
max(n_ratings$count)

#GENRE EXPLORATION
#genre example
edx %>% dplyr::select(title, genres) %>% slice(1:5,) %>% kable(caption = "Sample of movies with genres")

#number of genres
n_distinct(edx$genres)
edx$genres %>% unique() %>% str_split("\\|") %>% flatten_chr() %>% unique() %>% n_distinct()

#unique genres with counts - slow! 
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
#note "no genres listed"

#individual genres per movie - slow!
genres_per_movie <- edx %>% group_by(movieId) %>% 
  data.frame(count = str_count(edx$genres, "\\|") + 1 - str_count(edx$genres, "no genres listed")) %>%
  group_by(movieId, title) %>% summarize(count = mean(count))

genres_per_movie %>% 
  group_by(count)%>%
  ggplot(aes(count)) +
  geom_histogram(bins = 25, color = "black")+
  scale_x_continuous(breaks = c(seq(0, 8, 1)))+
  xlab("n_genres") +
  ylab("n_movies")

mean(genres_per_movie$count != 1)

#ratings per number of genres
ratings_per_genre <- edx %>% left_join(genres_per_movie, by = "movieId")

ratings_per_genre %>%
  group_by(n=count) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 25, color="black") +
  scale_x_continuous(breaks = c(seq(0,8,1))) +
  xlab("n_genres") +
  ylab("n_ratings")

mean(ratings_per_genre$count != 1)

# avg rating per genre combinations (with over 100,000 ratings)
edx %>% group_by(genres) %>%
  summarize(n = n(), avg_rating = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 100000) %>% 
  mutate(genres = reorder(genres, avg_rating)) %>% 
  ggplot(aes(x = genres, y = avg_rating, ymin = avg_rating - 2*se, ymax = avg_rating + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  ylim(3, 4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Genre combinations") +
  ylab("Average rating")

#TITLE EXPLORATION 
#Extract release year from title 
rel_yr <- "(?<=\\()\\d{4}(?=\\))"
edx <- edx %>%
  mutate(release_year = str_extract(title, rel_yr) %>%
           as.integer())

#release year vs average rating
edx %>%
  group_by(release_year) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(release_year, avg_rating)) +
  geom_point()+
  geom_smooth() +
  xlab("Release Year") +
  ylab("Average Rating")

min(edx$release_year)
max(edx$release_year)

#count of ratings per year
edx %>% group_by(release_year) %>%
  summarize(count = n()) %>%
  ggplot(aes(release_year, count)) +
  geom_line() +
  xlab("Year") +
  ylab("n_ratings")

#TIMESTAMP Exploration
#to use the timestamp, it first must be converted to datetime

edx <- edx %>% mutate(timestamp = as_datetime(timestamp))
min(edx$timestamp)
max(edx$timestamp)

#average rating per time (day,week,month,year)
day <- edx %>%
  mutate(day = round_date(timestamp, unit = "day")) %>%
  group_by(day) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(day, rating)) + geom_point() + geom_smooth()+
  ylim(c(1,5))
week <- edx %>%
  mutate(week = round_date(timestamp, unit = "week")) %>%
  group_by(week) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(week, rating)) + geom_point() + geom_smooth() +
  ylim(c(1,5))
month <- edx %>%
  mutate(month = round_date(timestamp, unit = "month")) %>%
  group_by(month) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(month, rating)) + geom_point() + geom_smooth()+
  ylim(c(1,5))
year <- edx %>%
  mutate(year = round_date(timestamp, unit = "year")) %>%
  group_by(year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year, rating)) + geom_point() + geom_smooth()+
  ylim(c(1,5))

grid.arrange(day, week, month, year, ncol = 4)


#create new column for week
edx <- edx %>%
  mutate(date = round_date(timestamp, unit = "week")) 

#count of ratings per week
edx %>% group_by(date) %>%
  summarize(count = n()) %>%
  ggplot(aes(date, count)) +
  geom_line() +
  xlab("Week") +
  ylab("n_ratings")

##################################################
# METHODS
##################################################

## create training and test sets
set.seed(2024, sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,] 
temp <- edx[test_index,]

# Ensure userId and movieId in test_set set are also in train_set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

#RMSE goal
goal_rmse <- 0.8649
rmse_results <- tibble(Model = "Goal RMSE", RMSE = goal_rmse, Difference = 0)
rmse_results %>% knitr::kable()

##################################################
# Model Development
##################################################
#Average model
#calculate mean
mu <- mean(train_set$rating)

#calculate RMSE
avg_rating_rmse <- RMSE(mu, test_set$rating)
rmse_results <- bind_rows(rmse_results, 
                           data.frame(Model="Average Rating", RMSE = avg_rating_rmse, Difference = avg_rating_rmse - goal_rmse))
rmse_results %>% knitr::kable()

#first model - Movie Effect
#calculate movie bias
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating-mu))
#plot movie bias
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 30, data = ., color = I("black"))
#b_i is the difference between the overall average (3.5) and the average of each movie's total ratings.  For example, movieId 1 has an overall average rating of
# 0.416 higher than 3.5.  So it's overall average rating is 3.512+0.416= 3.928.  we can confirm this by the following code:
train_set %>% filter(movieId == 1) %>% summarize(mean(rating))

#add movie bias (b_i) to overall average for each movie in test_set
preds <- mu + test_set %>%
  left_join(movie_avgs, by="movieId") %>%  
  pull(b_i)
range(preds) #check prediction range
model_1_rmse <- RMSE(preds, test_set$rating)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(Model="Movie Effect",
                                     RMSE = model_1_rmse,
                                     Difference = model_1_rmse - goal_rmse))
rmse_results %>% knitr::kable()

#second model - User effect
#calculate bias
user_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
#plot bias
user_avgs %>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black"))

#add user bias to prediction
preds <- test_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

#check prediction range
range(preds) #out of range,  Need to correct ratings to lowest of 0.5, highest of 5
preds_clamped <- clamp(preds, 0.5, 5)

model_2_rmse <- RMSE(preds_clamped, test_set$rating)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(Model="Movie + User Effects",
                                     RMSE = model_2_rmse, 
                                     Difference = model_2_rmse - goal_rmse))
rmse_results%>%knitr::kable()

#third model - genre effect
#calculate bias
genre_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

#plot difference from average
genre_avgs %>% qplot(b_g, geom ="histogram", bins = 30, data = ., color = I("black"))

#add genre bias to prediction
preds <- test_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

range(preds) 
preds_clamped <- clamp(preds, 0.5, 5)

model_3_rmse <- RMSE(preds_clamped, test_set$rating)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(Model="Movie + User + Genre Effects",
                                     RMSE = model_3_rmse, 
                                     Difference = model_3_rmse - goal_rmse))
rmse_results%>%knitr::kable()

# fourth model - release year effect
#calculate release year bias
year_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  group_by(release_year) %>%
  summarize(b_r = mean(rating - mu - b_i - b_u - b_g))

#plot bias
year_avgs %>% qplot(b_r, geom ="histogram", bins = 30, data = ., color = I("black"))

#add bias to predictions
preds <- test_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(year_avgs, by = "release_year") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_r) %>%
  pull(pred)
range(preds) #have predicted ratings as low as -0.55/as high as 6.11.  Need to correct ratings to lowest of 0.5, highest of 5
preds_clamped <- clamp(preds, 0.5, 5)

model_4_rmse <- RMSE(preds_clamped, test_set$rating)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(Model="Movie + User + Genre + Release Year Effects",
                                     RMSE = model_4_rmse, 
                                     Difference = model_4_rmse - goal_rmse))
rmse_results%>%knitr::kable()

# fifth model - time of rating effect
#calculate time of rating bias
time_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(year_avgs, by = "release_year") %>%
  group_by(date) %>%
  summarize(b_t = mean(rating - mu - b_i - b_u - b_g - b_r))
#plot bias
time_avgs %>% qplot(b_t, geom ="histogram", bins = 30, data = ., color = I("black"))

#add bias to predictions
preds <- test_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(year_avgs, by = "release_year") %>%
  left_join(time_avgs, by = "date") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_r + b_t) %>%
  pull(pred)
 
range(preds) #have predicted ratings as low as -0.54/6.11.Need to correct ratings to lowest of 0.5, highest of 5
preds_clamped <- clamp(preds, 0.5, 5)

model_5_rmse <- RMSE(preds_clamped, test_set$rating)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(Model="Movie + User + Genre + Release Year + Review Week Effects",
                                     RMSE = model_5_rmse, 
                                     Difference = model_5_rmse - goal_rmse))
rmse_results%>%knitr::kable()

##Regularization
#view top 10 largest prediction "mistakes" based on our model using least squares method residuals
preds <- test_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  left_join(genre_avgs, by = "genres") %>%
  left_join(year_avgs, by = "release_year") %>%
  left_join(time_avgs, by = "date") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_r + b_t)

preds %>% group_by(title) %>% 
  summarize(n_ratings = n(), avg_rating = mean(rating), avg_pred = mean(pred)) %>% 
  mutate(residual = avg_rating - avg_pred) %>%
  arrange(desc(abs(residual))) %>%
  dplyr::select(title, n_ratings, avg_pred, residual) %>%
  slice(1:10)
#obscure movies (not familiar with titles), low number of ratings

#top 10 best movies based on model
preds %>% group_by(title) %>% 
  summarize(n_ratings = n(), avg_rating = mean(rating), avg_pred = mean(pred)) %>%
  mutate(residual = avg_rating - avg_pred) %>%
  arrange(desc(avg_pred)) %>%
  dplyr::select(title, n_ratings, avg_pred, residual) %>%
  slice(1:10)
#many obscure movies with low number of ratings and high residuals

#use cross validation to plot lambda vs rmse
lambdas <- seq(3,7,0.1)
rmses <- sapply(lambdas, function(l){
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_g <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_u - b_i - mu)/(n()+l))
  b_r <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(release_year) %>%
    summarize(b_r = sum(rating - b_g - b_u - b_i - mu)/(n()+l))
  b_t <- train_set %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_g, by="genres") %>%
    left_join(b_r, by="release_year") %>%
    group_by(date) %>%
    summarize(b_t = sum(rating - b_r - b_g - b_u - b_i - mu)/(n()+l))
  preds <- test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_r, by = "release_year") %>%
    left_join(b_t, by = "date") %>%
    mutate(pred = mu + b_i + b_u + b_r + b_g + b_t) %>%
    pull(pred)
  return(RMSE(preds, test_set$rating))
})

qplot(lambdas, rmses)  
rmse_results <- bind_rows(rmse_results,
                          data_frame(Model="Regularized Movie + User + Genre + Release Year + Time Effects",  
                                     RMSE = min(rmses), 
                                     Difference = min(rmses) - goal_rmse))
#Impact of regularization
rmse_results %>% knitr::kable()

#Top 10 movies after regularization
l <- lambdas[which.min(rmses)] #select lambda that minimizes rmse
b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))
b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))
b_g <- train_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_u - b_i - mu)/(n()+l))
b_r <- train_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  group_by(release_year) %>%
  summarize(b_r = sum(rating - b_g - b_u - b_i - mu)/(n()+l))
b_t <- train_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by="genres") %>%
  left_join(b_r, by="release_year") %>%
  group_by(date) %>%
  summarize(b_t = sum(rating - b_r - b_g - b_u - b_i - mu)/(n()+l))
preds <- test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_r, by = "release_year") %>%
  left_join(b_t, by = "date") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_r + b_t) 

preds %>% group_by(title) %>% 
  summarize(n_ratings = n(), avg_rating = mean(rating), avg_pred = mean(pred)) %>%
  mutate(residual = avg_rating - avg_pred) %>%
  arrange(desc(avg_pred)) %>%
  dplyr::select(title, n_ratings, avg_pred, residual) %>%
  slice(1:10) %>%
  kable(caption = "10 highest rated movies after regularization")

##################################################
# Results
##################################################
#update final holdout test set to match edx features
#release year
final_holdout_test <- final_holdout_test %>%
  mutate(release_year = str_extract(title, rel_yr) %>%
           as.integer()) 
#date time and week rating was given
final_holdout_test <- final_holdout_test %>% mutate(timestamp = as_datetime(timestamp)) %>%
  mutate(date = round_date(timestamp, unit = "week"))

#train final algorithm using full edx dataset
mu <- mean(edx$rating)

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

b_r <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(release_year) %>%
  summarize(b_r = sum(rating - b_u - b_i - mu)/(n()+l))

b_g <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_r, by="release_year") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_r - b_u - b_i - mu)/(n()+l))

b_t <- edx %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_r, by="release_year") %>%
  left_join(b_g, by="genres") %>%
  group_by(date) %>%
  summarize(b_t = sum(rating - b_g - b_r - b_u - b_i - mu)/(n()+l))

predicted_ratings <- final_holdout_test %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_r, by = "release_year") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_t, by = "date") %>%
  mutate(pred = mu + b_i + b_u + b_r + b_g + b_t) %>%
  pull(pred)

range(predicted_ratings)
preds_clamped <- clamp(predicted_ratings, 0.5, 5)

final_rmse <- RMSE(preds_clamped, final_holdout_test$rating)
rmse_results <- tibble(Model="Goal RMSE", RMSE = goal_rmse, Difference = 0)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(Model="Final Holdout RMSE",
                                     RMSE = final_rmse, 
                                     Difference = final_rmse - goal_rmse))
rmse_results%>%knitr::kable()

#proportion of predicted ratings that are within RMSE stars of actual ratings
mean(abs(preds_clamped - final_holdout_test$rating)<  final_rmse)*100

##################################################
# Conclusion
##################################################
#calculate rate of rating
movielens <- movielens %>% 
  mutate(release_year = str_extract(title, "(?<=\\()\\d{4}(?=\\))") %>%
           as.integer()) %>%
  add_count(title) %>% #count title
  mutate(years = 2009 - release_year, rate = n/years) # divide title count by years movie has existed

#plot rating vs rate
movielens %>% group_by(title) %>%
  summarize(n = n(), years = 2009 - first(release_year),
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

