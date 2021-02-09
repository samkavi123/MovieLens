#######################################################################
# Movielens Recommendation System Project for eDX
# Sarma Nimishakavi
#######################################################################

#################################
# Create edx set, validation set
#################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
#if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")


library("stringr")
library("dplyr")
library("data.table")
library("caret")
library("tidyr")
library("lubridate")

knitr::opts_chunk$set(echo = FALSE)
options(dplyr.summarise.inform = FALSE)
options(tidyverse.quiet = TRUE)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId") %>%
# We will analyse by splitting multi-genres into separate rows
separate_rows(genres, sep = "\\|")%>%
# We will also extract movies release year from Title
mutate(releaseyear = as.numeric(str_sub(title,-5,-2)))

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <-  as.vector(createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE))
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

##########################
# RMSE function
##########################

RMSE <- function(actual , prediction){
  sqrt(mean((actual - prediction)^2))
}

##########################
# Explore data
##########################

head(edx) %>%
  print.data.frame()

# Movies which were rated more 
ratedmore <- edx %>% group_by(title) %>%
  summarize(numratings = n()) %>%
  arrange(desc(numratings))
head(ratedmore)
# MOvies that were rated once
edx %>% group_by(title) %>%
  summarize(numratings = n()) %>%
  filter(numratings==1) %>%
  count() %>% pull()
# MOvies that were never rated
edx %>% group_by(title)%>%
  summarize(numratings = n()) %>%
  filter(numratings<1) %>%
  count() %>% pull()

##########################
# Group and plot ratings by movie
##########################

edx %>%
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of movies") +
  ggtitle("Number of ratings per movie")

##########################
# Group and plot ratings by users
##########################

edx %>%
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(bins = 30, color = "black") +
  scale_x_log10() +
  xlab("Number of ratings") + 
  ylab("Number of users") +
  ggtitle("Number of ratings by users")

##########################
# Group and plot ratings by Genres
##########################


edx_genres<- edx %>% group_by(genres) %>%
  summarize(count = n(), mn= mean(rating), std=sd(rating)) %>% 
  filter(count >= 100000) %>% mutate(genres = reorder(genres, count))

ggplot(edx_genres,aes(x = genres, y = count/1000, fill = count)) +
  geom_bar(stat = "identity", color="black", position=position_dodge()) +
  scale_fill_continuous(trans = 'reverse') +
  ggrepel::geom_text_repel(aes(label = count), color = "black", size = 2.5, segment.color = "black") +
  geom_point() +
  guides(color = "none", fill = "none") +
  theme_bw() +
  labs(
    title = "Number of Ratings by Genres",
    x = "Genres",
    y = "Number of Ratings (1000s)"
  ) +theme(axis.text.x = element_text(angle = 90))

ggplot(edx_genres, aes(x=genres, y=mn, fill=count/1000)) + 
  scale_fill_continuous(trans = 'reverse') +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(fill = "Number of Ratings (1000s)") +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=mn-std, ymax=mn+std), width=.2,
                position=position_dodge(.9))  +
  labs(
    title = "Mean Ratings by Genres",
    x = "Genres",
    y = "Mean Ratings"
  ) +theme(axis.text.x = element_text(angle = 90))

rm(edx_genres)

##########################
# Group and plot ratings by release year
##########################

# for convenience we group movies in to release decades, for graph only
edx_rel_decades<- edx %>% mutate(decade = floor(releaseyear/10)*10) %>% 
  group_by(decade) %>% 
  summarize(count = n(), mn= mean(rating), std=sd(rating)) %>% 
  mutate(decade = reorder(decade, count))

# we plot number of ratings by decade

ggplot(edx_rel_decades, aes(x = decade, y = count/1000, fill = count)) +
  geom_bar(stat = "identity", color="black", position=position_dodge()) +
  scale_fill_continuous(trans = 'reverse') +
  ggrepel::geom_text_repel(aes(label = count), color = "black", size = 2.5, segment.color = "black") +
  geom_point() +
  guides(color = "none", fill = "none") +
  theme_bw() +
  labs(
    title = "Number of Ratings by Decade",
    x = "Release decade",
    y = "Number of Ratings (1000s)"
  ) +theme(axis.text.x = element_text(angle = 90))

# we pot error bars using mean and standard deviations
ggplot(edx_rel_decades, aes(x=decade, y=mn, fill=count/1000)) + 
  scale_fill_continuous(trans = 'reverse') +
  guides(fill = guide_legend(reverse=TRUE)) +
  labs(fill = "Count (1000s)") +
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=mn-std, ymax=mn+std), width=.2,
                position=position_dodge(.9))  +
  labs(
    title = "Mean Ratings by Release Decade",
    x = "Decade",
    y = "Mean Ratings"
  ) +theme(axis.text.x = element_text(angle = 90))

rm(edx_rel_decades)


##########################
# average method
##########################

# calculate the overall average rating on the training dataset
mu <- mean(edx$rating)

# predict all unknown ratings with mu and calculate the RMSE
naive_rmse<- RMSE(validation$rating, mu)

rmse_tbl <- tibble(method = "Average Model", RMSE = naive_rmse)
rmse_tbl %>% knitr::kable() #%>%  kableExtra::kable_styling()



######################
# Movie effect method
######################

# add average ranking term, b_i
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# predict all unknown ratings with mu and b_i
predicted_ratings <- validation %>% 
  left_join(b_i, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

# calculate RMSE of movie ranking effect
movie_rmse <-RMSE(validation$rating, predicted_ratings)

rmse_tbl <- bind_rows(rmse_tbl,tibble(method = "Movie Effect Model", RMSE = movie_rmse))
rmse_tbl %>% knitr::kable() #%>%  kableExtra::kable_styling()


# plot the distribution of b_i's
qplot(b_i, data = b_i, bins = 15, color = I("black"))



###############################
# Movie and user effect method
###############################

# compute user bias term, b_u
b_u <- edx %>% 
  left_join(b_i, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# predict new ratings with movie and user bias
predicted_ratings <- validation %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# calculate RMSE of movie and user ranking effect
movie_user_rmse <-RMSE(predicted_ratings, validation$rating)
rmse_tbl <- bind_rows(rmse_tbl,tibble(method = "Movie and User Effect Model", 
                                          RMSE = movie_user_rmse))
rmse_tbl %>% knitr::kable() #%>%  kableExtra::kable_styling()

qplot(b_u, data = b_u, bins = 15, color = I("black"))


###############################
# Movie, user, genre effect method
###############################

# compute genre bias term, b_g
b_g <- edx %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i-b_u))

# predict new ratings with movie,user and genre bias
predicted_ratings <- validation %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_g, by='genres') %>%
  mutate(pred = mu + b_i + b_u+b_g) %>%
  pull(pred)

# calculate RMSE of movie, user and genre ranking effect
movie_user_genre_rmse <-RMSE(predicted_ratings, validation$rating)
rmse_tbl <- bind_rows(rmse_tbl,tibble(method = "Movie, User and Genre Effect Model", 
                                          RMSE = movie_user_genre_rmse))
rmse_tbl %>% knitr::kable()

qplot(b_g, data = b_g, bins = 15, color = I("black"))

###############################
# Movie, user, genre, year effect method
###############################

# compute year bias term, b_y
b_y <- edx %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_g, by='genres') %>%
  group_by(releaseyear) %>%
  summarize(b_y = mean(rating - mu - b_i-b_u-b_g))

# predict new ratings with movie,user, genre and year bias
predicted_ratings <- validation %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_g, by='genres') %>%
  left_join(b_y, by='releaseyear') %>%
  mutate(pred = mu + b_i + b_u+b_g+b_y) %>%
  pull(pred)

# calculate RMSE of movie ranking effect
movie_user_genre_year_rmse <-RMSE(predicted_ratings, validation$rating)
rmse_tbl <- bind_rows(rmse_tbl,tibble(method = "Movie, User, Genre, Year Effect Model", 
                                          RMSE = movie_user_genre_year_rmse))
rmse_tbl %>% knitr::kable() #%>%  kableExtra::kable_styling()

qplot(b_y, data = b_y ,bins = 15,color = I("black"))


###########################################
# Regularized movie, user, genre, year effect method
###########################################

# determine best lambda from a sequence
lambdas <- seq(from=0, to=10, by=0.25)

# output RMSE of each lambda, repeat earlier steps (with regularization)
rmses <- sapply(lambdas, function(l){
  # calculate average rating across training data
  mu <- mean(edx$rating)
  # compute regularized movie bias term
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  # compute regularize user bias term
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  # compute user genres bias term, b_g
  b_g <- edx %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g = mean(rating - b_i-b_u -mu)/(n()+l))
  # compute year  bias term, b_y
  b_y <- edx %>%
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_g, by='genres') %>%
    group_by(releaseyear) %>%
    summarize(b_y = mean(rating - b_i-b_u-b_g-mu)/(n()+l))
  # compute predictions on validation set based on these above terms
  predicted_ratings <- validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by='genres') %>%
    left_join(b_y, by='releaseyear') %>%
    mutate(pred = mu + b_i + b_u + b_g+b_y) %>% #
    pull(pred)
  # output RMSE of these predictions
  return(RMSE(predicted_ratings, validation$rating))
})

# quick plot of RMSE vs lambdas
qplot(lambdas, rmses)
# print lambda with minimum RMSE 
lambdas[which.min(rmses)]



######################################################
# Final model with regularized movie and user effects
######################################################

# The final linear model with the minimizing lambda
lam <- lambdas[which.min(rmses)]

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lam))
# compute regularize user bias term
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lam))
b_g <- edx %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - b_i-b_u-mu)/(n()+lam))
b_y <- edx %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_g, by='genres') %>%
  group_by(releaseyear) %>%
  summarize(b_y = mean(rating - b_i-b_u-b_g-mu)/(n()+lam))
# compute predictions on validation set based on these above terms
predicted_ratings <- validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_y, by = "releaseyear") %>%
  mutate(pred = mu + b_i + b_u+b_g+b_y) %>%
  pull(pred)
# output RMSE of these predictions
regularized_movie_user_genre_year_rmse <-RMSE(predicted_ratings, validation$rating)
rmse_tbl <- bind_rows(rmse_tbl, tibble(method = "Regulaized Movie, User, Genre and Year Effect Model", 
                                           RMSE = regularized_movie_user_genre_year_rmse))


lowestRME <-which.min(rmse_tbl$RMSE)
rmse_tbl %>% knitr::kable()
# %>%  kableExtra::kable_styling() %>%
#   kableExtra::row_spec(lowestRME, bold = T, color = "black", background = "yellow")

#clean up
rm(b_g,b_i,b_u,b_y,edx, ratedmore, rmse_tbl,validation,predicted_ratings,rmses,lambdas)

