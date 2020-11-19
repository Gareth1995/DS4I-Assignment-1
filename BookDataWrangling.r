#### Data Wrangling the book data ####
library(tidyverse)
library(recosystem)


# loading the data
load('data/book_ratings.RData')

# joining ratings with book info
book_ratings <- left_join(book_ratings, book_info)

# extracting users that haven't read any book
book_ratings <- book_ratings %>% 
  group_by(User.ID) %>%
  mutate(totalRating = sum(Book.Rating)) %>%
  filter(totalRating > 0) %>%
  ungroup() %>%
  select(-totalRating)

# creating train and test sets
set.seed(2020)
train_index <- sample(unique(book_ratings$User.ID), size = 0.7*7503)  # 70% train

# splitting data frame
user_train <- book_ratings %>% # train set
  filter(User.ID %in% train_index)

user_test <- book_ratings %>% # test set
  filter(!(User.ID %in% train_index))

# transpose dataset to have user ID's as rows and books as cols with ratings as obs

# train
book_ratings_train <- user_train %>%
  select(User.ID, Book.Rating, ISBN) %>%
  pivot_wider(names_from = ISBN, values_from = Book.Rating, values_fill = 0) %>%
  as.data.frame()

users.list <- as.character(book_ratings_train$User.ID)
book_ratings_train <- book_ratings_train %>% select(-User.ID)
row.names(book_ratings_train) <- as.character(users.list)
book_ratings_train <- as.matrix(book_ratings_train)

# test
book_ratings_test <- user_test %>%
  select(User.ID, Book.Rating, ISBN) %>%
  pivot_wider(names_from = ISBN, values_from = Book.Rating, values_fill = 0) %>%
  as.data.frame()

users.list <- as.character(book_ratings_test$User.ID)
book_ratings_test <- book_ratings_test %>% select(-User.ID)
row.names(book_ratings_test) <- as.character(users.list)
book_ratings_test <- as.matrix(book_ratings_test)

# create a read book data frame to show which books have been read

# train
books_read_train <- user_train %>%
  mutate(read = ifelse(Book.Rating > 0, 1, 0)) %>%
  select(User.ID, ISBN, read) %>%
  pivot_wider(names_from = ISBN, values_from = read, values_fill = 0) %>%
  as.data.frame()

books_read_train <- books_read_train %>% select(-1)
rownames(books_read_train) <- rownames(book_ratings_train)
books_read_train <- as.matrix(books_read_train)

# test
books_read_test <- user_test %>%
  mutate(read = ifelse(Book.Rating > 0, 1, 0)) %>%
  select(User.ID, ISBN, read) %>%
  pivot_wider(names_from = ISBN, values_from = read, values_fill = 0) %>%
  as.data.frame()

books_read_test <- books_read_test %>% select(-1)
rownames(books_read_test) <- rownames(book_ratings_test)
books_read_test <- as.matrix(books_read_test)

# save to RData object
# ...


#### User-based recommendation ####

# calculating similarity matrix for users
# using matrix multiplication instead of loops
cosine.sim <- function(rated_movies){
  num <- as.matrix(rated_movies) %*% as.matrix(t(rated_movies))
  denom <- sqrt(diag(num)) %*% t(sqrt(diag(num)))
  #simi <- (num)/((sqrt(diag(num))) %*% (t(sqrt(num))))
  simi = num/denom
  simi <- ifelse(is.nan(simi), 0, simi)
  diag(simi) <- 0
  ans <- round(simi, 3)
  return(ans)
}
user_train_simis <- cosine.sim(book_ratings_train)

# create function to take a user, their user similarity vector and their rated movies and calculate recommendation score
user_recommendations <- function(user, user_simis, rated_books, books_read){
  
  # ensuring user is of type character
  user <- ifelse(is.character(user), user, as.character(user))
  
  # obtain K = 15 nearest neighbours
  k_nearest <- names(sort(user_simis[user,], decreasing = T)[1:15])
  
  # sum ratings of K nearest neighbours for each book
  sum_ratings <- apply(rated_books[k_nearest, ], 2, sum)
  num_ratings <- apply((rated_books[k_nearest, ] > 0), 2, sum)
  
  
  # calculating scores for books for each user
  user_scores <- data.frame(ISBN = colnames(rated_books),
                            score = as.vector(user_simis[user,] %*% rated_books),
                            rating = as.vector(sum_ratings/num_ratings),
                            read = as.vector(books_read[user,]),
                            orig_rating = as.vector(rated_books[user,]))
  
  # average original rating to apply to books that haven't been read by k nearest neighbours
  # avg_org_rating <- user_scores %>%
  #   filter(orig_rating > 0) %>%
  #   select(orig_rating)
  # 
  # avg_org_rating <- mean(avg_org_rating$orig_rating)

  # removing seen movies and sorting the rest by score
    user_scores %>%
      filter(score != 0, read == 1) %>%
      mutate(rating = ifelse(is.nan(rating), 0, rating)) %>%
      arrange(desc(rating)) %>%
      select(-read)
}


# Accuracy of user based method using train dataset
test_UB <- user_recommendations('277042', user_train_simis, book_ratings_train, books_read_train)
  
pred_trainRating_ub <- lapply(as.character(rownames(book_ratings_train)),
                       user_recommendations,
                       user_train_simis, book_ratings_train, books_read_train)

# method to extract RMSE from each user in the train set after predicting their ratings using UB CF
get_rmse <- function(aDataFrame){
  sqrdErr <- sum((aDataFrame$orig_rating - aDataFrame$rating)^2)
  meanSqrdErr <- sqrdErr/nrow(aDataFrame)
  sqrt(meanSqrdErr) # root mean squared error
}

# average RMSE over all the users in the training set
train_rmse_ub <- mean(unlist(lapply(pred_trainRating_ub, get_rmse)))

#### Item-based recommendation ####

# calculate similarity matrix between movies
books_sim = cosine.sim(t(book_ratings_train))
diag(books_sim) <- 1

# predict for a single user
# user_read <- book_ratings %>%
#   filter(User.ID == '51', Book.Rating > 0) %>%
#   select(ISBN, Book.Rating)
# view(user_read)


# # summming across the rows
# if(nrow(user_read) == 1){
#   sum_book_sims <- cbind(as.data.frame(books_sim[,user_read$ISBN]), rownames(as.data.frame(books_sim[,user_read$ISBN])))
# }else{
#   sum_book_sims <- as.data.frame(sort(apply(books_sim[,user_read$ISBN], 1, sum), decreasing = T))
#   sum_book_sims <- cbind(sum_book_sims, rownames(sum_book_sims))
# }
# 
# colnames(sum_book_sims) <- c('sim_scores','ISBN')
# sum_book_sims <- sum_book_sims %>%
#   left_join(user_read)
# 
# book_sim_wProp <- sum_book_sims %>%
#   mutate(prop = sim_scores/sum(sim_scores)) %>%
#   mutate(prop_rating = prop*1500) %>%
#   mutate(pred_rating = prop_rating/ (max(prop_rating)/max(Book.Rating, na.rm = T)))
# 
# view(book_sim_wProp) 


# function that takes in a user and outputs recommended books using item-based CF
item_based_rec <- function(user, bookSimilarities, bookDB){
  
  # ensure user is a character
  user <- ifelse(is.character(user), user, as.character(user))
  
  # the books read by a user
  user_read <- bookDB %>% 
    filter(User.ID == user, Book.Rating > 0) %>%
    rename(orig_rating = Book.Rating) %>%
    select(ISBN, orig_rating)
  
  # sum similarity scores across read books for all books in DB
  if(nrow(user_read) == 1){
    sum_book_sims <- cbind(as.data.frame(books_sim[,user_read$ISBN]), rownames(as.data.frame(books_sim[,user_read$ISBN])))
  }else{
    sum_book_sims <- as.data.frame(sort(apply(bookSimilarities[,user_read$ISBN], 1, sum), decreasing = T))
    sum_book_sims <- cbind(sum_book_sims, rownames(sum_book_sims))
  }
  
  colnames(sum_book_sims) <- c('sim_scores','ISBN')
  sum_book_sims <- sum_book_sims %>%
    left_join(user_read)
  
  # calculating ratings based on proportions of similarities
  # i.e divide all possible rating points one can give according to similarity proportion
  book_sim_wProp <- sum_book_sims %>%
    mutate(prop = sim_scores/sum(sim_scores)) %>%
    mutate(prop_rating = prop*1500) %>%
    mutate(rating = prop_rating/ (max(prop_rating)/max(orig_rating, na.rm = T)))
  
   
  # organising output
  book_sim_wProp %>%
    filter(!is.na(orig_rating)) %>%
    select(ISBN, sim_scores, rating, orig_rating) %>%
    arrange(desc(rating))
}

test_ib <- item_based_rec('277042', books_sim, user_train)

# recommended books for a specific user using item-based CF
pred_trainRating_ib <- lapply(as.character(rownames(book_ratings_train)), 
                              item_based_rec, books_sim, user_train)

# average RMSE over all the users in the training set using item based approach
train_rmse_ib <- mean(unlist(lapply(pred_trainRating_ib, get_rmse)))

#### Matrix factorization recommender system ####

# using recosystem
# train
reco_train <- user_train[,1:3] %>%
  filter(Book.Rating != 0) # only use ratings greater than 0 for optimisation

# changing ids and isbns
id_change <- rbind(unique(reco_train$User.ID), 0:(length(unique(reco_train$User.ID)) - 1))
colnames(id_change) <- id_change[1,]
id_change <- id_change[2,]

isbn_change <- rbind((reco_train$ISBN %>% unique()), 0:149)
colnames(isbn_change) <- isbn_change[1,]
isbn_change <- isbn_change[2,]

# changing the user ids and isbns according to recosystem form
reco_train <- reco_train %>%
  mutate(User.ID.reco = id_change[as.character(User.ID)],
         ISBN.reco = isbn_change[as.character(ISBN)]) %>%
  as.data.frame()

# test
reco_test <- user_test[,1:3] %>%
  filter(Book.Rating != 0) # only use ratings greater than 0 for optimisation

# changing ids and isbns
id_change <- rbind((reco_test$User.ID %>% unique()), 0:(length(unique(reco_test$User.ID)) - 1))
colnames(id_change) <- id_change[1,]
id_change <- id_change[2,]

# changing the user ids and isbns according to recosystem form
reco_test <- reco_test %>%
  mutate(User.ID.reco = id_change[as.character(User.ID)],
         ISBN.reco = isbn_change[as.character(ISBN)]) %>%
  as.data.frame()


# save data frame to disk
#write.table(book_ratings_reco, 'data/recoDat.txt', row.names = F, col.names = F)

mf_train <- data_memory(reco_train[,"User.ID.reco"], 
                        reco_train[,"ISBN.reco"],
                        reco_train[,"Book.Rating"])

mf_test <- data_memory(reco_test[,"User.ID.reco"], reco_test[,"ISBN.reco"], reco_test[,"Book.Rating"])

#train_set <- data_file('data/recoDat.txt',
#                       index1 = T)
# calling the recosystem function
r = Reco()

# train the model (no l2 regularisation or bias)
set.seed(2020)
r$train(mf_train, opts = c(dim = 20, costp_l2 = 0,
                            costq_l2 = 0, nthread = 4))


# extract the latent matrix
output_matrix <- r$output(out_P = out_memory(), out_Q = out_memory())
user_factors <- output_matrix$P
book_factors <- output_matrix$Q

train_preds <- user_factors %*% t(book_factors)
colnames(train_preds) <- unique(reco_train$ISBN)
rownames(train_preds) <- unique(reco_train$User.ID)

# original ratings
# train
reco_ratings_train <- reco_train %>%
  select(User.ID, ISBN, Book.Rating) %>%
  pivot_wider(names_from = ISBN, values_from = Book.Rating, values_fill = NA) %>%
  as.data.frame()

rownames(reco_ratings_train) <- reco_ratings_train$User.ID
reco_ratings_train <- reco_ratings_train %>% select(-1)

# train accuracy
train_rmse <- sqrt(mean((as.matrix(train_preds - reco_ratings_train)^2), na.rm = T))
train_rmse

test_pred <- r$predict(mf_test, out_memory())

#### Matrix factorization with L2 regularisation and bias ####
opts <- r$tune(mf_train, opts = c(dim = 20, costp_l1 = 0, costq_l1 = 0,
                          lrate = 0.1, nthread = 4))

set.seed(2020)
r$train(mf_train, opts = c(opts$min, nthread = 4))


#### Creating the ensamble method ####

# function that takes average across the 3 approaches
ensamble <- function(user, userSimis, bookRatings, BooksRead, bookSim, bookDB, mfMatrix){
  
  # get books read by a user
  user_read <- bookDB %>% 
    filter(User.ID == user, Book.Rating > 0) %>%
    select(ISBN) %>%
    as.data.frame()
  
  # get UB predicted rating
  user_based <- user_recommendations(user, userSimis, bookRatings, BooksRead) %>%
    arrange(desc(orig_rating)) %>%
    as.data.frame()
  
  # get IB predicted rating
  item_based <- item_based_rec(user, bookSim, bookDB) %>%
    arrange(desc(orig_rating)) %>%
    as.data.frame()

  # obtain individual book ratings and average them
  ratings <- cbind(user_based$rating, item_based$rating,
                   as.vector(sort(mfMatrix['277042', user_read$ISBN], decreasing = T)),
                   user_based$ISBN) %>%
    as.data.frame()
  colnames(ratings) <- c('UB', 'IB', 'MF', 'ISBN')
  
  #ratings
  ratings <- ratings %>%
    mutate(rating = round((as.numeric(UB)+as.numeric(IB)+as.numeric(MF))/3),1) %>%
    select(rating, ISBN)
  
  ratings <- cbind(ratings, orig_rating = bookRatings[user, user_read$ISBN])
  ratings
  
}
ensamble_pred <- ensamble('277937', user_train_simis, book_ratings_train,
                          books_read_train, books_sim, book_ratings, train_preds)

ensamble_pred

# get rmse
get_rmse(ensamble_pred)

pred_trainRating_ensamble <- lapply(as.character(rownames(book_ratings_train)),
                                    ensamble, user_train_simis, book_ratings_train,
                                    books_read_train, books_sim, book_ratings, train_preds)



# average RMSE over all the users in the training set
train_rmse_ensamble <- mean(unlist(lapply(pred_trainRating_ensamble, get_rmse)))




