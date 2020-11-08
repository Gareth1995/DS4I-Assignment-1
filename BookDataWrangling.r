#### Data Wrangling the book data ####
library(tidyverse)
library(recosystem)


# loading the data
load('data/book_ratings.RData')

# joining ratings with book info
book_ratings <- left_join(book_ratings, book_info)

# creating train and test sets
set.seed(2020)
train_index <- sample(unique(book_ratings$User.ID), size = 0.7*10000)  # 70% train

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
user.simis <- cosine.sim(book_ratings_trans)

# score <- as.vector(user.simis["276925", ]) %*% book_ratings_trans
# scoreDat <- as.data.frame(t(score))
# colnames(scoreDat) <- c("score")
# scoreDat %>% filter(score != 0) %>% arrange(desc(score))

# create function to take a user, their user similarity vector and their rated movies and calculate recommendation score
user_recommendations <- function(user, user_simis, rated_books, books_read){
  
  # ensuring user is of type character
  user <- ifelse(is.character(user), user, as.character(user))
  
  # obtain K = 5 nearest neighbours
  k_nearest <- names(sort(user_simis[user,], decreasing = T)[1:32])
  
  # sum ratings of K nearest neighbours for each book
  sum_ratings <- apply(rated_books[k_nearest, ], 2, sum)
  num_ratings <- apply((rated_books[k_nearest, ] > 0), 2, sum)
  
  
  # calculating scores for books for each user
  user_scores <- data.frame(ISBN = colnames(rated_books),
                            score = as.vector(user_simis[user,] %*% rated_books),
                            rating = as.vector(sum_ratings/num_ratings),
                            read = as.vector(books_read[user,]))
  
  # removing seen movies and sorting the rest by score
    user_scores %>%
      filter(score != 0, read == 0) %>%
      mutate(rating = ifelse(is.nan(rating), NA, rating)) %>%
      arrange(desc(score)) %>%
      select(-read)
}


# calculate a score value(based on rating and user similarity) to weight movie rec
scrs <- user_recommendations('278418', user.simis, book_ratings_trans, read_book)
scrs # NA indicate not enough data to predict the rating for the specific book for the specific user

#### Item-based recommendation ####

# calculate similarity matrix between movies
books_sim = cosine.sim(t(book_ratings_train))

# predict for a single user
user_read <- book_ratings %>%
  filter(User.ID == '277042', Book.Rating > 0) %>%
  select(ISBN, Book.Rating) #%>%
  #unlist() %>%
  #as.character()
view(user_read)


#view(sort(books_sim[,user_read$ISBN], decreasing = T))# sum across the rows to get score for each book
#view(books_sim[,user_read$ISBN])
# summming across the rows
sum_book_sims <- as.data.frame(sort(apply(books_sim[,user_read$ISBN], 1, sum), decreasing = T))
sum_book_sims <- cbind(sum_book_sims, rownames(sum_book_sims))

colnames(sum_book_sims) <- c('sim_scores','ISBN')
sum_book_sims <- sum_book_sims %>%
  left_join(user_read)

book_sim_wProp <- sum_book_sims %>%
  mutate(prop = sim_scores/sum(sim_scores)) %>%
  mutate(prop_rating = prop*1500) %>%
  mutate(pred_rating = prop_rating/ (max(prop_rating)/max(Book.Rating, na.rm = T)))

view(book_sim_wProp) 


# function that takes in a user and outputs recommended books using item-based CF
item_based_rec <- function(user, bookSimilarities, bookDB){
  
  # ensure user is a character
  user <- ifelse(is.character(user), user, ascharacter(user))
  
  # the books read by a user
  user_read <- bookDB %>%
    filter(User.ID == user, Book.Rating > 0) %>%
    select(ISBN, Book.Rating)
  
  # sum similarity scores across read books for all books in DB
  sum_book_sims <- as.data.frame(sort(apply(bookSimilarities[,user_read$ISBN], 1, sum), decreasing = T))
  sum_book_sims <- cbind(sum_book_sims, rownames(sum_book_sims))
  
  colnames(sum_book_sims) <- c('sim_scores','ISBN')
  sum_book_sims <- sum_book_sims %>%
    left_join(user_read)
  
  # calculating ratings based on proportions of similarities
  # i.e divide all possible rating points one can give according to similarity proportion
  book_sim_wProp <- sum_book_sims %>%
    mutate(prop = sim_scores/sum(sim_scores)) %>%
    mutate(prop_rating = prop*1500) %>%
    mutate(pred_rating = prop_rating/ (max(prop_rating)/max(Book.Rating, na.rm = T)))
  

  # organising output
  book_sim_wProp %>%
    filter(is.na(Book.Rating)) %>%
    select(ISBN, sim_scores, pred_rating) %>%
    arrange(desc(sim_scores))
}

# recommended books for a specific user using item-based CF
view(item_based_rec('277042', books_sim, book_ratings))


#### Matrix factorization recommender system ####

# accuracy measurement to optimize (100 users and 100 books to start)
# rec_accuracy <- function(x, observed_ratings){ #}, ratingDB){
#   
#   # extract latent parameters for users and books (using 5 latent factors)
#   user_factors <- matrix(x[1:50], 10, 5) # using 5 latent factors and 10 users
#   book_factors <- matrix(x[51:175], 5, 25) # 5 latent factors for 25 books
#   
#   # get prediction rating from dot product between user and book latent factors
#   pred_rating <- user_factors %*% book_factors
#   
#   # calculate RMSE for optimisation
#   errors <- (observed_ratings - pred_rating)^2
#   
#   sqrt(mean(errors[!is.na(observed_ratings)])) # optimize this. Takes into account non NA values only
# }
# 
# set.seed(2020)
# # optimization
# opt1 <- optim(par = runif(175), rec_accuracy,
#               observed_ratings = book_ratings_trans[1:10,1:25],
#               control = list(maxit = 100000))
# 
# opt1$convergence
# opt1$par
# 
# # testing the predictions
# user_factors <- matrix(opt1$par[1:50], 10, 5)
# book_factors <- matrix(opt1$par[51:175], 5, 25)
# 
# pred_ratings <- user_factors %*% book_factors
# dim(pred_ratings)
# rbind(round(pred_ratings[8,],1)[1:5], as.numeric(book_ratings_trans[8,])[1:5])

# using recosystem
# train
reco_train <- user_train[,1:3] %>%
  filter(Book.Rating != 0) # only use ratings greater than 0 for optimisation

# changing ids and isbns
id_change <- rbind((reco_train$User.ID %>% unique()), 0:5218)
colnames(id_change) <- id_change[1,]
id_change <- id_change[2,]

isbn_change <- rbind((reco_train$ISBN %>% unique()), 0:149)
colnames(isbn_change) <- isbn_change[1,]
isbn_change <- isbn_change[2,]

# changing the user ids and isbns according to recosystem form
reco_train <- reco_train %>%
  mutate(User.ID.reco = id_change[as.character(User.ID)],
         ISBN.reco = isbn_change[as.character(ISBN)]) #%>%
  #select(User.ID.reco, ISBN.reco, Book.Rating)

# test
reco_test <- user_test[,1:3] %>%
  filter(Book.Rating != 0) # only use ratings greater than 0 for optimisation

# changing ids and isbns
id_change <- rbind((reco_test$User.ID %>% unique()), 0:2283)
colnames(id_change) <- id_change[1,]
id_change <- id_change[2,]

# changing the user ids and isbns according to recosystem form
reco_test <- reco_test %>%
  mutate(User.ID.reco = id_change[as.character(User.ID)],
         ISBN.reco = isbn_change[as.character(ISBN)]) #%>%
  #select(User.ID.reco, ISBN.reco, Book.Rating)


# save data frame to disk
#write.table(book_ratings_reco, 'data/recoDat.txt', row.names = F, col.names = F)

mf_train <- data_memory(reco_train[,"User.ID.reco"], reco_train[,"ISBN.reco"], reco_train[,"Book.Rating"])
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





