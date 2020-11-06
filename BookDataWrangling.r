#### Data Wrangling the book data ####
library(tidyverse)
library(plotmath)


# loading the data
load('data/book_ratings.RData')

#book_info <- as_tibble(book_info)
#book_ratings <- as_tibble(book_ratings)

# joining ratings with book info
book_ratings <- left_join(book_ratings, book_info)

# transpose dataset to have user ID's as rows and books as cols with ratings as obs
book_ratings_trans <- book_ratings %>%
  select(User.ID, Book.Rating, ISBN) %>%
  pivot_wider(names_from = ISBN, values_from = Book.Rating, values_fill = 0) %>%
  as.data.frame()

users.list <- as.character(book_ratings_trans$User.ID)
book_ratings_trans <- book_ratings_trans %>% select(-User.ID)
row.names(book_ratings_trans) <- as.character(users.list)
book_ratings_trans <- as.matrix(book_ratings_trans)

# create a read book data frame to show which books have been read
read_book <- book_ratings %>%
  mutate(read = ifelse(Book.Rating > 0, 1, 0)) %>%
  select(User.ID, ISBN, read) %>%
  pivot_wider(names_from = ISBN, values_from = read, values_fill = 0) %>%
  as.data.frame()

read_book <- read_book %>% select(-1)
rownames(read_book) <- as.character(users.list)
read_book <- as.matrix(read_book)

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
books_sim = cosine.sim(t(book_ratings_trans))

# predict for a single user
user_read <- book_ratings %>%
  filter(User.ID == '278137', Book.Rating > 0) %>%
  select(ISBN) %>%
  unlist() %>%
  as.character()
user_read


sort(books_sim[,user_read], decreasing = T)# sum across the rows to get score for each book

# summming across the rows
sort(apply(books_sim[,user_read], 1, sum), decreasing = T) # unread book scores

user_score <- tibble(ISBN = row.names(books_sim),
                     score = apply(books_sim[,user_read], 1, sum),
                     read = read_book['278137',])

user_score %>%
  filter(read==0) %>%
  select(-read) %>%
  arrange(desc(score))

# function that takes in a user and outputs recommended books using item-based CF
item_based_rec <- function(user, bookSimilarities, bookDB, readBooks){
  
  # ensure user is a character
  user <- ifelse(is.character(user), user, ascharacter(user))
  
  # total count of ratings per book
  totalRaters <- bookDB %>%
    filter(Book.Rating > 0) %>%
    group_by(ISBN) %>%
    count()
  
  # obtain average rating for each book per user
  b_read <- bookDB %>%
    left_join(totalRaters) %>%
    group_by(ISBN) %>%
    mutate(totRate = sum(Book.Rating)) %>%
    ungroup() %>%
    group_by(ISBN) %>%
    mutate(avgRating = totRate/n) %>%
    filter(User.ID == user, Book.Rating > 0) %>%
    select(ISBN, avgRating) #%>%
    #unlist() %>%
    #as.character()
  
  # get all books with similarity values
  read_book_avgs <- b_read$avgRating
  bookSimilarities[,b_read$ISBN]
  
  # creating table with item-based scores
  itemB_scores <- tibble(ISBN = row.names(bookSimilarities),
                        score = apply(bookSimilarities[,b_read$ISBN], 1, sum),
                        rating = mean(b_read$avgRating),
                        read = readBooks[user,])

  # organising output
  itemB_scores %>%
    filter(read==0) %>%
    select(-read) %>%
    arrange(desc(rating))
}

# recommended books for a specific user using item-based CF
view(item_based_rec('278137', books_sim, book_ratings, read_book))


#### Matrix factorization recommender system ####

# accuracy measurement to optimize (100 users and 100 books to start)
rec_accuracy <- function(x, observed_ratings){ #}, ratingDB){
  
  # extract latent parameters for users and books (using 5 latent factors)
  user_factors <- matrix(x[1:5000], 100, 5)
  book_factors <- matrix(x[5001:10000], 5, 100)
  
  # get prediction rating from dot product between user and book latent factors
  pred_rating <- user_factors %*% book_factors
  
  # calculate RMSE for optimisation
  errors <- (observed_ratings - pred_rating)^2
  
  sqrt(mean(errors[!is.na(observed_ratings)])) # optimize this. Takes into account non NA values only
}

set.seed(2020)
# optimization
opt1 <- optim(par = runif(10000), rec_accuracy,
              observed_ratings = book_ratings_trans[100,],
              control = list(maxit = 100000))

opt1$convergence

# testing the predictions
user_factors <- matrix(opt1$par[1:5000], 100, 5)
book_factors <- matrix(opt1$par[5001:10000], 5, 1000)

pred_ratings <- user_factors %*% book_factors
dim(pred_ratings)
rbind(round(pred_ratings[1,],1)[1:5], as.numeric(book_ratings_trans[1,])[1:5])
  
  
  


