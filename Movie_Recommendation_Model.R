#Movie Recommendation Model ---------------------------------

#Set the working directory to this files directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Datasets ---------------------------------------------------
movie_data <- read.csv("movies.csv")
ratings_data <- read.csv("ratings.csv")
#Merging the two datasets based on the movieId
merged_movie_data <- merge(movie_data,ratings_data,by = c("movieId","movieId"))
#Reshaping the merged dataframe such that each row is a vector of movie ratings from an individual user, and each column is a vector of movie ratings for a single movie
UserRatings <- reshape(merged_movie_data,idvar="userId",timevar = "movieId",drop = c("title","genres","timestamp"),direction = "wide",sep = ".")
UserRatings <- UserRatings[order(UserRatings$userId),]
row.names(UserRatings) <- NULL
#Removing all movies which had less than 50 ratings / removing all users who gave less than 30 ratings
filtercol <- apply(UserRatings,2,function(x) sum(!is.na(x)) > 50)
UserRatings <- UserRatings[,filtercol]
filterrow <- apply(UserRatings,1,function(x) sum(!is.na(x)) > 50)
UserRatings <- UserRatings[filterrow,]

#Generating a dataframe which is the transpose of UserRatings for use with the correlation function
UserRatings_t <- data.frame(t(UserRatings[-1]))
colnames(UserRatings_t) <- UserRatings[, 1]

#Building the Model ----------------------------------------
#GetSimilarity, Function which will generate a dataframe that contains the pearson correlation coefficient between each user based on their movie ratings
GetSimilarity <- function(x){
  similarity <- cor(x, method = "pearson", use = "pairwise")
  similarity[lower.tri(similarity,diag=TRUE)]=NA 
  similarity=as.data.frame(as.table(similarity)) 
  similarity=na.omit(similarity)  #Get rid of the junk we flagged above
  similarity=similarity[order(similarity$Var1,-similarity$Freq),]
  
  Output <- similarity
}
#Using the GetSimilarity function to generate the similarity dataframe for UserRatings_t
UserSimilarity <- GetSimilarity(UserRatings_t)

#GetNearestNeighbors, Function to generate a dataframe which will contain the 30 most similar users to each user
GetNearestNeighbors <- function(x){
  NN_df <- data.frame()
  for(i in unique(x$Var1)){
    sim_trim <- x[which(x$Var1 == i),]
    trim_vec <- sim_trim[1:30,2]
    NN_df <- rbind(NN_df,trim_vec)
  }
  colnames(NN_df) <- 1:30
  
  Output <- NN_df
}
#Using the GetNearestNeighbors function to generate the nearest neighbors dataframe for the Users
UserNN <- GetNearestNeighbors(UserSimilarity)

#PredictUserRatings, Function which will predict what each user would rate every movie based on the average ratings from their nearest neighbors
PredictUserRatings <- function(ratings,nn){
  prediction_df <- ratings
  for(i in 1:nrow(prediction_df)){
    for(j in 2:ncol(prediction_df)){
      selection <- as.numeric(as.vector(nn[i,]))
      avg <- mean(ratings[selection,j],na.rm = T)
      prediction_df[i,j] <- avg
    }
  }
  
  Output <- prediction_df
}
#Using the PredictUserRatings function to generate a dataframe of predicted ratings
PredictedRatings <- PredictUserRatings(UserRatings,UserNN)
#Generating a dataframe which is the transpose of PredictedRatings for use with the recommendations function
PredictedRatings_t <- data.frame(t(PredictedRatings[-1]))

#GetRecommendations, Function which will get the top 10 rating predictions for a given user, these recommendations will come from the subset of movies the user has not rated 
GetRecommendations <- function(movies, ratings, predictions, userId){
  predictions = predictions[order(predictions[,userId]),]
  x = 1
  y = 1
  rowsVect = c(1:10)
  while(x <= 10){
    rowName <- rownames(predictions)[y]
    if(is.na(ratings[userId,rowName])){
      rowsVect[x] = y
      x = x+1
    }
    y = y+1
  }
  top_vect <- attr(predictions[rowsVect,], "row.names")
  top_vect <- sub("rating.", "", top_vect, fixed = TRUE)
  matchVect <- match(as.numeric(top_vect),movies[,1])
  top_vect <- movies[matchVect,2]
  
  Output <- top_vect
}
#Example, Using the GetRecommendations function to get a vector of ten movie recommendations for user number 1
User1Recommendations <- GetRecommendations(movie_data,UserRatings,PredictedRatings_t,1)
#Example Loop to generate a dataframe of Recommendations for every user
UserRecommendations <- data.frame()
for(i in 1:nrow(UserRatings)){
  recommendations <- GetRecommendations(movie_data,UserRatings,PredictedRatings_t,i)
  UserRecommendations <- rbind(UserRecommendations,recommendations)
}
colnames(UserRecommendations) <- 1:10

#GetTopMovies, Function which will return the top 10 rated movies for a given user (from their actual ratings)
GetTopMovies <- function(movies, ratings, userId){
  ratings = ratings[order(ratings[,userId]),]
  
  top_vect <- attr(ratings[1:10,], "row.names")
  top_vect <- sub("rating.", "", top_vect, fixed = TRUE)
  matchVect <- match(as.numeric(top_vect),movies[,1])
  top_vect <- movies[matchVect,2]
  
  Output <- top_vect
}
#Example, using the GetTopMovies function to get the top rated movies for user number 1 (from their actual ratings)
User1TopMovies <- GetTopMovies(movie_data,UserRatings_t,1)

#Performance Analysis --------------------------------------------
#GetMSE, function which calculates the mean squared error between ratings predictions and actual ratings
GetMSE <- function(ratings, predictions){
  n <- 0
  SSE <- 0
  for(i in 1:nrow(ratings)){
    for(j in 2:ncol(ratings)){
      if(is.na(ratings[i,j]) | is.na(predictions[i,j])){
        next
      }
      SSE = SSE + (ratings[i,j]-predictions[i,j])^2
      n = n + 1
    }
  }
  
  Output <- SSE / n
}
#Using the GetMSE function to get the MSE for the ratings predictions
PredictionMSE <- GetMSE(UserRatings,PredictedRatings)

#Creating a binary version of the ratings (predicted and actual) for binary classification performance analysis
#Ratings above 2.75 are classified as good (1) and ratings below that are classified and bad (0)
UserRatings_b <- UserRatings
UserRatings_b[,2:ncol(UserRatings_b)] <- ifelse(UserRatings_b[,2:ncol(UserRatings_b)] > 2.75,1,0)
PredictedRatings_b <- PredictedRatings
PredictedRatings_b[,2:ncol(PredictedRatings_b)] <- ifelse(PredictedRatings_b[,2:ncol(PredictedRatings_b)] > 2.75,1,0)

#GetConfusionMatrix, function which generates a confusion matrix based on the binary predictions
GetConfusionMatrix <- function(ratings_b, predictions_b){
  TP <- 0
  FP <- 0
  TN <- 0
  FN <- 0
  for(i in 1:nrow(ratings_b)){
    for(j in 2:ncol(ratings_b)){
      if(is.na(ratings_b[i,j]) | is.na(predictions_b[i,j])){
        next
      }
      if(ratings_b[i,j] == 1){
        if(predictions_b[i,j] == 1){
          TP = TP + 1
        }else if(predictions_b[i,j] == 0){
          FN = FN + 1
        }
      }else if(ratings_b[i,j] == 0){
        if(predictions_b[i,j] == 1){
          FP = FP + 1
        }else if(predictions_b[i,j] == 0){
          TN = TN + 1
        }
      }
    }
  }
  confMatrix <- matrix(c(TP,FP,FN,TN),nrow = 2)
  colnames(confMatrix) <- c("Pred.Pos","Pred.Neg")
  rownames(confMatrix) <- c("Actual.Pos","Actual.Neg")
  accuracy <- (TP + TN)/(TP+FP+TN+FN)
  
  Output <- list(ConfusionMatrix = confMatrix, Accuracy = accuracy)
}
#Using the GetConfusionMatrix function to generate the confusion matrix for the ratings predictions
ConfusionAnalysis <- GetConfusionMatrix(UserRatings_b,PredictedRatings_b)

