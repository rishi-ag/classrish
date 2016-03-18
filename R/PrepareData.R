# Preprocesses data based on input
# ----------------------------------------------------------------------
#' Preprocess Data
#'
#' Preprocesses data based on input
#'
#' @param path provides path to training and test data
#' @param mode Defines the kind of preprocessing to be done. mode 0 does not process any data but seperates features from labels and returns 
#' a training set and a test set. mode1 creates z scores out of all the continuous features in training data set
#' For test data set the training sample means and sample variances are used to normalize data. 
#' mode 2 resizes all continuous features between 0 and 1
#' @param sample If sample is set to TRUE only a subset of processed data is returned
#' @param size The size of subset of data to be returned of \code{sample} set to TRUE
#' @return A list with two components.
#' train - A list with two components. One is a vector of labels, the other is a data frame of features.
#' test - If mode is not 1 A data frame of features. Else a list where one component is the sample mean 
#' and sample sd used to normalize data and the other component is the list of test features
#' @export
#' @examples
#' path <- "/home/rishabh/mres/ml_comp/data/"
#' data <- PrepareData(path, mode = 2, sample = TRUE, size = 100)


PrepareData <- function(path, mode = 0, sample = FALSE, size = 1000){
   
  if(sample) {
    train <- read.csv(paste0(path, "news_popularity_training.csv"), header = T)[1:size,]
    test <- read.csv(paste0(path, "news_popularity_test.csv"), header = T)[1:size,]
  } else {
    train <- read.csv(paste0(path, "news_popularity_training.csv"), header = T)
    test <- read.csv(paste0(path, "news_popularity_test.csv"), header = T)
  }
  
  
  train <- list(label = factor(train$popularity), 
                features = subset(train, select=-c(popularity, url, id)))
  test <- subset(test, select=-c(url, id))
  
  
  if ( mode == 0) {
    train$features <- cbind(train$features[,-c(13:18, 31:38)], train$features[,c(13:18, 31:38)])
    test <- cbind(test[,-c(13:18, 31:38)], test[,c(13:18, 31:38)])
    
  }else if ( mode == 1) {
    sep.feat.list <- SepFeatures(train$features)
    train$features <- sep.feat.list$features
    test <- SepFeatures(test,sep.feat.list$sample.stats)
  }
  
  else if ( mode == 2) {
    resized.feat <- ResizeFeatures(train$features, test)
    train$features <- resized.feat$train.feat
    test <- resized.feat$test.feat
  }
  return(list(train = train,
              test = test)) 
}

# Seperates features into continuous and categorical
# ----------------------------------------------------------------------
#' Seperate Features into continuous and categorical
#'
#' Seperate Features into continuous and categorical
#'
#' @param features A dataframe of features
#' @param sample.stats If samplle.stats is provided it will normalise features given those sample stats
#' @return A list with two components. Features and sample stats

SepFeatures <- function(features, sample.stats = NA){
  #' normalises continous features
  
  feat.fac <- features[,c(13:18, 31:38)]
  
  feat.continuous <- features[,-c(13:18, 31:38)]
  
  if(is.na(sample.stats)) {
    sample.stats <- data.frame(vec.mean = double(),
                               vec.sd = double())
    
    for (colname in names(feat.continuous)) {
      normalised.list <- Normalise(feat.continuous[[colname]])
      feat.continuous[[colname]] <- normalised.list$vector
      sample.stats <- rbind(sample.stats, normalised.list$stats)
    }
    
    for (colname in names(feat.fac)) {
      feat.fac[[colname]] <- as.factor(feat.fac[[colname]])
    }
    
  } else {
    
    for (i in 1:length(names(feat.continuous))) {
      normalised.list <- Normalise(feat.continuous[,i], sample.stats[i,1], sample.stats[i,2])
      feat.continuous[,i] <- normalised.list$vector
      
    }
    
    for (colname in names(feat.fac)) {
      feat.fac[[colname]] <- as.factor(feat.fac[[colname]])
    }
  }
  
  
  
  return(list(features = cbind(feat.continuous, feat.fac),
              sample.stats = sample.stats)) 
}

# Normalises data
# ----------------------------------------------------------------------
#' Normalises data
#'
#' Normalises data
#'
#' @param vector A feature
#' @param vec.mean Is the sample mean if provided
#' @param vec.sd Is the sample sd if provided
#' @return A list of normalised vector and sample stats
#' @examples 
#' vec <- Normalise(rnorm(100), 0.1, 1.1)
Normalise <- function(vector, vec.mean = NA, vec.sd = NA){
  if(is.na(vec.mean)) vec.mean <- mean(vector)
  if(is.na(vec.sd)) vec.sd <- sd(vector)
  return(list(vector = (vector -vec.mean)/vec.sd,
              stats = c(vec.mean, vec.sd)))
} 


# Resize Features
# ----------------------------------------------------------------------
#' Resize Features
#'
#' Resize Features
#'
#' @param train.feat A training data frame
#' @param test.feat A test data frame 
#' @return list of training and test features

ResizeFeatures <- function(train.feat, test.feat){
  #' normalises continous features between 0 and 1
  
  train.feat.fac <- train.feat[,c(13:18, 31:38)]
  train.feat.continuous <- train.feat[,-c(13:18, 31:38)]
  
  test.feat.fac <- test.feat[,c(13:18, 31:38)]
  test.feat.continuous <- test.feat[,-c(13:18, 31:38)]
  
  
  for (colname in names(train.feat.continuous)) {
    vec.min <- min(min(train.feat.continuous), min(test.feat.continuous))
    vec.max <- max(max(train.feat.continuous), max(test.feat.continuous))
    train.feat.continuous[[colname]] <- Resize(train.feat.continuous[[colname]], 
                                               vec.min, vec.max)
    test.feat.continuous[[colname]] <- Resize(test.feat.continuous[[colname]], 
                                              vec.min, vec.max)
  }
  
  return(list(train.feat = cbind(train.feat.continuous, train.feat.fac),
              test.feat = cbind(test.feat.continuous, test.feat.fac))) 
}


# Resize vector
# ----------------------------------------------------------------------
#' Resize vector
#'
#' Resize vector
#'
#' @param vector A feature
#' @param vec.min Minimum value of feature
#' @param  vec.max Maximum value of feature 
#' @return Resized vector
#' @examples
#' Resize(rnorm(100, 5, 1), 2, 6)  

Resize <- function(vector, vec.min, vec.max) {
  return((vector - vec.min)/ (vec.max - vec.min))
}