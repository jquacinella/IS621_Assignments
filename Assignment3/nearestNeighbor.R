setwd("~/Documents/Masters/IS621//Assignment3");

# Read in data for k-nearest neighbors
nearestNeighbor.training <- read.csv("sample-training-data-nearest-neighbor.csv");
nearestNeighbor.testing <- read.csv("sample-testing-data-nearest-neighbor.csv");



# Our euclidean distance function
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
#euc.dist2 <- function(x1, x2) norm(as.matrix(x2 - x1), "f")

# Implementation of k-nearest-neighbors
nearestNeighbor <- function(data.training, column.class, k, data.testing) {
  # Grab list of independent variables
  variables <- colnames(data.training)[which(colnames(data.training) != column.class)];
  
  # Normalize
  for(variable in variables) {
    min <- min(data.training[ , variable]);
    max <- max(data.training[ , variable]);
    data.training[ , variable] <-  (data.training[ , variable] - min) / (max - min);
    
    min <- min(data.testing[ , variable]);
    max <- max(data.testing[ , variable]);
    data.testing[ , variable] <-  (data.testing[ , variable] - min) / (max - min);
  }
  
  predictions <- rep(0, nrow(data.testing));
  
  for(i in 1:nrow(data.testing)) {  
    dists <- c();
    
    # Create list of distances between testing row and all training rows
    for(j in 1:nrow(data.training)) {
      dist <- euc.dist(data.testing[i, variables], data.training[j, variables]);
      dists <- append(dists, dist);
    }
    
    # Sort distances, keeping the index in return as well
    res <- sort(dists, index.return = TRUE)
    
    # Find top k neighbors from our distance calculations
    potentialClasses <- c()
    for(idx in res$ix[1:k]) {
      potentialClasses <- append(potentialClasses, data.training[idx, column.class])
    }
    
    # Update our predictions based on which class appears most often
    prediction <- names(which(table(potentialClasses) == max(table(potentialClasses))))
    predictions[i] <- prediction;
  }
  
  # Append our predictions
  df <- data.frame( x=predictions );
  names(df) <- c("Prediction");
  data.testing <- cbind(data.testing, df);
  data.testing;
}


# Call our implementation of k-nearest-neighbors
k <- 3;
nearestNeighbor.testing.results <- nearestNeighbor(nearestNeighbor.training, 'Class', k, nearestNeighbor.testing);


# Print out the number of correct predictions
numCorrect <- sum(nearestNeighbor.testing.results$Class == nearestNeighbor.testing.results$Prediction);
print(paste("Accuracy: ", ( numCorrect * 100 ) / nrow(nearestNeighbor.testing.results), "%", sep=""));

table(nearestNeighbor.testing.results$Prediction, nearestNeighbor.testing.results$Class)