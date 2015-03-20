require(class);

setwd("~/Documents/Masters/IS621//Assignment3");

# Read in data for k-nearest neighbors
nearestNeighbor.training <- read.csv("sample-training-data-nearest-neighbor.csv");
nearestNeighbor.testing <- read.csv("sample-testing-data-nearest-neighbor.csv");

variables <- colnames(nearestNeighbor.training)[which(colnames(nearestNeighbor.training) != 'Class')];

# Normalize
for(variable in variables) {
  min <- min(nearestNeighbor.training[ , variable]);
  max <- max(nearestNeighbor.training[ , variable]);
  nearestNeighbor.training[ , variable] <-  (nearestNeighbor.training[ , variable] - min) / (max - min);
  
  min <- min(nearestNeighbor.testing[ , variable]);
  max <- max(nearestNeighbor.testing[ , variable]);
  nearestNeighbor.testing[ , variable] <-  (nearestNeighbor.testing[ , variable] - min) / (max - min);
}

# Need to convert class column to a factor
nearestNeighbor.training$Class <- as.factor(nearestNeighbor.training$Class)
nearestNeighbor.testing$Class <- as.factor(nearestNeighbor.testing$Class)

# Loop over k values from 3 to 13
# NOTE: looks like k = 3 gives best accuracy on test set
for(currK in seq(3,13,2)) {
  # Print confusion matrix
  predictions <- knn(nearestNeighbor.training[, variables], nearestNeighbor.testing[, variables], nearestNeighbor.training$Class, k = currK, prob=TRUE)
  table(nearestNeighbor.testing[, 'Class'], predictions)
  
  # Print out the number of correct predictions
  numCorrect <- sum(nearestNeighbor.testing$Class == predictions);
  print(paste("Accuracy: ", ( numCorrect * 100 ) / nrow(nearestNeighbor.testing), "%", sep=""));
}