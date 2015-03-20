require(class);

setwd("~/Documents/Masters/IS621//Assignment3");

# Read in data for k-nearest neighbors
nearestNeighbor.training <- read.csv("pima-training-data.csv");
nearestNeighbor.public <- read.csv("pima-learning-data-public.csv");
nearestNeighbor.private <- read.csv("pima-learning-data-private.csv");

# Need to convert class column to a factor
nearestNeighbor.training$class <- as.factor(nearestNeighbor.training$class);
nearestNeighbor.public$class <- as.factor(nearestNeighbor.public$class);

variables <- colnames(nearestNeighbor.training)[which(colnames(nearestNeighbor.training) != 'class')];

# Normalize
for(variable in variables) {
  min <- min(nearestNeighbor.training[ , variable]);
  max <- max(nearestNeighbor.training[ , variable]);
  nearestNeighbor.training[ , variable] <-  (nearestNeighbor.training[ , variable] - min) / (max - min);
  
  min <- min(nearestNeighbor.public[ , variable]);
  max <- max(nearestNeighbor.public[ , variable]);
  nearestNeighbor.public[ , variable] <-  (nearestNeighbor.public[ , variable] - min) / (max - min);
  
  min <- min(nearestNeighbor.private[ , variable]);
  max <- max(nearestNeighbor.private[ , variable]);
  nearestNeighbor.private[ , variable] <-  (nearestNeighbor.private[ , variable] - min) / (max - min);
}

# Loop over k values from 3 to 13
for(currK in seq(3,13,2)) {
  # Print confusion matrix
  predictions <- knn(nearestNeighbor.training[, variables], nearestNeighbor.public[, variables], nearestNeighbor.training$class, k = currK, prob=TRUE)
  table(nearestNeighbor.public[, 'class'], predictions);
  
  # Print out the number of correct predictions
  numCorrect <- sum(nearestNeighbor.public$class == predictions);
  print(paste("For k = ", currK, ", we get an accuracy of: ", ( numCorrect * 100 ) / nrow(nearestNeighbor.public), "%", sep=""));

  # Confusion Matrix
  #print(table(predictions, nearestNeighbor.public$class));
}

predictions <- knn(nearestNeighbor.training[, variables], nearestNeighbor.private[, variables], nearestNeighbor.training$class, k = currK, prob=TRUE);
nearestNeighbor.private <- cbind(nearestNeighbor.private, prediction=predictions);
write.csv(file="pima.private.predictions.csv", nearestNeighbor.private);
