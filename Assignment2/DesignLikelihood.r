### Coin Prediction using Max Likelihood from Multivariate Normal Distribution
###
### James Quacinella
### Assignment 2 - IS621

# Covariance matricies
e.cent <- matrix(c(0.0025, 0, 0, 0, 0.1452, 0, 0, 0, 0.0009), ncol=3);
e.dime <- matrix(c(0.0021, 0, 0, 0, 0.1283, 0, 0, 0, 0.0007), ncol=3);

# Covariance inverses
e.cent.inverse <- solve(e.cent);
e.dime.inverse <- solve(e.dime);

# Given means of variables per class
means.cent.mass <- 2.500;
means.cent.diameter <- 19.05;
means.cent.thickness <- 1.52;
means.dime.mass <- 2.268;
means.dime.diameter <- 17.91;
means.dime.thickness <- 1.35;

# Means as vectors
mean.cent = matrix(c(means.cent.mass, means.cent.diameter, means.cent.thickness), ncol=1)
mean.dime = matrix(c(means.dime.mass, means.dime.diameter, means.dime.thickness), ncol=1)

# Implementation of multivariate normal distribution for cents
expectation.cent <- function(x) {
  d <- nrow(x);
  return (1 / ((sqrt(2*pi))^d * sqrt(norm(e.cent))) * exp((t(x - mean.cent) %*% e.cent.inverse %*% (x - mean.cent)) / -2) );
}

# Implementation of multivariate normal distribution for dimes
expectation.dime <- function(x) {
  d <- nrow(x);
  return (1 / ((sqrt(2*pi))^d * sqrt(norm(e.dime))) * exp((t(x - mean.dime) %*% e.dime.inverse %*% (x - mean.dime)) / -2) );
}

# Function that will make a prediction based on input vector and expetations 
# from the multivariate normal distribution above
predict <- function(x) {
  x.input <- matrix(x, ncol=1);
  if(expectation.cent(x.input) > expectation.dime(x.input)) {
    "cent";
  }
  else {
    "dime";
  }
}

# Load public data
data.public <- read.csv('coin-training-data.csv', header = TRUE);
# data.public <- read.csv('test-public.csv', header = TRUE);
input <- data.public[, c("mass", "diameter", "thickness")] 

# Predict
data.public$output <- apply(input, 1, predict)
misclassified <- data.public[(data.public$output != data.public$answer), ];
print(paste("Misclassified: ", nrow(misclassified)));

View(data.public);



### PREDICTION


# Input the private test data
data.private <- read.csv('test-private.csv', header = TRUE);
input.private <- data.private[, c("mass", "diameter", "thickness")] 

# Predict
data.private$output <- apply(input.private, 1, predict)
View(data.private);

# Write output
write.csv(data.private, file="likelihood.predictions.csv")
