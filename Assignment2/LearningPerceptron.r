### Coin Prediction using Perceptron
###
### James Quacinella
### Assignment 2 - IS621

# Initialize state
N <- 4;        # Number of dimensions
w <- rep(0, N) # Weight for perceptron, starting at zero vector

# Solution found after equilibrium
w <- c(2597.10000,  -878.00639,    94.24669, -1566.20134)

##### Training

convertCoinToInt <- function(row) {if(row['answer']=='dime'){1;} else {-1}}
convertIntToCoin <- function(row) {if(row['prediction']==1){'dime';} else {'cent'}}


# Read training data
setwd("~/Code/Masters/IS621/Assignment2/")
data.training <- read.csv('coin-training-data.csv', header = TRUE);

# Add bias input
data.training$bias <- rep(1, nrow(data.training))

# Convert answer to -1 and 1 for cent and dime respectfully
data.training$answer <- apply(data.training, 1, convertCoinToInt)

# Grab our input matrix
input.training <- data.training[c("bias", "mass", "diameter", "thickness")]

count <- 0;
while(count < 500000) {
  # Create an output column, which is our perceptron output
  data.training$output <- apply(input.training, 1, function(row) sign(sum(row * w)))  
  
  # Find all misclassified rows and pick on at random
  misclassified <- data.training[(data.training$output != data.training$answer), ];
  retrain.idx <- sample(nrow(misclassified), 1);
  retrain.input <- misclassified[retrain.idx, c("bias", "mass", "diameter", "thickness")];
  retrain.correct <- misclassified[retrain.idx, c("answer")];
  
  # Checkup on output every once in a while
  if (count %% 1 == 0) {
    print(paste("Misclassified: ", nrow(misclassified)));
    print(w);
  }
  
  # Check if we have no more misclassified inputs; if so, we are done
  if (nrow(misclassified) == 0) {
    break;
  }
  
  # Otherwise, update weights according to perceptron learning
  w <- as.numeric(w + retrain.correct * retrain.input);
  
  # Keep track of number of times thru this
  count <- count + 1;
}

# Save weights as our new model
model <- w


### TESTING


# Input the test data
data.test <- read.csv('test-public.csv', header = TRUE);

# Add bias input to the test data
data.test$bias <- rep(1, nrow(data.test))

# Convert answer to -1 and 1 for cent and dime respectfully
data.test$answer <- apply(data.test, 1, function(row) {if(row['answer']=='dime'){1;} else {-1}})

# Grab our input test matrix
input.test <- data.test[c("bias", "mass", "diameter", "thickness")]

# Create an output column, which is our perceptron output
data.test$output <- apply(input.test, 1, function(row) sign(sum(row * w)))  
  
# Find all misclassified rows and pick on at random
model.performance <- data.training[(data.test$output != data.test$answer), ];
print(paste("Error rate:", nrow(model.performance) / nrow(data.test), '%'))


### PREDICTION


# Input the private test data
data.predict <- read.csv('test-private.csv', header = TRUE)

# Add bias input to the test data
data.predict$bias <- rep(1, nrow(data.predict))

# Grab our input test matrix
input.predict <- data.predict[c("bias", "mass", "diameter", "thickness")]

# Create an output column, which is our perceptron output
data.predict$prediction <- apply(input.predict, 1, function(row) sign(sum(row * w)))

# Convert answer from -1 and 1 to cent and dime, respectfully
data.predict$prediction <- apply(data.predict, 1, convertIntToCoin)

# Output prediction data frame, where $prediction is our predicition
data.predict$prediction

# Save as CSV file
write.csv(data.predict, file="perceptron.predictions.csv")
