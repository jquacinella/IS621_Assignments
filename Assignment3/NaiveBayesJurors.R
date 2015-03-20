setwd("~/Documents/Masters/IS621//Assignment3");

# Read in data for naive bayes
naivebayes.training <- read.csv("jury-training-data.csv");
naivebayes.public <- read.csv("jury-learning-data-public.csv");
naivebayes.private <- read.csv("jury-learning-data-private.csv");

# Drop NAs
naivebayes.training <- na.omit(naivebayes.training)
naivebayes.public <- na.omit(naivebayes.public)
naivebayes.private <- na.omit(naivebayes.private)

# Implementation of Naive Bayes
naive.bayes <- function(data.training, column.class, data.testing) {
  # Pull out all possible classes
  classes <- levels(as.factor(data.training[, column.class]));
  
  numTraining = nrow(data.training)
  
  # Start the probability data frame
  prob.table <- data.frame();
  
  # Construct the columns of the probability df
  for(class in classes) { 
    # TODO: could be better
    df <- data.frame( x=numeric() );
    names(df) <- c(paste("Class", class, sep='_'));
    prob.table <- cbind(prob.table, df);
  }
  
  # Get list of variables from input data frame (excluding)
  variables <- colnames(naivebayes.training)[which(colnames(naivebayes.training) != column.class)];
  
  # Fill in table
  for(class in classes) { 
    # Number of training examples with this class value
    numClass <- sum(data.training[, column.class] == class)
    
    # Loop through possible classes and possible variable values ...
    for(variable in variables) {
      # Get a list of all possible values for this categorical data
      possible.values <- levels(as.factor(data.training[ , variable]));
      
      # For each possible value for this variable ...
      for(possible.value in possible.values) {
        # Calculate class and variable column names TODO: refactor
        classIdx <- paste("Class", class, sep='_');
        rowIdx <- paste(variable, gsub(" ", "", possible.value, fixed = TRUE), sep="_");
        
        # The probability is the number of times this variable has this value while also belonging to this class, 
        # divided by the number of times the class appears in the data
        #print(data.training[, variable] == possible.value);
        prob.table[rowIdx, classIdx] <- 
          sum(data.training[, variable] == possible.value & data.training[, column.class] == class) / numClass;
      }
    }
    
    # Calculate and store prior p(ci)
    prob.table['prior', classIdx] <- numClass / numTraining;
  }
  
  # Show final probability table
  print(prob.table);
  
  # Keep track of our predictions for each row in test data
  predictions <- rep(0, nrow(data.testing));
  
  # Classify each row in the test data
  for(i in 1:nrow(data.testing)) {
    # Tracker variables for current prediction, max probability seen and current probability
    prediction <- '';
    maxProbability <- 0;
    prob <- 0;
    for(class in classes) {
      # For this class, find the correct column in prob.table data frame
      classIdx <- paste("Class", class, sep='_');
      
      # Start the prob multiplier with the prior (based on only the class)
      prob <- prob.table["prior", classIdx];
      
      # For each variable, update prob by multiplying it with correct value from prob.table (i.e. P(value | class))
      for(variable in variables) {
        prob <- prob * prob.table[paste(variable, gsub(" ", "", data.testing[i, variable], fixed=TRUE), sep="_"), classIdx];
      }
      
      # Keep track of the class that maximizes probability
      if (prob > maxProbability) {
        maxProbability <- prob;
        prediction <- class;
      }
    }
    
    # Append our prediction to the vector of predcitions made so far
    predictions[i] <- prediction;
  }
  
  # Append predictions
  df <- data.frame( x=predictions );
  names(df) <- c("Prediction");
  data.testing <- cbind(data.testing, df);
  data.testing;
}


# Call our implementation of naive bayes
naivebayes.public.results <- naive.bayes(naivebayes.training, 'tendency', naivebayes.public);

# Print out the number of correct predictions
numCorrect <- sum(naivebayes.public.results$tendency == naivebayes.public.results$Prediction);
print(paste("Accuracy: ", ( numCorrect * 100 ) / nrow(naivebayes.public.results), "%", sep=""));

# Confusion matrix
table(naivebayes.public.results$Prediction, naivebayes.public.results$tendency)




# Call our implementation of naive bayes for private data set
naivebayes.private.results <- naive.bayes(naivebayes.training, 'tendency', naivebayes.private);
write.csv(file="jurors.private.predictions.csv", naivebayes.private.results)