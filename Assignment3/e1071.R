require(e1071);

setwd("~/Documents/Masters/IS621//Assignment3");

# Read in data for naive bayes
naivebayes.training <- read.csv("sample-training-data-naive-bayes.csv");
naivebayes.testing <- read.csv("sample-testing-data-naive-bayes.csv");

# Need to convert class column to a factor
naivebayes.training$Class <- as.factor(naivebayes.training$Class)
naivebayes.testing$Class <- as.factor(naivebayes.testing$Class)

# Create a naive bayes model and use it to predict our testing data
model <- naiveBayes(Class ~ ., data = naivebayes.training)
pred <- predict(model, naivebayes.testing, type="class")

# Same confusion matrix as custom implementation
table(pred, naivebayes.testing$Class)