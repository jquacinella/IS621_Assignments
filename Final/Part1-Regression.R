setwd("~/Code/Masters/IS621/Final/")

### Part 1 - Regression

# Load Data
cigarette.training <- read.csv("cigarette-training-data.csv", header=TRUE)
cigarette.evaluation <- read.csv("cigarette-evaluation-data.csv", header=TRUE)

# View Data
View(cigarette.training)

# Create 7 models
models <- vector("list", 7)
models[[1]] <- lm(Sales ~ Income + Age + Price, data=cigarette.training)
models[[2]] <- lm(Sales ~ Income, data=cigarette.training)
models[[3]] <- lm(Sales ~ Age, data=cigarette.training)
models[[4]] <- lm(Sales ~ Price, data=cigarette.training)
models[[5]] <- lm(Sales ~ Income + Age, data=cigarette.training)
models[[6]] <- lm(Sales ~ Income + Price, data=cigarette.training)
models[[7]] <- lm(Sales ~ Age + Price, data=cigarette.training)

# Print R^2 and Mean of Errors Squared of all models
for (i in 1:7) {
  print(paste(summary(models[[i]])$r.squared, mean(models[[i]]$residuals^2)))
}

# Residual plots for models based on single predictor
residuals.income = resid(models[[2]])
residuals.age = resid(models[[3]])
residuals.price = resid(models[[4]])

# Income residuals
plot(cigarette.training$Income, residuals.income, ylab="Residuals", xlab="Income", main="Residuals for Income") 
abline(0, 0)

# Age Residuals
plot(cigarette.training$Age, residuals.age, ylab="Residuals", xlab="Age", main="Residuals for Age") 
abline(0, 0)

# Price Residuals
plot(cigarette.training$Price, residuals.price, ylab="Residuals", xlab="Price", main="Residuals for Price") 
abline(0, 0)


### Final evaluation
cigarette.evaluation$prediction <- predict(models[[1]], cigarette.evaluation)
write.csv(cigarette.evaluation, "cigarette-evaluation-data-with-prediction.csv")
