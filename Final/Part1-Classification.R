# setwd("~/Code/Masters/IS621/Final/")

### Part 1 - Classification

# Load Data
data = read.csv('crime-training-data.csv', head=TRUE, sep=",")
training.df = head(data, 373)
testing.df = tail(data, 93)
private.df = read.csv('crime-evaluation-data.csv', head=TRUE, sep=",")
result.df = data.frame(Class = testing.df[,c(14)])


# Rpart Model
require(rpart)
training.rpart1 = rpart(target ~ zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv,
                        data=training.df, method="class")
plotcp(training.rpart1)
printcp(training.rpart1)

plot(training.rpart1, uniform = TRUE)
text(training.rpart1, use.n = TRUE, cex = 0.75)


# Accuracy of un-pruned model on testing data / Evaluation
predict.df = data.frame(predict(training.rpart1, newdata=testing.df, type="prob"))
names(predict.df) = c("C0","C1")
result.df$Rpart1.Class = ifelse(predict.df$C1 > predict.df$C0, "1", "0" )
result.df$Rpart1.Prob = ifelse(predict.df$C1 > predict.df$C0, predict.df$C1, predict.df$C0 )
head(result.df)
print(paste("Accuracy: ", sum(testing.df$target == result.df$Rpart1.Class) / length(testing.df$target)))



# Pruning
#training.rpart2 = prune(training.rpart1, cp = 0.05)
# printcp(training.rpart2)
# plot(training.rpart2, uniform = TRUE)
# text(training.rpart2, use.n = TRUE, cex = 0.75)

# Commenting this block because I did not prune the tree.
# predict.df = predict(training.rpart2, newdata=testing.df, type="class")
# result2.df = data.frame(class = testing.df$target, Scored.Labels = Prediction)
# head(result2.df)
# confusionMatrix(result2.df$Scored.Labels, result2.df$class)



##### C50 Model

library(C50)
#training.df <- subset(training.df, select = -c(Rpart.Class,Rpart2.Class) )
treeModel = C5.0(x=training.df[,-14], y=as.factor(training.df$target))
treeModel
summary(treeModel)
plot(treeModel)

# Accuracy / Eval test set
predict.df = data.frame(predict(treeModel, testing.df[,-14], type="prob"))
names(predict.df) = c("C0","C1")
result.df$C50.Class = ifelse(predict.df$C1 > predict.df$C0, "1", "0" )
result.df$C50.Prob = ifelse(predict.df$C1 > predict.df$C0, predict.df$C1, predict.df$C0 )
print(paste("Accuracy: ", sum(testing.df$target == result.df$C50.Class) / length(testing.df$target)))


# Results are not much different. Only one mismatch.
result.df[result.df$Rpart1.Class != result.df$C50.Class,]



### Final Predicitons

predict.df = data.frame(predict(treeModel, private.df[,-14], type="prob"))
names(predict.df) = c("C0","C1")
private.df$C50.FinalPrediction = ifelse(predict.df$C1 > predict.df$C0, "1", "0" )
private.df$C50.FinalConfidence = ifelse(predict.df$C1 > predict.df$C0, predict.df$C1, predict.df$C0 )
write.csv(private.df, "crime-evaluation-data-with-prediciton.csv")



### Analysis

# Accuracy is greater in the C50 model
library(caret)
confusionMatrix(result.df$Rpart1.Class, result.df$Class)
confusionMatrix(result.df$C50.Class, result.df$Class)