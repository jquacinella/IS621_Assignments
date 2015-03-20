### Part 1 - Entropy function
entropy <- function(target) {
  # Must treat incoming vector as a factor
  target <- as.factor(target);
  
  # Calcuate entropy by looping over possible values in target
  ent <- 0;
  for (level in levels(target)) { 
    ratio <- sum(target == level) / length(target); 
    ent <- ent - ratio * log2(ratio); 
  }
  
  # Return final entropy sum
  return(ent);
}


### Part 2 - Information gain function
information.gain <- function(independent, target) {
  # Must treat incoming vector as a factor
  independent <- as.factor(independent);
  
  # Calculate entropy pre-split
  entropy.start <- entropy(target);
  
  # For each level of the independent variable, calculate entropy and add it to running sum
  entropy.new <- 0;
  for (level in levels(independent)) { 
    entropy.new <- entropy.new + (sum(independent == level) / (length(independent))) * entropy(target[independent == level]);
  }

  # Return the difference between entropy in pre-split and post-spit state
  return(entropy.start - entropy.new);
}



### Part 3 - Finding Highest Information Gain
max.gain <- function(df, target.name) {
  gains <- c();           # List os information gains per column
  gain.max <- 0;          # Maximum gain we have seen so far
  gain.max.column <- "";  # Maximum column where max gain happens
  
  # For each column, we will calculate the information again and store it in a result vector
  for(column in colnames(df)) {
    # We do not need to attempt to split over target column
    if(column != target.name) {
      # Calculate information gain if we split on this column
      gain.new <- information.gain(df[,column], df[ ,target.name]);
      
      # Update our results list
      gains <- append(gains, gain.new);
      
      # Did we find a new maximum gan? If so, update the maximum variables
      if(gain.new > gain.max) {
        gain.max <- gain.new;
        gain.max.column <- column;
      }
    }
  }
  
  # Return our results
  return(list(gains=gains, max.column=gain.max.column));
}



### Split is a util function to help split data frames on a categorical variable
split <- function(df, column.name) {
  # List of sub-dfs we generate after our split
  dfs <- list()
  
  # Create vector of column names minus the one we want to split on
  # which will be gone from the sub-dfs we create
  colnames <- colnames(df);
  colnames <- colnames[colnames != column.name];
  
  # Loop through levels of column, creating a new sub-df for that value
  count <- 1;
  for(level in levels(df[, column.name])) {
    dfs[[level]] <- df[df[, column.name] == level, colnames];
    count <- count + 1;
  }
  
  # Return list of sub dataframes
  return(dfs);
}


# Returns what the majority vote is for a result column as well as that value's probability 
# of being in the 'clashset'
majority.vote <- function(clash.set) {
  t <- table(clash.set);
  max.value <- names(which.max(t));
  prob <- t[max.value] / length(clash.set);
  return(list(max.value=max.value, prob=prob));
}


#### Juror code

# Load jury data
setwd("~/Documents/Masters/IS621//Assignment4");
jury.training <- read.csv("jury-training-data.csv");
jury.training <- na.omit(jury.training);

max.gain(jury.training, "tendency")  # outputs gender

dfs.gender <- split(jury.training, "gender")
max.gain(dfs.gender$Female, "tendency")   # outputs marital
max.gain(dfs.gender$Male, "tendency")     # outputs marital

dfs.gender.female.marital <- split(dfs.gender$Female, "marital")
dfs.gender.male.marital <- split(dfs.gender$Male, "marital")

## All three subdfs have agegroup as their next split, which leaves employment
dfs.gender.female.marital.married.agegroup <- split(dfs.gender.female.marital$Married, "agegroup");
dfs.gender.female.marital.divorced.agegroup <- split(dfs.gender.female.marital$Divorced, "agegroup");
dfs.gender.female.marital.single.agegroup <- split(dfs.gender.female.marital$Single, "agegroup");

dfs.gender.female.marital.married.agegroup.young.employment <- split(dfs.gender.female.marital.married.agegroup$'Younger Adult', "employment"); 
dfs.gender.female.marital.married.agegroup.old.employment <- split(dfs.gender.female.marital.married.agegroup$'Older Adult', "employment");
dfs.gender.female.marital.divorced.agegroup.young.employment <- split(dfs.gender.female.marital.divorced.agegroup$'Younger Adult', "employment"); 
dfs.gender.female.marital.divorced.agegroup.old.employment <- split(dfs.gender.female.marital.divorced.agegroup$'Older Adult', "employment");
dfs.gender.female.marital.single.agegroup.young.employment <- split(dfs.gender.female.marital.single.agegroup$'Younger Adult', "employment");
dfs.gender.female.marital.single.agegroup.old.employment <- split(dfs.gender.female.marital.single.agegroup$'Older Adult', "employment");



## All three subdfs have employment as their next split, which means agegroup is left
dfs.gender.male.marital.married.employment <- split(dfs.gender.male.marital$Married, "employment");
dfs.gender.male.marital.divorced.employment <- split(dfs.gender.male.marital$Divorced, "employment");
dfs.gender.male.marital.single.employment <- split(dfs.gender.male.marital$Single, "employment");


dfs.gender.male.marital.married.employment.yes.agegroup <- split(dfs.gender.male.marital.married.employment$'Employed', "agegroup"); 
dfs.gender.male.marital.married.employment.no.agegroup <- split(dfs.gender.male.marital.married.employment$'Not Employed', "agegroup");
dfs.gender.male.marital.divorced.employment.yes.agegroup <- split(dfs.gender.male.marital.divorced.employment$'Employed', "agegroup"); 
dfs.gender.male.marital.divorced.employment.no.agegroup <- split(dfs.gender.male.marital.divorced.employment$'Not Employed', "agegroup");
dfs.gender.male.marital.single.employment.yes.agegroup <- split(dfs.gender.male.marital.single.employment$'Employed', "agegroup");
dfs.gender.male.marital.single.employment.no.agegroup <- split(dfs.gender.male.marital.single.employment$'Not Employed', "agegroup");




# Final branch clash sets (and their majority votes)
majority.vote(dfs.gender.female.marital.married.agegroup.young.employment$Employed);
majority.vote(dfs.gender.female.marital.married.agegroup.young.employment$'Not Employed');
majority.vote(dfs.gender.female.marital.married.agegroup.old.employment$Employed);
majority.vote(dfs.gender.female.marital.married.agegroup.old.employment$'Not Employed');
majority.vote(dfs.gender.female.marital.divorced.agegroup.young.employment$Employed);
majority.vote(dfs.gender.female.marital.divorced.agegroup.young.employment$'Not Employed');
majority.vote(dfs.gender.female.marital.divorced.agegroup.old.employment$Employed);
majority.vote(dfs.gender.female.marital.divorced.agegroup.old.employment$'Not Employed');
majority.vote(dfs.gender.female.marital.single.agegroup.young.employment$Employed);
majority.vote(dfs.gender.female.marital.single.agegroup.young.employment$'Not Employed');
majority.vote(dfs.gender.female.marital.single.agegroup.old.employment$Employed);
majority.vote(dfs.gender.female.marital.single.agegroup.old.employment$'Not Employed');
majority.vote(dfs.gender.male.marital.married.employment.yes.agegroup$'Younger Adult'); 
majority.vote(dfs.gender.male.marital.married.employment.yes.agegroup$'Older Adult');
majority.vote(dfs.gender.male.marital.married.employment.no.agegroup$'Younger Adult');
majority.vote(dfs.gender.male.marital.married.employment.no.agegroup$'Older Adult');
majority.vote(dfs.gender.male.marital.divorced.employment.yes.agegroup$'Younger Adult'); 
majority.vote(dfs.gender.male.marital.divorced.employment.yes.agegroup$'Older Adult'); 
majority.vote(dfs.gender.male.marital.divorced.employment.no.agegroup$'Younger Adult');
majority.vote(dfs.gender.male.marital.divorced.employment.no.agegroup$'Older Adult');
majority.vote(dfs.gender.male.marital.single.employment.yes.agegroup$'Younger Adult');
majority.vote(dfs.gender.male.marital.single.employment.yes.agegroup$'Older Adult');
majority.vote(dfs.gender.male.marital.single.employment.no.agegroup$'Younger Adult');
majority.vote(dfs.gender.male.marital.single.employment.no.agegroup$'Older Adult');


# This function will take in the defendent vector and output a prediction based on the tree above
juryPredict <- function (defendent) {
  # Male Branch
  if(defendent$gender == "Male") {
    if(defendent$marital == "Married") {
      if(defendent$employment == "Employed") {
        if(defendent$agegroup == "Older Adult") {
          return(majority.vote(dfs.gender.male.marital.married.employment.yes.agegroup$'Older Adult'));
        }
        else {
          return(majority.vote(dfs.gender.male.marital.married.employment.yes.agegroup$'Younger Adult'));
        }
      }
      else {
        if(defendent$agegroup == "Older Adult") {
          return(majority.vote(dfs.gender.male.marital.married.employment.no.agegroup$'Older Adult'));
        }
        else {
          return(majority.vote(dfs.gender.male.marital.married.employment.no.agegroup$'Younger Adult'));
        }
      }
    }
    else if(defendent$marital == "Divorced") {
      if(defendent$employment == "Employed") {
        if(defendent$agegroup == "Older Adult") {
          return(majority.vote(dfs.gender.male.marital.divorced.employment.yes.agegroup$'Older Adult'));
        }
        else {
          return(majority.vote(dfs.gender.male.marital.divorced.employment.yes.agegroup$'Younger Adult'));
        }
      }
      else {
        if(defendent$agegroup == "Older Adult") {
          return(majority.vote(dfs.gender.male.marital.divorced.employment.no.agegroup$'Older Adult'));
        }
        else {
          return(majority.vote(dfs.gender.male.marital.divorced.employment.no.agegroup$'Younger Adult'));
        }
      }
    }
    else if(defendent$marital == "Single") {
      if(defendent$employment == "Employed") {
        if(defendent$agegroup == "Older Adult") {
          return(majority.vote(dfs.gender.male.marital.single.employment.yes.agegroup$'Older Adult'));
        }
        else {
          return(majority.vote(dfs.gender.male.marital.single.employment.yes.agegroup$'Younger Adult'));
        }
      }
      else {
        if(defendent$agegroup == "Older Adult") {
          return(majority.vote(dfs.gender.male.marital.single.employment.no.agegroup$'Older Adult'));
        }
        else {
          return(majority.vote(dfs.gender.male.marital.single.employment.no.agegroup$'Younger Adult'));
        }
      }
    }
  }
  # Female branch
  else {
    if(defendent$marital == "Married") {
      if(defendent$agegroup == "Older Adult") {
        if(defendent$employment == "Employed") {
          return(majority.vote(dfs.gender.female.marital.married.agegroup.old.employment$Employed));
        }
        else {
          return(majority.vote(dfs.gender.female.marital.married.agegroup.old.employment$'Not Employed'));
        }
      }
      else {
        if(defendent$employment == "Employed") {
          return(majority.vote(dfs.gender.female.marital.married.agegroup.young.employment$Employed));
        }
        else {
          return(majority.vote(dfs.gender.female.marital.married.agegroup.young.employment$'Not Employed'));
        }
      }
    }
    else if(defendent$marital == "Divorced") {
      if(defendent$agegroup == "Older Adult") {
        if(defendent$employment == "Employed") {
          return(majority.vote(dfs.gender.female.marital.divorced.agegroup.old.employment$Employed));
        }
        else {
          return(majority.vote(dfs.gender.female.marital.divorced.agegroup.old.employment$'Not Employed'));
        }
      }
      else {
        if(defendent$employment == "Employed") {
          return(majority.vote(dfs.gender.female.marital.divorced.agegroup.young.employment$Employed));
        }
        else {
          return(majority.vote(dfs.gender.female.marital.divorced.agegroup.young.employment$'Not Employed'));
        }
      }
    }
    else if(defendent$marital == "Single") {
      if(defendent$agegroup == "Older Adult") {
        if(defendent$employment == "Employed") {
          return(majority.vote(dfs.gender.female.marital.single.agegroup.old.employment$Employed));
        }
        else {
          return(majority.vote(dfs.gender.female.marital.single.agegroup.old.employment$'Not Employed'));
        }
      }
      else {
        if(defendent$employment == "Employed") {
          return(majority.vote(dfs.gender.female.marital.single.agegroup.young.employment$Employed));
        }
        else {
          return(majority.vote(dfs.gender.female.marital.single.agegroup.young.employment$'Not Employed'));
        }
      }
    }
  }
}




# Read in private juror data
jury.public <- read.csv("jury-learning-data-public.csv");
jury.public <- na.omit(jury.public);

# Using the above function, figure out how many we correctly predict
jury.public.predictions <- list()
for(i in 1:nrow(jury.public)) {
  jury.public.predictions[[i]] <- juryPredict(jury.public[i, ])$max.value
}

# Results show 68% accuracy
print(paste("On public data set, we get an accuracy of ", 100 * sum(jury.public.predictions == jury.public$tendency) / length(jury.public.predictions), "%"));




# Private juror data
jury.private <- read.csv("jury-learning-data-private.csv");
jury.private <- na.omit(jury.private);

# Using the above function, predict on the private data set
jury.private.predictions <- list()
for(i in 1:nrow(jury.private)) {
  jury.private.predictions[[i]] <- juryPredict(jury.private[i, ])$max.value
}

# Save predictions
jury.private <- cbind(jury.private, predictions=unlist(jury.private.predictions))
write.csv(file="jury.private.predicitons.csv", x=jury.private)





#### Using C50 Module
require("C50");
treeModel <- C5.0(x = jury.training[, -5], y = jury.training$tendency);
treeModel;
summary(treeModel);

ruleModel <- C5.0(x = jury.training[, -5], y = jury.training$tendency, rules=TRUE);
ruleModel;
summary(ruleModel);

# Predict the public data set to get a sense of its accuracy
c50.predictions <- predict.C5.0(ruleModel, jury.public);

# Results show 74% accuracy
print(paste("On the public data set, C5.0 gets an accuracy of ", 100 * sum(c50.predictions == jury.public$tendency) / length(jury.public$tendency), "%"));







#### Using rpart module
require("rpart");
fit <- rpart(tendency ~ gender + marital + agegroup + employment, 
             data = jury.training);

# Predict the public data set to get a sense of its accuracy
rpart.predictions <- predict(fit, jury.public, type = "class");

# Results show 66.38% accuracy
print(paste("On the public data set, C5.0 gets an accuracy of ", 100 * sum(rpart.predictions == jury.public$tendency) / length(jury.public$tendency), "%"));


# Save tree to PNG file
png(filename="tree.png")
plot(fit);
text(fit, use.n = TRUE);
dev.off()

