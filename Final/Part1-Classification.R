setwd("Code/Masters/IS621/Final/")

### Part 1 - Classification

# Load Data
crime.training <- read.table("crime-training-data.csv", header=TRUE)
crime.evaluation <- read.table("crime-evaluation-data.csv", header=TRUE)

## COPY FROM SHARAD