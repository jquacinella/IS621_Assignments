setwd("Code/Masters/IS621/Final/")

### Part 3 - Lab

# Load data and scale it appropriately
protein <- read.table("country-protein.txt", header=TRUE)
proteinmatrix <- scale(protein[,2:10])
attr(proteinmatrix,"scaled:center")
attr(proteinmatrix,"scaled:scale")
protein.hierarchical <- hclust(distances, method="ward.D")
plot(protein.hierarchical, labels=protein$Country)

protein.kmeans <- kmeans(proteinmatrix, centers=5, iter.max=100, nstart=100)
summary(protein.kmeans)
protein.kmeans$cluster
protein.kmeans$totss
protein.kmeans$withinss
protein.kmeans$size
protein$cluster <- protein.kmeans$cluster
proteinsorted <- protein[order(protein$cluster),]
View(proteinsorted) # inspect the dataframe


### Problem

# Read data and scale
countries <- read.csv("countries-challenge-data.csv")
countriesmatrix <- scale(countries[, 2:4])

# Check sum of withinss for various kmeans models
wss <- vector("list", 10)
countries.kmeans <- vector("list", 10)
for (i in 1:10) {
  countries.kmeans[[i]] <- kmeans(countriesmatrix, centers=i, iter.max=100, nstart=100)
  wss[[i]] <- countries.kmeans[[i]]$tot.withinss
  print(paste("Kmeans(", i ,"): ", wss[[i]]))
}

# Plot elbow diagram
plot(1:10, wss, xlab="Num Clusters", ylab="Sum of withinss")
title("Elbow Diagram")

# Assign Clusers to data
#countries$cluster3 <- countries.kmeans3$cluster
#countries$cluster4 <- countries.kmeans4$cluster
#countries$cluster5 <- countries.kmeans[[5]]$cluster
countries$cluster6 <- countries.kmeans[[6]]$cluster
#countriessorted5 <- countries[order(countries$cluster5),]
#View(countriessorted5)
countriessorted6 <- countries[order(countries$cluster6),]
View(countriessorted6)
write.csv(countriessorted6, "countries-mapping.csv")




# Heirarchical clustering
countries.distances <- dist(countriesmatrix, method="euclidean")
countries.hierarchical <- hclust(countries.distances, method="ward.D")
plot(countries.hierarchical, labels=countries$country)
