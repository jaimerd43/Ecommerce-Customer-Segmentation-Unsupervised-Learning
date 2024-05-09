#set seed
set.seed(40425150)

#hierarchical clustering whit 4 linkage methods -----
hclust<- hclust(dist(scale(cbind(new_data$Avg_Quantity, new_data$Total_Quantity, new_data$total_value, new_data$Avg_UnitPrice, new_data$Avg_ReturnRate))), method = "complete")
hclust1<- hclust(dist(scale(cbind(new_data$Avg_Quantity, new_data$Total_Quantity, new_data$total_value, new_data$Avg_UnitPrice, new_data$Avg_ReturnRate))), method = "single")
hclust2<- hclust(dist(scale(cbind(new_data$Avg_Quantity, new_data$Total_Quantity, new_data$total_value, new_data$Avg_UnitPrice, new_data$Avg_ReturnRate))), method = "centroid")
hclust3<- hclust(dist(scale(cbind(new_data$Avg_Quantity, new_data$Total_Quantity, new_data$total_value, new_data$Avg_UnitPrice, new_data$Avg_ReturnRate))), method = "average")

#different nstart values 
nstart_values <- c(10, 50, 100)

x <- c(1:10)

#for complete method---
plot(hclust)
y <- sort(hclust$height, decreasing = TRUE)[1:10]
plot(x,y); lines(x,y, col= "blue")

results <- vector("list", length = 3)
for (i in 1:length(nstart_values)) {
  seg_kmeans <- kmeans(x = data.frame(new_data$Avg_Quantity, new_data$Total_Quantity, new_data$total_value, new_data$Avg_UnitPrice, new_data$Avg_ReturnRate), centers = 8, nstart = nstart_values[i])
  results[[i]] <- seg_kmeans
}

# Comparing results
for (i in 1:length(results)) {
  cat("Results for nstart =", nstart_values[i], ":\n")
  print(results[[i]])
  cat("\n")
}


#for single method
plot(hclust1)
y <- sort(hclust1$height, decreasing = TRUE)[1:10]
plot(x,y); lines(x,y, col= "blue")

results1 <- vector("list", length = 3)
for (i in 1:length(nstart_values)) {
  seg_kmeans1 <- kmeans(x = data.frame(new_data$Avg_Quantity, new_data$Total_Quantity, new_data$total_value, new_data$Avg_UnitPrice, new_data$Avg_ReturnRate), centers = 6, nstart = nstart_values[i])
  results1[[i]] <- seg_kmeans1
}

# Comparing results
for (i in 1:length(results1)) {
  cat("Results for nstart =", nstart_values[i], ":\n")
  print(results1[[i]])
  cat("\n")
  cat("Results for nstart =", nstart_values[i], ":\n")
  cat("tot.withinss:", results1[[i]]$tot.withinss, "\n\n")
}


## for centroid method 

plot(hclust2)
y <- sort(hclust2$height, decreasing = TRUE)[1:10]
plot(x,y); lines(x,y, col= "blue")

results2 <- vector("list", length = 3)
for (i in 1:length(nstart_values)) {
  seg_kmeans2 <- kmeans(x = data.frame(new_data$Avg_Quantity, new_data$Total_Quantity, new_data$total_value, new_data$Avg_UnitPrice, new_data$Avg_ReturnRate), centers = 5, nstart = nstart_values[i])
  results2[[i]] <- seg_kmeans2
}

# Comparing results
for (i in 1:length(results2)) {
  cat("Results for nstart =", nstart_values[i], ":\n")
  print(results2[[i]])
  cat("\n")
  cat("Results for nstart =", nstart_values[i], ":\n")
  cat("tot.withinss:", results2[[i]]$tot.withinss, "\n\n")
}


## for average method 

plot(hclust3)
y3 <- sort(hclust3$height, decreasing = TRUE)[1:10]
plot(x,y3); lines(x,y3, col= "blue")

results3 <- vector("list", length = 3)
for (i in 1:length(nstart_values)) {
  seg_kmeans3 <- kmeans(x = data.frame(new_data$Avg_Quantity, new_data$Total_Quantity, new_data$total_value, new_data$Avg_UnitPrice, new_data$Avg_ReturnRate), centers = 4, nstart = nstart_values[i])
  results3[[i]] <- seg_kmeans3
}

seg_kmeans3$tot.withinss
# Comparing results
for (i in 1:length(results3)) {
  cat("Results for nstart =", nstart_values[i], ":\n")
  print(results3[[i]])
  cat("\n")
  cat("Results for nstart =", nstart_values[i], ":\n")
  cat("tot.withinss:", results3[[i]]$tot.withinss, "\n\n")
}


optimal_clusters_complete <- 8
optimal_clusters_single <- 6
optimal_clusters_centroid <- 5
optimal_clusters_average <- 4

# Create elbow plots
par(mfrow=c(2,2))  

# elbow plot for "complete"
y <- sort(hclust$height, decreasing = TRUE)[1:10]
plot(x, y, type = "l", col = "blue", main = "Method: Complete", xlab = "", ylab = "")
abline(v = optimal_clusters_complete, col = "red", lty = 2)

# elbow plot for "single"
y1 <- sort(hclust1$height, decreasing = TRUE)[1:10]
plot(x, y1, type = "l", col = "red", main = "Method: Single", xlab = "", ylab = "")
abline(v = optimal_clusters_single, col = "blue", lty = 2)

# elbow plot for "centroid"
y2 <- sort(hclust2$height, decreasing = TRUE)[1:10]
plot(x, y2, type = "l", col = "green", main = "Method: Centroid", xlab = "", ylab = "")
abline(v = optimal_clusters_centroid, col = "blue", lty = 2)

# elbow plot for "average"
y3 <- sort(hclust3$height, decreasing = TRUE)[1:10]
plot(x, y3, type = "l", col = "orange", main = "Method: Average", xlab = "", ylab = "")
abline(v = optimal_clusters_average, col = "blue", lty = 2)

#endogram 
plot(hclust)
plot(hclust1)
plot(hclust2)
plot(hclust3)
