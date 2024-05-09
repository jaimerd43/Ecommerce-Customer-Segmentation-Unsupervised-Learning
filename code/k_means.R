##final k mean selection ---

seg_kmeans_final <- kmeans(x = data.frame(new_data$Avg_Quantity, new_data$Total_Quantity, new_data$total_value, new_data$Avg_UnitPrice, new_data$Avg_ReturnRate), centers = 6, nstart = 50)
seg_kmeans_final$tot.withinss

segment <- seg_kmeans_final$cluster
segmentation <- cbind(new_data, segment)
table(segmentation$segment)

#visualize the segments 
segment_counts <- table(segmentation$segment)
segment_data <- as.data.frame(segment_counts)

names(segment_data) <- c("Segment", "Count")

# Create bar plot
ggplot(segment_data, aes(x = Segment, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Count), vjust = -0.5) + 
  labs(title = "Customers in each segment", x = "Segment", y = "Customers") +
  theme_minimal()

library(cluster)
library(factoextra)

new_data_numeric <- new_data[sapply(new_data, is.numeric)]

# clusters visualization 
fviz_cluster(seg_kmeans_final, new_data_numeric, 
             ggtheme = theme_minimal())
