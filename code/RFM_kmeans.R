##rfm analysis ----

data <- data %>%
  mutate(revenue = Quantity * UnitPrice)

rfm <- data

data2 <- data %>%
  filter(!is.na(CustomerID))

rfm <- data %>%
  group_by(CustomerID) %>%
  summarise(
    revenue = sum(revenue),
    number_of_orders = n_distinct(InvoiceNo),
    recency_days = round(as.numeric(difftime(as.POSIXct("2021-11-24 17:06:00 UTC", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), max(InvoiceDate), units = "days"))),
    purchase = 1,
    zip_code = get_mode(ZipCode))

groups <- 5

## 5.3 Run RFM Analysis with Independent Sort
rfm$recency_score_indep <- ntile(rfm$recency_days*-1, groups)
rfm$frequency_score_indep <- ntile(rfm$number_of_orders, groups)
rfm$monetary_score_indep <- ntile(rfm$revenue, groups)
rfm$rfm_score_indep <- paste(rfm$recency_score_indep*100 + rfm$frequency_score_indep * 10 + rfm$monetary_score_indep)
rfm$recency_score_seq <- ntile(rfm$recency_days*-1, groups)
r_groups <- NULL; rf_groups <- NULL; temp <- NULL ## Initialize empty matrices

for (r in 1:groups) {
  r_groups[[r]] <- filter(rfm, rfm$recency_score_seq == r)
  r_groups[[r]]$frequency_score_seq <- ntile(r_groups[[r]]$number_of_orders, groups)
  for (m in 1:groups) {
    rf_groups[[m]] <- filter(r_groups[[r]], r_groups[[r]]$frequency_score_seq == m)
    rf_groups[[m]]$monetary_score_seq <- ntile(rf_groups[[m]]$revenue, groups)
    temp <- bind_rows(temp, rf_groups[[m]])
  }
}

rfm_result <- temp[order(temp$CustomerID),]
View(rfm_result)
rfm_result$rfm_score_seq <- paste(rfm_result$recency_score_seq*100 + rfm_result$frequency_score_seq * 10 + rfm_result$monetary_score_seq)

## Export RFM Results with Independent and Sequential Sort
write.csv(rfm_result, "Q:/Marketing Analytics/rfm_results.csv", row.names = FALSE) ## Name file rfm_result.csv


rfm_result <- data.frame(rfm_result)


##customer segmentation for rfm results
rfm_result <- rfm_result %>%
  mutate(
    Segment2 = case_when(
      recency_score_seq <= 2 & frequency_score_seq >= 4 & monetary_score_seq >= 4 ~ "Champions",
      recency_score_seq <= 3 & frequency_score_seq >= 3 & monetary_score_seq >= 3 ~ "Loyal Customers",
      recency_score_seq <= 2 & frequency_score_seq <= 3 & monetary_score_seq <= 3 ~ "Potential Loyalist",
      recency_score_seq >= 4 & frequency_score_seq >= 3 & monetary_score_seq >= 3 ~ "At Risk",
      recency_score_seq == 1 & frequency_score_seq <= 2 & monetary_score_seq <= 2 ~ "New Customers",
      recency_score_seq <= 3 & frequency_score_seq == 2 & monetary_score_seq == 2 ~ "Promising",
      recency_score_seq >= 4 & frequency_score_seq <= 2 & monetary_score_seq >= 2 ~ "Hibernating",
      frequency_score_seq >= 4 & monetary_score_seq <= 2 ~ "Price Sensitive",
      TRUE ~ "Others"
    )
  )

#join rfm table and segmentation table 
join <- inner_join(rfm_result, segmentation, by = "CustomerID")


segment_counts <- join %>%
  group_by(Segment2) %>%
  summarise(Count = n())


print(segment_counts)



library(ggplot2)

#this is created because when running the second time seed was not included and segments numbers changed
join <- join %>%
  mutate(segment = case_when(
    segment == 1 ~ 2,
    segment == 2 ~ 7,
    segment == 4 ~ 6,
    segment == 5 ~ 4,
    segment == 6 ~ 5,
    segment == 7 ~ 1,
    TRUE ~ segment  
  ))


join$segment <- as.numeric(join$segment)


#bar plot of customer segmentation with RFM 
ggplot(segment_counts, aes(x = reorder(Segment2, -Count), y = Count, fill = Segment2)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  labs(x = "Segment", y = "Customers", title = "Customer Segment Distribution") +
  coord_flip() + 
  scale_fill_brewer(palette = "Set2") +
  theme(
    axis.text.y = element_text(face = "bold")
  )

#bar plot of customer combining both types of segmentation done 
ggplot(join, aes(x = Segment2, fill = segment)) +
  geom_bar(show.legend = FALSE) +
  theme_minimal() +
  labs(x = "Segment", y = "Customer Count", title = "Customer Distribution by previous Segments") +
  facet_wrap(~ segment, scales = "free") + 
  theme(
    strip.text = element_text(face = "bold"), 
    axis.text.y = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  )


##k-means for rfm results ----


hclust4 <- hclust(dist(scale(cbind(rfm_result$recency_score_seq, rfm_result$frequency_score_seq, rfm_result$monetary_score_seq))), method = "complete")

y <- sort(hclust$height, decreasing = TRUE)[1:10]
plot(x,y); lines(x,y, col= "blue")

kmeans_rfm <- kmeans(x = data.frame(rfm_result$recency_score_seq, rfm_result$frequency_score_seq, rfm_result$monetary_score_seq), centers = 8, nstart = 50)


segmentrfm <- kmeans_rfm$cluster
segmentationrfm <- cbind(rfm_result, segmentrfm)
segment_countsrfm <- table(segmentationrfm$segmentrfm)

segment_datarfm <- as.data.frame(segment_countsrfm)

names(segment_datarfm) <- c("Segment", "Count")


ggplot(segment_datarfm, aes(x = Segment, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = Count), vjust = 0) + # Añadir etiquetas de conteo encima de las barras
  labs(title = "Customers in each segment", x = "Segment", y = "Customers") +
  theme_minimal()

