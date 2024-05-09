##Data preparation -----

#remove observations which purchase was cancelled
data <- subset(data, !startsWith(InvoiceNo, "C"))

#check how many values are higher that 1
filtered_data <- data[data$ReturnRate > 1, ]

# Calculate the percentage of the filtered data set compared to the original dataset
percentage_higher_than_1 <- (nrow(filtered_data) / nrow(data)) * 100

# Print the percentage
print(percentage_higher_than_1)

#remove rows where returnrate is higher than 1
data <- data[data$ReturnRate <= 1, ]

#convert invocedate into the correct format
data$InvoiceDate <- as.POSIXct(data$InvoiceDate, format = "%Y-%m-%dT%H:%M")

#mutate work levels
data <- data %>%
  mutate(Work = case_when(
    Work == 1 ~ "Health services",
    Work == 2 ~ "Financial services",
    Work == 3 ~ "Sales",
    Work == 4 ~ "Advertising/PR",
    Work == 5 ~ "Education",
    Work == 6 ~ "Industrial Sector",
    Work == 7 ~ "Engineering",
    Work == 8 ~ "Technology",
    Work == 9 ~ "Retail & Services",
    Work == 10 ~ "Self-Employed",
    Work == 11 ~ "Other"
  ))

#mutate Education levels

data <- data %>%
  mutate(Edcation = case_when(
    Edcation == 1 ~ "High School",
    Edcation == 2 ~ "Undergraduate",
    Edcation == 3 ~ "Postgraduate"
  ))

### Marriage

data <- data %>%
  mutate(Married = case_when(
    Married == 1 ~ "Married",
    Married == 0 ~ "Single"
  ))

##convert categorical variables into factors 

data$Work <- as.factor(data$Work)
data$Edcation <- as.factor(data$Edcation)
data$Married <- as.factor(data$Married)
data$ZipCode <- as.factor(data$ZipCode)

## imputation of missing values in customer ID 

#create a data set with all missing values 
data_na <- data %>%
  filter(is.na(CustomerID))

#filter the original data for non missing values
data <- data %>%
  filter(!is.na(CustomerID))

##same InvoceNo same customer
data_na <- data_na %>%
  group_by(InvoiceNo) %>%
  mutate(CustomerID = cur_group_id())

# join both data sets
data <- bind_rows(data, data_na)


#mode for categorical variables 
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#group by customer id w mean for numerical and mode for categorical 
new_data <- data %>%
  group_by(CustomerID) %>%
  summarise(Age = mean(Age), 
            Work = get_mode(Work),
            Avg_Quantity = mean(Quantity),
            Total_Quantity = sum(Quantity),
            total_value = sum(Quantity * UnitPrice),
            Avg_UnitPrice = mean(UnitPrice),
            Married = get_mode(Married),
            total_invoice = n_distinct(InvoiceNo),
            Avg_ReturnRate = mean(ReturnRate),
            Income = mean(Income),
            Edcation = get_mode(Edcation),
            zipcode = get_mode(ZipCode))%>% filter(
              total_value >= quantile(total_value, 0.025),
              total_value <= quantile(total_value, 0.975),
              Total_Quantity >= quantile(Total_Quantity, 0.025),
              Total_Quantity <= quantile(Total_Quantity, 0.975))

