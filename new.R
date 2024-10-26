# Load Required Libraries
library(dplyr)
library(caret)  
library(ggplot2)
library(lubridate)
library(readr)
library(tidyverse)
library(quadprog)
library(forecast)
library(tseries)
library(Metrics)
library(zoo)


rm(list = ls())

# Load the Data
data1 <- read_csv("C:/Users/Admin/Downloads/Business Intelligence/Business Intelligence Project/dataset20092010.csv")
data2 <- read_csv("C:/Users/Admin/Downloads/Business Intelligence/Business Intelligence Project/dataset20102011.csv")

# Combine the datasets
raw_master_data <- bind_rows(data1, data2)

# Check the combined dataset
glimpse(raw_master_data)

# View rows with missing values using the correct column names
missing_rows <- raw_master_data %>%
  filter(is.na(`Customer ID`) | is.na(Quantity) | is.na(Price) | is.na(InvoiceDate) | is.na(Country))

# Display the missing values summary and rows
print(missing_rows)

# Check for missing values across specified columns
missing_summary <- raw_master_data %>%
  summarise(
    across(c(`Customer ID`, Quantity, Price, InvoiceDate, Country, Description), 
           ~sum(is.na(.)), 
           .names = "missing_{col}")
  ) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Missing_Count") %>%
  filter(Missing_Count > 0)  # Filter to show only columns with missing values

# Print the summary of missing columns
print(missing_summary)


# Check for duplicates based on specific columns (Invoice and StockCode)
duplicates_specific <- raw_master_data %>%
  filter(duplicated(select(., Invoice, StockCode)) | duplicated(select(., Invoice, StockCode), fromLast = TRUE))

# View the duplicates
View(duplicates_specific)

# Check for exact duplicates
duplicates_exact <- raw_master_data %>%
  filter(duplicated(.) | duplicated(., fromLast = TRUE))

# View the exact duplicates
View(duplicates_exact)

# Identify and remove exact duplicates
master_data_no_dupes <- raw_master_data %>%
  distinct(Invoice, StockCode, Quantity, Price, `Customer ID`, .keep_all = TRUE)

# Check the dataset after removing duplicates
glimpse(master_data_no_dupes)

# Remove rows where Quantity or Price is zero
master_data_no_zeroes <- master_data_no_dupes %>%
  filter(Quantity != 0 & Price != 0)

# Check the dataset after removing zero Quantity and Price rows
glimpse(master_data_no_zeroes)

# Step 1: Create a TotalSales column by multiplying Quantity and Price
master_data_with_totalsales <- master_data_no_zeroes %>%
  mutate(TotalSales = Quantity * Price)

# Step 2: View all distinct regions (countries) in the dataset
unique_regions <- master_data_with_totalsales %>%
  distinct(Country)

# Ensure all unique regions are printed
print(unique_regions, n = Inf)

# Add a Region column based on Country values
master_data_with_regions <- master_data_with_totalsales %>%
  mutate(Region = case_when(
    Country %in% c("European Community", "Iceland", "West Indies" ,"United Kingdom", "Channel Islands", "France", "Belgium", "EIRE", "Germany", "Portugal", "Netherlands", "Poland", "Spain", "Italy", "Cyprus", "Greece", "Austria", "Sweden", "Norway", "Denmark", "Finland", "Switzerland", "Malta", "Lithuania", "Czech Republic") ~ "EU",
    Country %in% c("USA", "Canada", "Brazil") ~ "NA/LATAM",
    Country %in% c("Australia", "Israel", "Bahrain", "New Zealand", "Japan", "Hong Kong", "Singapore", "Thailand", "Israel", "Lebanon", "Korea", "United Arab Emirates", "Saudi Arabia") ~ "APAC",
    Country %in% c("Nigeria", "RSA", "Lithuania") ~ "AFRICA",
    TRUE ~ "Other"
  ))

# Check the updated dataset with region-based segmentation
print(master_data_with_regions)

# Create one-hot encoding for the Region column
master_data_with_onehot_regions <- master_data_with_regions %>%
  select(-Region) %>%  # Exclude the Region column to avoid duplication
  cbind(model.matrix(~ Region - 1, data = master_data_with_regions))

# Check the new one-hot encoded dataset
print(master_data_with_onehot_regions)

# Data Transformation: Extract Year, Month, and Day of the Week
master_data_super_clean <- master_data_with_onehot_regions %>%
  mutate(
    InvoiceDate = mdy_hm(InvoiceDate),  # Converts date and time
    Year = year(InvoiceDate),
    Month = month(InvoiceDate, label = TRUE),
    DayOfWeek = wday(InvoiceDate, label = TRUE)
  )

# Check the updated dataset
print(master_data_super_clean)


# convert date to numerical
master_data_super_clean <- master_data_super_clean %>%
  mutate(
    Month = as.numeric(factor(Month, levels = month.abb)),
    DayOfWeek = as.numeric(factor(DayOfWeek, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
  )

master_data_super_clean <- master_data_super_clean %>%
  select(
    Invoice, StockCode, Description, InvoiceDate, Month, Day = DayOfWeek, Year, Quantity, Price, TotalSales, CustomerID = `Customer ID`, Country, RegionAFRICA, RegionAPAC, RegionEU, `RegionNA/LATAM`, RegionOther
  )

# Overall Summary Calculation
overall_summary <- master_data_super_clean %>%
  summarise(
    Unique_Customers = n_distinct(`CustomerID`, na.rm = TRUE),
    Guest_Customers = n_distinct(Invoice[is.na(`CustomerID`)]), 
    Mean_Total_Sales = mean(TotalSales, na.rm = TRUE),
    Median_Total_Sales = median(TotalSales, na.rm = TRUE),
    SD_Total_Sales = sd(TotalSales, na.rm = TRUE),
    Mean_Quantity = mean(Quantity, na.rm = TRUE),
    Median_Quantity = median(Quantity, na.rm = TRUE),
    SD_Quantity = sd(Quantity, na.rm = TRUE),
    Total_Unique_Invoices = n_distinct(Invoice)
  )

# Print Overall Summary
print(overall_summary)

# Region-Based Summary Calculation
region_summary <- master_data_super_clean %>%
  mutate(
    Region = case_when(
      RegionAFRICA == 1 ~ "AFRICA",
      RegionAPAC == 1 ~ "APAC",
      RegionEU == 1 ~ "EU",
      `RegionNA/LATAM` == 1 ~ "NA/LATAM",
      RegionOther == 1 ~ "Other",
      TRUE ~ NA_character_
    ),
    Is_Guest = is.na(CustomerID)  # Create a logical column for guest customers
  ) %>%
  group_by(Region) %>%
  summarise(
    Unique_Customers = n_distinct(CustomerID, na.rm = TRUE),
    Guest_Customers = n_distinct(Invoice[Is_Guest]),  # Count guest customers by unique invoices
    Mean_Total_Sales = mean(TotalSales, na.rm = TRUE),
    Median_Total_Sales = median(TotalSales, na.rm = TRUE),
    SD_Total_Sales = sd(TotalSales, na.rm = TRUE),
    Mean_Quantity = mean(Quantity, na.rm = TRUE),
    Median_Quantity = median(Quantity, na.rm = TRUE),
    SD_Quantity = sd(Quantity, na.rm = TRUE),
    Total_Unique_Invoices = n_distinct(Invoice)  # Count of unique invoices per region
  )


# Print updated Region-Based Summary
print(region_summary)


multi_region_customer <- master_data_super_clean %>%
  mutate(
    Region = case_when(
      RegionAFRICA == 1 ~ "AFRICA",
      RegionAPAC == 1 ~ "APAC",
      RegionEU == 1 ~ "EU",
      `RegionNA/LATAM` == 1 ~ "NA/LATAM",
      RegionOther == 1 ~ "Other",
      TRUE ~ NA_character_
    )
  ) %>%
  select(CustomerID, Region) %>%
  filter(!is.na(CustomerID)) %>%
  group_by(CustomerID) %>%
  summarize(Regions = paste(unique(Region), collapse = ", "), .groups = "drop") %>%
  filter(str_count(Regions, ",") >= 1)  # More than one region

# Print customers with the same ID in different regions
print(multi_region_customer)


# Remove negative total sales and negative quantities
master_data_positive_only <- master_data_super_clean %>%
  filter(TotalSales >= 0, Quantity >= 0)


# Recalculate Overall Summary (with Removed Negative Total Sales)
overall_summary_positive <- master_data_positive_only %>%
  summarise(
    Unique_Customers = n_distinct(CustomerID, na.rm = TRUE),
    Guest_Customers = n_distinct(Invoice[is.na(CustomerID)]), 
    Mean_Total_Sales = mean(TotalSales, na.rm = TRUE),
    Median_Total_Sales = median(TotalSales, na.rm = TRUE),
    SD_Total_Sales = sd(TotalSales, na.rm = TRUE),
    Mean_Quantity = mean(Quantity, na.rm = TRUE),
    Median_Quantity = median(Quantity, na.rm = TRUE),
    SD_Quantity = sd(Quantity, na.rm = TRUE),
    Total_Unique_Invoices = n_distinct(Invoice)
  )

# Region-Based Summary Calculation (Positive Total Sales Only)
region_summary_positive <- master_data_positive_only %>%
  mutate(
    Region = case_when(
      RegionAFRICA == 1 ~ "AFRICA",
      RegionAPAC == 1 ~ "APAC",
      RegionEU == 1 ~ "EU",
      `RegionNA/LATAM` == 1 ~ "NA/LATAM",
      RegionOther == 1 ~ "Other",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Region) %>%
  summarise(
    Unique_Customers = n_distinct(`CustomerID`, na.rm = TRUE),
    Guest_Customers = n_distinct(Invoice[is.na(`CustomerID`)]),  # Count guest customers by unique invoices
    Mean_Total_Sales = mean(TotalSales, na.rm = TRUE),
    Median_Total_Sales = median(TotalSales, na.rm = TRUE),
    SD_Total_Sales = sd(TotalSales, na.rm = TRUE),
    Mean_Quantity = mean(Quantity, na.rm = TRUE),
    Median_Quantity = median(Quantity, na.rm = TRUE),
    SD_Quantity = sd(Quantity, na.rm = TRUE),
    Total_Unique_Invoices = n_distinct(Invoice)  # Count of unique invoices per region
  )

# Testing to see Distribution

# Set a random seed for reproducibility
set.seed(111)

# Split the data into training and test sets (80/20 split)
train_index <- createDataPartition(master_data_super_clean$TotalSales, p = 0.8, list = FALSE)
train_data <- master_data_super_clean[train_index, ]
test_data <- master_data_super_clean[-train_index, ]

# Z-score scaling for the training set
train_data_z <- train_data %>%
  mutate(TotalSales_Z = scale(TotalSales, center = TRUE, scale = TRUE))

# Apply the same scaling to the test set
test_data_z <- test_data %>%
  mutate(TotalSales_Z = (TotalSales - mean(train_data$TotalSales)) / sd(train_data$TotalSales))

# Min-Max scaling for the training set
train_data_minmax <- train_data %>%
  mutate(TotalSales_MinMax = (TotalSales - min(TotalSales)) / (max(TotalSales) - min(TotalSales)))

# Apply the same scaling to the test set
test_data_minmax <- test_data %>%
  mutate(TotalSales_MinMax = (TotalSales - min(train_data$TotalSales)) / (max(train_data$TotalSales) - min(train_data$TotalSales)))


# Fit a linear model on the Z-score scaled training data
model_z <- lm(TotalSales_Z ~ Quantity + Price, data = train_data_z)
predictions_z <- predict(model_z, newdata = test_data_z)

# Evaluate model performance (RMSE)
rmse_z <- sqrt(mean((predictions_z - test_data_z$TotalSales_Z)^2))
cat("RMSE for Z-Score Scaled Data:", rmse_z, "\n")

# Fit a linear model on the Min-Max scaled training data
model_minmax <- lm(TotalSales_MinMax ~ Quantity + Price, data = train_data_minmax)
predictions_minmax <- predict(model_minmax, newdata = test_data_minmax)

# Evaluate model performance (RMSE)
rmse_minmax <- sqrt(mean((predictions_minmax - test_data_minmax$TotalSales_MinMax)^2))
cat("RMSE for Min-Max Scaled Data:", rmse_minmax, "\n")


#going to do min-max on the dataset!


# Apply Min-Max scaling to the selected numerical columns in the dataset
master_data_scaled <- master_data_super_clean %>%
  mutate(
    Quantity_MinMax = (Quantity - min(Quantity, na.rm = TRUE)) / (max(Quantity, na.rm = TRUE) - min(Quantity, na.rm = TRUE)),
    Price_MinMax = (Price - min(Price, na.rm = TRUE)) / (max(Price, na.rm = TRUE) - min(Price, na.rm = TRUE)),
    TotalSales_MinMax = (TotalSales - min(TotalSales, na.rm = TRUE)) / (max(TotalSales, na.rm = TRUE) - min(TotalSales, na.rm = TRUE))
  )

# Apply Min-Max scaling to the selected numerical columns in the dataset
master_data_scaled_only_totalsales <- master_data_super_clean %>%
  mutate(
    TotalSales_MinMax = (TotalSales - min(TotalSales, na.rm = TRUE)) / (max(TotalSales, na.rm = TRUE) - min(TotalSales, na.rm = TRUE))
)

# Filter for positive total sales and quantities
master_data_scaled_positive <- master_data_super_clean %>%
  filter(TotalSales >= 0, Quantity >= 0) %>%
  mutate(
    Quantity_MinMax = (Quantity - min(Quantity, na.rm = TRUE)) / (max(Quantity, na.rm = TRUE) - min(Quantity, na.rm = TRUE)),
    Price_MinMax = (Price - min(Price, na.rm = TRUE)) / (max(Price, na.rm = TRUE) - min(Price, na.rm = TRUE)),
    TotalSales_MinMax = (TotalSales - min(TotalSales, na.rm = TRUE)) / (max(TotalSales, na.rm = TRUE) - min(TotalSales, na.rm = TRUE))
  )

# Recalculate Overall Summary for the scaled positive data
overall_summary_scaled <- master_data_scaled_positive %>%
  summarise(
    Unique_Customers = n_distinct(CustomerID, na.rm = TRUE),
    Guest_Customers = n_distinct(Invoice[is.na(CustomerID)]), 
    Mean_Total_Sales = mean(TotalSales_MinMax, na.rm = TRUE),
    Median_Total_Sales = median(TotalSales_MinMax, na.rm = TRUE),
    SD_Total_Sales = sd(TotalSales_MinMax, na.rm = TRUE),
    Mean_Quantity = mean(Quantity_MinMax, na.rm = TRUE),
    Median_Quantity = median(Quantity_MinMax, na.rm = TRUE),
    SD_Quantity = sd(Quantity_MinMax, na.rm = TRUE),
    Total_Unique_Invoices = n_distinct(Invoice)
  )

# Region-Based Summary Calculation for the scaled positive data
region_summary_scaled <- master_data_scaled_positive %>%
  mutate(
    Region = case_when(
      RegionAFRICA == 1 ~ "AFRICA",
      RegionAPAC == 1 ~ "APAC",
      RegionEU == 1 ~ "EU",
      `RegionNA/LATAM` == 1 ~ "NA/LATAM",
      RegionOther == 1 ~ "Other",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Region) %>%
  summarise(
    Unique_Customers = n_distinct(CustomerID, na.rm = TRUE),
    Guest_Customers = n_distinct(Invoice[is.na(CustomerID)]),  # Count guest customers by unique invoices
    Mean_Total_Sales = mean(TotalSales_MinMax, na.rm = TRUE),
    Median_Total_Sales = median(TotalSales_MinMax, na.rm = TRUE),
    SD_Total_Sales = sd(TotalSales_MinMax, na.rm = TRUE),
    Mean_Quantity = mean(Quantity_MinMax, na.rm = TRUE),
    Median_Quantity = median(Quantity_MinMax, na.rm = TRUE),
    SD_Quantity = sd(Quantity_MinMax, na.rm = TRUE),
    Total_Unique_Invoices = n_distinct(Invoice)  # Count of unique invoices per region
  )

# Print the summary
print(region_summary_scaled)

# Check for quantities larger than 100
bulk_transactions <- master_data_super_clean %>%
  filter(Quantity > 25)

# Display the transactions with quantities larger than 100
print(bulk_transactions)


# Calculate mean and standard deviation for Quantity
quantity_stats <- master_data_super_clean %>%
  summarise(
    mean_quantity = mean(Quantity, na.rm = TRUE),
    sd_quantity = sd(Quantity, na.rm = TRUE)
  )

# Add z-scores to the dataset
master_data_super_clean_z <- master_data_super_clean %>%
  mutate(z_score = (Quantity - quantity_stats$mean_quantity) / quantity_stats$sd_quantity)

# Identify outliers based on z-score (e.g., z-score > 3 or < -3)
outliers <- master_data_super_clean_z %>%
  filter(abs(z_score) > 3)



# Count bulk orders in the original data
master_data_sc_bulk <- master_data_super_clean_z %>%
  filter(Quantity >= 25)  # Adjust this threshold based on your definition of bulk orders

# Summary of the outliers and bulk orders
summary_outliers <- outliers %>% summarise(outlier_count = nrow(.))

print(summary_outliers)

summary_bulk_orders <- master_data_sc_bulk %>% summarise(bulk_order_count = nrow(.))

# Print results
print(summary_outliers)
print(summary_bulk_orders)

# Calculate mean quantities on the original dataset
impact_of_bulk_orders <- master_data_super_clean_z %>%
  summarize(
    mean_quantity_without_bulk = mean(Quantity[Quantity < 25], na.rm = TRUE),
    sd_quantity_without_bulk = sd(Quantity[Quantity < 25], na.rm = TRUE),
    mean_quantity_with_bulk = mean(Quantity[Quantity >= 25], na.rm = TRUE),
    sd_quantity_with_bulk = sd(Quantity[Quantity >= 25], na.rm = TRUE)
  )

# Print the impact of bulk orders
print("Impact of Bulk Orders on Quantity:")
print(impact_of_bulk_orders)



# Calculate metrics with outliers
overall_metrics_with_outliers <- master_data_super_clean_z %>%
  summarise(
    mean_quantity = mean(Quantity, na.rm = TRUE),
    sd_quantity = sd(Quantity, na.rm = TRUE)
  )

# Calculate metrics without outliers
overall_metrics_without_outliers <- master_data_super_clean %>%
  filter(!(Quantity %in% outliers$Quantity)) %>%  # Use 'outliers' instead of 'outliers_regular'
  summarise(
    mean_quantity = mean(Quantity, na.rm = TRUE),
    sd_quantity = sd(Quantity, na.rm = TRUE)
  )

# Print the results
print("Overall Metrics with Outliers:")
print(overall_metrics_with_outliers)

print("Overall Metrics without Outliers:")
print(overall_metrics_without_outliers)

# Create a dataset without outliers based on z-score criteria
master_data_super_clean_no_outliers <- master_data_super_clean_z %>%
  filter(abs(z_score) <= 3)  # Keep only rows where the z-score is between -3 and 3

# Optionally, you can remove the z_score column if you don't need it in the new dataset
master_data_super_clean_no_outliers <- master_data_super_clean_no_outliers %>%
  select(-z_score)







# Print the first few rows to verify
print(head(master_data_super_clean_no_outliers))








# Add the "Region" column to overall_summary
overall_summary <- overall_summary %>%
  mutate(Region = "Overall")

# Ensure that the column order matches between overall_summary and region_summary
overall_summary <- overall_summary %>%
  select(Region, everything())

# Combine overall_summary and region_summary
combined_summary <- bind_rows(overall_summary, region_summary)


# Create a combined summary by adding overall_summary_positive to the bottom of region_summary_positive
combined_summary_positive <- bind_rows(region_summary_positive, 
                                       overall_summary_positive %>% mutate(Region = "Overall"))


# Create a combined summary by adding overall_summary_positive to the bottom of region_summary_positive
combined_summary_scaled <- bind_rows(region_summary_scaled, 
                                       overall_summary_scaled %>% mutate(Region = "Overall"))


overall_summary_bulk <- master_data_super_clean_no_outliers %>%
  summarise(
    Unique_Customers = n_distinct(CustomerID, na.rm = TRUE),
    Guest_Customers = n_distinct(Invoice[is.na(CustomerID)]), 
    Mean_Total_Sales = mean(TotalSales, na.rm = TRUE),
    Median_Total_Sales = median(TotalSales, na.rm = TRUE),
    SD_Total_Sales = sd(TotalSales, na.rm = TRUE),
    Mean_Quantity = mean(Quantity, na.rm = TRUE),
    Median_Quantity = median(Quantity, na.rm = TRUE),
    SD_Quantity = sd(Quantity, na.rm = TRUE),
    Total_Unique_Invoices = n_distinct(Invoice)
  )

# Region-Based Summary Calculation (Positive Total Sales Only)
region_summary_bulk <- master_data_super_clean_no_outliers %>%
  mutate(
    Region = case_when(
      RegionAFRICA == 1 ~ "AFRICA",
      RegionAPAC == 1 ~ "APAC",
      RegionEU == 1 ~ "EU",
      `RegionNA/LATAM` == 1 ~ "NA/LATAM",
      RegionOther == 1 ~ "Other",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Region) %>%
  summarise(
    Unique_Customers = n_distinct(`CustomerID`, na.rm = TRUE),
    Guest_Customers = n_distinct(Invoice[is.na(`CustomerID`)]),  # Count guest customers by unique invoices
    Mean_Total_Sales = mean(TotalSales, na.rm = TRUE),
    Median_Total_Sales = median(TotalSales, na.rm = TRUE),
    SD_Total_Sales = sd(TotalSales, na.rm = TRUE),
    Mean_Quantity = mean(Quantity, na.rm = TRUE),
    Median_Quantity = median(Quantity, na.rm = TRUE),
    SD_Quantity = sd(Quantity, na.rm = TRUE),
    Total_Unique_Invoices = n_distinct(Invoice)  # Count of unique invoices per region
  )

# Create a combined summary by adding overall_summary_positive to the bottom of region_summary_positive
combined_summary_bulk <- bind_rows(region_summary_bulk, 
                                     overall_summary_bulk %>% mutate(Region = "Overall"))




# Assuming master_data_super_clean_no_outliers is your data frame
master_data_cleaned_no_outliers_positive <- master_data_super_clean_no_outliers %>%
  filter(Quantity >= 0, Price >= 0)

overall_summary_bulk_positive <- master_data_cleaned_no_outliers_positive %>%
  summarise(
    Unique_Customers = n_distinct(CustomerID, na.rm = TRUE),
    Guest_Customers = n_distinct(Invoice[is.na(CustomerID)]), 
    Mean_Total_Sales = mean(TotalSales, na.rm = TRUE),
    Median_Total_Sales = median(TotalSales, na.rm = TRUE),
    SD_Total_Sales = sd(TotalSales, na.rm = TRUE),
    Mean_Quantity = mean(Quantity, na.rm = TRUE),
    Median_Quantity = median(Quantity, na.rm = TRUE),
    SD_Quantity = sd(Quantity, na.rm = TRUE),
    Total_Unique_Invoices = n_distinct(Invoice)
  )

# Region-Based Summary Calculation (Positive Total Sales Only)
region_summary_bulk_positive <- master_data_cleaned_no_outliers_positive %>%
  mutate(
    Region = case_when(
      RegionAFRICA == 1 ~ "AFRICA",
      RegionAPAC == 1 ~ "APAC",
      RegionEU == 1 ~ "EU",
      `RegionNA/LATAM` == 1 ~ "NA/LATAM",
      RegionOther == 1 ~ "Other",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Region) %>%
  summarise(
    Unique_Customers = n_distinct(`CustomerID`, na.rm = TRUE),
    Guest_Customers = n_distinct(Invoice[is.na(`CustomerID`)]),  # Count guest customers by unique invoices
    Mean_Total_Sales = mean(TotalSales, na.rm = TRUE),
    Median_Total_Sales = median(TotalSales, na.rm = TRUE),
    SD_Total_Sales = sd(TotalSales, na.rm = TRUE),
    Mean_Quantity = mean(Quantity, na.rm = TRUE),
    Median_Quantity = median(Quantity, na.rm = TRUE),
    SD_Quantity = sd(Quantity, na.rm = TRUE),
    Total_Unique_Invoices = n_distinct(Invoice)  # Count of unique invoices per region
  )

# Create a combined summary by adding overall_summary_positive to the bottom of region_summary_positive
combined_summary_bulk_positive <- bind_rows(region_summary_bulk_positive, 
                                   overall_summary_bulk_positive %>% mutate(Region = "Overall"))


#graph stupid

# Total Sales
total_sales_no_bulk <- sum(master_data_cleaned_no_outliers_positive$TotalSales)
total_sales_bulk <- sum(master_data_positive_only$TotalSales)

# Mean and Median
mean_sales_no_bulk <- mean(master_data_cleaned_no_outliers_positive$TotalSales)
mean_sales_bulk <- mean(master_data_positive_only$TotalSales)
median_sales_no_bulk <- median(master_data_cleaned_no_outliers_positive$TotalSales)
median_sales_bulk <- median(master_data_positive_only$TotalSales)

# Total Quantity
total_quantity_no_bulk <- sum(master_data_cleaned_no_outliers_positive$Quantity)
total_quantity_bulk <- sum(master_data_positive_only$Quantity)

# Unique Customers
unique_customers_no_bulk <- length(unique(master_data_cleaned_no_outliers_positive$CustomerID))
unique_customers_bulk <- length(unique(master_data_positive_only$CustomerID))


# Original Data
total_sales_with_low <- sum(master_data_cleaned_no_outliers_positive$TotalSales)
mean_sales_with_low <- mean(master_data_cleaned_no_outliers_positive$TotalSales)
median_sales_with_low <- median(master_data_cleaned_no_outliers_positive$TotalSales)
total_quantity_with_low <- sum(master_data_cleaned_no_outliers_positive$Quantity)
unique_customers_with_low <- length(unique(master_data_cleaned_no_outliers_positive$CustomerID))

# Filtered Data
filtered_data <- master_data_cleaned_no_outliers_positive[master_data_cleaned_no_outliers_positive$TotalSales >= 1, ]

total_sales_without_low <- sum(filtered_data$TotalSales)
mean_sales_without_low <- mean(filtered_data$TotalSales)
median_sales_without_low <- median(filtered_data$TotalSales)
total_quantity_without_low <- sum(filtered_data$Quantity)
unique_customers_without_low <- length(unique(filtered_data$CustomerID))

# Create Impact Analysis Table
impact_analysis_low_orders <- data.frame(
  Metric = c("Total Sales", "Mean Sales", "Median Sales", "Total Quantity", "Unique Customers"),
  With_Low_Orders = c(total_sales_with_low, mean_sales_with_low, median_sales_with_low, total_quantity_with_low, unique_customers_with_low),
  Without_Low_Orders = c(total_sales_without_low, mean_sales_without_low, median_sales_without_low, total_quantity_without_low, unique_customers_without_low)
)

print(impact_analysis_low_orders)



# Filter for negative values
master_data_negative_only <- master_data_super_clean %>%
  filter(TotalSales < 0)


overall_summary_negative_only <- master_data_negative_only %>%
  summarise(
    Unique_Customers = n_distinct(CustomerID, na.rm = TRUE),
    Guest_Customers = n_distinct(Invoice[is.na(CustomerID)]), 
    Mean_Total_Sales = mean(TotalSales, na.rm = TRUE),
    Median_Total_Sales = median(TotalSales, na.rm = TRUE),
    SD_Total_Sales = sd(TotalSales, na.rm = TRUE),
    Mean_Quantity = mean(Quantity, na.rm = TRUE),
    Median_Quantity = median(Quantity, na.rm = TRUE),
    SD_Quantity = sd(Quantity, na.rm = TRUE),
    Total_Unique_Invoices = n_distinct(Invoice)
  )

# Region-Based Summary Calculation (Positive Total Sales Only)
region_summary_negative_only <- master_data_negative_only %>%
  mutate(
    Region = case_when(
      RegionAFRICA == 1 ~ "AFRICA",
      RegionAPAC == 1 ~ "APAC",
      RegionEU == 1 ~ "EU",
      `RegionNA/LATAM` == 1 ~ "NA/LATAM",
      RegionOther == 1 ~ "Other",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Region) %>%
  summarise(
    Unique_Customers = n_distinct(`CustomerID`, na.rm = TRUE),
    Guest_Customers = n_distinct(Invoice[is.na(`CustomerID`)]),  # Count guest customers by unique invoices
    Mean_Total_Sales = mean(TotalSales, na.rm = TRUE),
    Median_Total_Sales = median(TotalSales, na.rm = TRUE),
    SD_Total_Sales = sd(TotalSales, na.rm = TRUE),
    Mean_Quantity = mean(Quantity, na.rm = TRUE),
    Median_Quantity = median(Quantity, na.rm = TRUE),
    SD_Quantity = sd(Quantity, na.rm = TRUE),
    Total_Unique_Invoices = n_distinct(Invoice)  # Count of unique invoices per region
  )



# Print the region-based summary
print(region_summary_negative_only)

# Create a combined summary by adding overall_summary_positive to the bottom of region_summary_positive
combined_summary_negative_only <- bind_rows(region_summary_negative_only, 
                                            overall_summary_negative_only %>% mutate(Region = "Overall"))

# Create a histogram of negative sales
ggplot(master_data_negative_only, aes(x = TotalSales)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Histogram of Negative Sales")

# Calculate the percentage of negative sales
percentage_negative <- nrow(master_data_negative_only) / nrow(data) * 100
print(paste0("Percentage of negative sales: ", percentage_negative, "%"))













# Create the original box plot of Quantity Sold (without log transformation)
box_plot_quantity <- ggplot(master_data_positive_only, aes(y = Quantity)) +
  geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "",
       y = "Quantity Sold") +
  theme_minimal()

# Display the original box plot
print(box_plot_quantity)

# Create the zoomed-in version of the box plot (without log transformation)
box_plot_quantity_zoom <- ggplot(master_data_positive_only, aes(y = Quantity)) +
  geom_boxplot(fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "",
       y = "Quantity Sold") +
  coord_cartesian(ylim = c(0, 500)) +  # Adjust y-axis limit for zoom
  theme_minimal()

# Display the zoomed-in version of the box plot
print(box_plot_quantity_zoom)

# Create the log-transformed box plot of Quantity Sold
box_plot_quantity_log <- ggplot(master_data_positive_only, aes(y = log(Quantity))) +
  geom_boxplot(fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "",
       y = "Log(Quantity Sold)") +
  theme_minimal()

# Display the log-transformed box plot
print(box_plot_quantity_log)

# Create the zoomed-in version of the log-transformed box plot
box_plot_quantity_log_zoom <- ggplot(master_data_positive_only, aes(y = log(Quantity))) +
  geom_boxplot(fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "",
       y = "Log(Quantity Sold)") +
  coord_cartesian(ylim = c(0, 6)) +  # Adjust y-axis limit for zoom
  theme_minimal()

# Display the zoomed-in version of the log-transformed box plot
print(box_plot_quantity_log_zoom)



# Create the histogram of Total Sales (without log transformation)
histogram_total_sales <- ggplot(master_data_positive_only, aes(x = TotalSales)) +
  geom_histogram(binwidth = 100, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "",
       x = "Total Sales", 
       y = "Frequency") +
  theme_minimal()

# Display the original histogram
print(histogram_total_sales)

# Create the zoomed-in version of the histogram (without log transformation)
histogram_total_sales_zoom <- ggplot(master_data_positive_only, aes(x = TotalSales)) +
  geom_histogram(binwidth = 100, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "",
       x = "Total Sales", 
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 2000)) +  # Adjust x-axis limit for zoom
  theme_minimal()

# Display the zoomed-in histogram
print(histogram_total_sales_zoom)

# Create the log-transformed histogram of Total Sales
histogram_total_sales_log <- ggplot(master_data_positive_only, aes(x = log(TotalSales))) +
  geom_histogram(binwidth = 0.1, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "",
       x = "Log(Total Sales)", 
       y = "Frequency") +
  theme_minimal()

# Display the log-transformed histogram
print(histogram_total_sales_log)

# Create the zoomed-in version of the log-transformed histogram
histogram_total_sales_log_zoom <- ggplot(master_data_positive_only, aes(x = log(TotalSales))) +
  geom_histogram(binwidth = 0.1, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "",
       x = "Log(Total Sales)", 
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 6)) +  # Adjust x-axis limit for zoom
  theme_minimal()

# Display the zoomed-in log-transformed histogram
print(histogram_total_sales_log_zoom)


#####################################################

# Creating a combined Region column
master_data_positive_only <- master_data_positive_only %>%
  mutate(Region = case_when(
    RegionAFRICA == 1 ~ "AFRICA",
    RegionAPAC == 1 ~ "APAC",
    RegionEU == 1 ~ "EU",
    `RegionNA/LATAM` == 1 ~ "NA/LATAM",
    RegionOther == 1 ~ "Other",
    TRUE ~ "Unknown"  # Add a catch-all case for safety
  ))

#####################################################

customer_counts <- master_data_positive_only %>%
  mutate(CustomerType = ifelse(is.na(CustomerID), "Guest", "Unique")) %>%
  group_by(Region, CustomerType) %>%
  summarise(Count = n(), .groups = "drop")

# Stacked Bar Chart of Unique and Guest Customers by Region
ggplot(customer_counts, aes(x = Region, y = Count, fill = CustomerType)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "Region", 
       y = "Count of Customers") +
  scale_fill_manual(values = c("Unique" = "blue", "Guest" = "red"), 
                    name = "Customer Type") +
  theme_minimal()


# Check for missing values in customer_counts
summary(customer_counts)

# Stacked Bar Chart of Unique and Guest Customers by Region (Zoomed In with Log Scale)
ggplot(customer_counts, aes(x = Region, y = Count, fill = CustomerType)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "",
       x = "Region", 
       y = "Count of Customers (Log Scale)") +
  scale_fill_manual(values = c("Unique" = "blue", "Guest" = "red"), 
                    name = "Customer Type") +
  scale_y_log10() +  # Removed limits for auto-scaling
  theme_minimal()



############################

###########################
###########################

###########################

# Histogram of Total Sales without Bulk Orders (log-transformed)
ggplot(master_data_cleaned_no_outliers_positive, aes(x = log(TotalSales))) +
  geom_histogram(binwidth = 0.5, fill = "yellow", color = "black", alpha = 0.7) +
  labs(title = "") +
  theme_minimal()

# Box Plot of Quantity Sold without Bulk Orders (log-transformed)
ggplot(master_data_cleaned_no_outliers_positive, aes(y = log(Quantity))) +
  geom_boxplot(fill = "yellow", color = "black") +
  labs(title = "") +
  theme_minimal()



# Identify one-hot encoded region columns
region_cols <- grep("^Region", names(master_data_positive_only), value = TRUE)

# Decode one-hot encoded regions into a single 'region' column with actual names
master_data_positive_only$region <- apply(master_data_positive_only[, region_cols], 1, function(x) {
  if (x["RegionAFRICA"] == 1) return("AFRICA")
  if (x["RegionAPAC"] == 1) return("APAC")
  if (x["RegionEU"] == 1) return("EU")
  if (x["RegionNA/LATAM"] == 1) return("NA/LATAM")
  if (x["RegionOther"] == 1) return("Other")
  return(NA)  # To capture rows where no region is assigned
})

# Calculate average total sales for each region with bulk orders
avg_total_sales_with_bulk <- master_data_positive_only %>%
  group_by(region) %>%
  summarize(avg_total_sales = mean(TotalSales))



# Identify one-hot encoded region columns
region_cols <- grep("^Region", names(master_data_cleaned_no_outliers_positive), value = TRUE)

# Decode one-hot encoded regions into a single 'region' column with actual names
master_data_cleaned_no_outliers_positive$region <- apply(master_data_cleaned_no_outliers_positive[, region_cols], 1, function(x) {
  if (x["RegionAFRICA"] == 1) return("AFRICA")
  if (x["RegionAPAC"] == 1) return("APAC")
  if (x["RegionEU"] == 1) return("EU")
  if (x["RegionNA/LATAM"] == 1) return("NA/LATAM")
  if (x["RegionOther"] == 1) return("Other")
  return(NA)  # To capture rows where no region is assigned
})

# Calculate average total sales for each region without bulk orders
avg_total_sales_without_bulk <- master_data_cleaned_no_outliers_positive %>%
  group_by(region) %>%
  summarize(avg_total_sales = mean(TotalSales))

# Combine the results into a single data frame, renaming the .id to "Bulk Order"
data_by_region <- bind_rows(avg_total_sales_with_bulk, avg_total_sales_without_bulk, .id = "Bulk Order")

# Recode the 'Bulk Order' column to 'With Bulk Orders' and 'Without Bulk Orders'
data_by_region$`Bulk Order` <- recode(data_by_region$`Bulk Order`,
                                      "1" = "With Bulk Orders",
                                      "2" = "Without Bulk Orders")

# Create the bar chart with the updated legend labels
ggplot(data_by_region, aes(x = region, y = avg_total_sales, fill = `Bulk Order`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "") +
  scale_fill_manual(values = c("With Bulk Orders" = "blue", "Without Bulk Orders" = "red")) +
  theme_minimal()


########################## ########################## ##########################

##########################
##########################
##########################

########################## ########################## ##########################

# ANOVA for multiple region comparison (positive sales without bulk orders)
anova_test <- aov(log(TotalSales) ~ region, data = master_data_cleaned_no_outliers_positive)
summary(anova_test)

# ANOVA for multiple region comparison (positive sales with bulk orders)
anova_test_bulk <- aov(log(TotalSales) ~ region, data = master_data_positive_only)
summary(anova_test_bulk)

# Post-hoc Tukey HSD test
TukeyHSD(anova_test)
TukeyHSD(anova_test_bulk)

# Create new datasets for scaled data
master_data_cleaned_no_outliers_positive_scaled <- master_data_cleaned_no_outliers_positive
master_data_positive_only_scaled <- master_data_positive_only

# Standardization (Z-scaling)
master_data_cleaned_no_outliers_positive_scaled$TotalSales_scaled <- scale(master_data_cleaned_no_outliers_positive$TotalSales)
master_data_positive_only_scaled$TotalSales_scaled <- scale(master_data_positive_only$TotalSales)

# Min-Max scaling function
min_max_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Min-Max Scaling
master_data_cleaned_no_outliers_positive_scaled$TotalSales_minmax <- min_max_scale(master_data_cleaned_no_outliers_positive$TotalSales)
master_data_positive_only_scaled$TotalSales_minmax <- min_max_scale(master_data_positive_only$TotalSales)

# ANOVA for multiple region comparison (scaled TotalSales, without bulk orders)
anova_test_scaled <- aov(TotalSales_scaled ~ region, data = master_data_cleaned_no_outliers_positive_scaled)
summary(anova_test_scaled)

# ANOVA for multiple region comparison (scaled TotalSales, with bulk orders)
anova_test_scaled_bulk <- aov(TotalSales_scaled ~ region, data = master_data_positive_only_scaled)
summary(anova_test_scaled_bulk)

# Define churn threshold (e.g., customers with no purchase in the last 6 months)
churn_threshold <- as.Date("2011-12-01") - 180

# Calculate churn rate
churn_data <- master_data_super_clean %>%
  group_by(CustomerID) %>%
  summarize(last_purchase = max(as.Date(InvoiceDate))) %>%
  mutate(churned = ifelse(last_purchase < churn_threshold, 1, 0))

# Churn rate percentage
churn_rate <- sum(churn_data$churned) / nrow(churn_data) * 100



# Overall Sales
ggplot(master_data_super_clean, aes(x = Quantity, y = TotalSales)) +
  geom_point(color = "blue") +
  labs(title = "",
       x = "Quantity Sold",
       y = "Total Sales") +
  theme_minimal()

# positive with bulk
ggplot(master_data_cleaned_no_outliers_positive, aes(x = Quantity, y = TotalSales)) +
  geom_point(color = "green") +
  labs(title = "",
       x = "Quantity Sold",
       y = "Total Sales") +
  theme_minimal()

# Positive without bulk
ggplot(master_data_positive_only, aes(x = Quantity, y = TotalSales)) +
  geom_point(color = "orange") +
  labs(title = "",
       x = "Quantity Sold",
       y = "Total Sales") +
  theme_minimal()
















# Ensure that the Date format is correct
master_data_super_clean$InvoiceDate <- as.Date(master_data_super_clean$InvoiceDate)
master_data_super_clean_no_outliers$InvoiceDate <- as.Date(master_data_super_clean_no_outliers$InvoiceDate)

# Summarize sales data by Date for both datasets
super_clean_daily_sales <- master_data_super_clean %>%
  group_by(InvoiceDate) %>%
  summarise(TotalSales = sum(TotalSales))

clean_no_outliers_daily_sales <- master_data_super_clean_no_outliers %>%
  group_by(InvoiceDate) %>%
  summarise(TotalSales = sum(TotalSales))

# Plotting the time series data for both datasets
ggplot(super_clean_daily_sales, aes(x = InvoiceDate, y = TotalSales)) +
  geom_line(color = "blue") +
  labs(title = "",
       x = "Date", y = "Total Sales") +
  theme_minimal()

ggplot(clean_no_outliers_daily_sales, aes(x = InvoiceDate, y = TotalSales)) +
  geom_line(color = "green") +
  labs(title = "",
       x = "Date", y = "Total Sales") +
  theme_minimal()

# Convert to Time Series format (monthly frequency)
super_clean_ts <- ts(super_clean_daily_sales$TotalSales, frequency = 30, start = c(2009, 12))
clean_no_outliers_ts <- ts(clean_no_outliers_daily_sales$TotalSales, frequency = 30, start = c(2009, 12))

# Assuming super_clean_ts and clean_no_outliers_ts are defined
# Decompose the time series into trend, seasonal, and random components
super_clean_decomp <- decompose(super_clean_ts)
clean_no_outliers_decomp <- decompose(clean_no_outliers_ts)

# Plot the decompositions using autoplot from the forecast package
autoplot(super_clean_decomp) + ggtitle("")
autoplot(clean_no_outliers_decomp) + ggtitle("")

# Compare positive vs negative sales trends
positive_sales <- master_data_super_clean %>%
  filter(TotalSales > 0) %>%
  group_by(InvoiceDate) %>%
  summarise(TotalSales = sum(TotalSales))

negative_sales <- master_data_super_clean %>%
  filter(TotalSales < 0) %>%
  group_by(InvoiceDate) %>%
  summarise(TotalSales = sum(TotalSales))

ggplot() +
  geom_line(data = positive_sales, aes(x = InvoiceDate, y = TotalSales), color = "blue") +
  geom_line(data = negative_sales, aes(x = InvoiceDate, y = TotalSales), color = "red") +
  labs(title = "",
       x = "Date", y = "Total Sales") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# Compare positive vs negative sales trends without outliers
positive_sales_no_outliers <- master_data_super_clean_no_outliers %>%
  filter(TotalSales > 0) %>%
  group_by(InvoiceDate) %>%
  summarise(TotalSales = sum(TotalSales))

negative_sales_no_outliers <- master_data_super_clean_no_outliers %>%
  filter(TotalSales < 0) %>%
  group_by(InvoiceDate) %>%
  summarise(TotalSales = sum(TotalSales))

ggplot() +
  geom_line(data = positive_sales_no_outliers, aes(x = InvoiceDate, y = TotalSales), color = "blue") +
  geom_line(data = negative_sales_no_outliers, aes(x = InvoiceDate, y = TotalSales), color = "red") +
  labs(title = "",
       x = "Date", y = "Total Sales") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)










# Convert to time series
master_data_cleaned_no_outliers_positive$Date <- as.Date(master_data_cleaned_no_outliers_positive$InvoiceDate, format="%Y-%m-%d")
master_data_positive_only$Date <- as.Date(master_data_positive_only$InvoiceDate, format="%Y-%m-%d")

# Summing sales by month for time series analysis
ts_sales_clean <- master_data_cleaned_no_outliers_positive %>%
  group_by(Date = as.yearmon(Date)) %>%
  summarise(TotalSales = sum(TotalSales))

ts_sales_positive <- master_data_positive_only %>%
  group_by(Date = as.yearmon(Date)) %>%
  summarise(TotalSales = sum(TotalSales))

# Convert into time series objects
ts_clean <- ts(ts_sales_clean$TotalSales, start=c(2009,12), frequency=12)
ts_positive <- ts(ts_sales_positive$TotalSales, start=c(2009,12), frequency=12)

# Fit ARIMA model (auto.arima automatically selects best parameters)
arima_clean <- auto.arima(ts_clean, seasonal=TRUE)
arima_positive <- auto.arima(ts_positive, seasonal=TRUE)

# ARIMA model summary
summary(arima_clean)
summary(arima_positive)

# Forecast for the next 12 months
forecast_clean <- forecast(arima_clean, h=12)
forecast_positive <- forecast(arima_positive, h=12)

# Plot forecast for clean dataset
autoplot(forecast_clean) + ggtitle("")

# Plot forecast for positive dataset
autoplot(forecast_positive) + ggtitle("")

# Model accuracy (train-test split, or compare forecast with known values)
actual_clean <- ts_clean
pred_clean <- fitted(arima_clean)

actual_positive <- ts_positive
pred_positive <- fitted(arima_positive)

# Calculate RMSE for both datasets
rmse_clean <- rmse(actual_clean, pred_clean)
rmse_positive <- rmse(actual_positive, pred_positive)

cat("RMSE for Clean Sales Data: ", rmse_clean, "\n")
cat("RMSE for Positive Sales Data: ", rmse_positive, "\n")





# Build linear regression model
lm_clean <- lm(TotalSales ~ Date, data=ts_sales_clean)
lm_positive <- lm(TotalSales ~ Date, data=ts_sales_positive)

# Summary of regression model
summary(lm_clean)
summary(lm_positive)

# Plot regression line with clean sales data
ggplot(ts_sales_clean, aes(x=Date, y=TotalSales)) +
  geom_point() +
  geom_smooth(method="lm", col="blue") +
  ggtitle("") +
  theme_minimal()

# Plot regression line with positive sales data
ggplot(ts_sales_positive, aes(x=Date, y=TotalSales)) +
  geom_point() +
  geom_smooth(method="lm", col="green") +
  ggtitle("") +
  theme_minimal()



set.seed(111)

positive_only <- master_data_positive_only %>%
  select(CustomerID, TotalSales, Quantity, Frequency = n_distinct("Invoice")) %>%
  group_by(CustomerID) %>%
  summarise(
    TotalSales = sum(TotalSales, na.rm = TRUE),
    AverageOrderSize = mean(TotalSales, na.rm = TRUE),
    PurchaseFrequency = n()
  )

# Repeat the same for the other datasets
positive_no_outliers <- master_data_cleaned_no_outliers_positive %>%
  select(CustomerID, TotalSales, Quantity, Frequency = n_distinct("Invoice")) %>%
  group_by(CustomerID) %>%
  summarise(
    TotalSales = sum(TotalSales, na.rm = TRUE),
    AverageOrderSize = mean(TotalSales, na.rm = TRUE),
    PurchaseFrequency = n()
  )

general_sales_no_outliers <- master_data_super_clean_no_outliers %>%
  select(CustomerID, TotalSales, Quantity, Frequency = n_distinct("Invoice")) %>%
  group_by(CustomerID) %>%
  summarise(
    TotalSales = sum(TotalSales, na.rm = TRUE),
    AverageOrderSize = mean(TotalSales, na.rm = TRUE),
    PurchaseFrequency = n(),
    ReturnRate = sum(Quantity < 0) / n()  # Calculate return rate
  )

# Example: Scale the data
positive_only_scaled <- scale(positive_only %>% select(-CustomerID))
positive_no_outliers_scaled <- scale(positive_no_outliers %>% select(-CustomerID))
general_sales_no_outliers_scaled <- scale(general_sales_no_outliers %>% select(-CustomerID))

# Determine optimal number of clusters using Elbow Method
wss <- function(k) {
  kmeans(positive_only_scaled, k, nstart = 10 )$tot.withinss
}

# Compute and plot the WSS for different k values
k.values <- 1:10
wss_values <- sapply(k.values, wss)

plot(k.values, wss_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters K",
     ylab = "Total Within-Cluster Sum of Squares")

# Perform K-means clustering with the chosen number of clusters (say, k = 4)
kmeans_result <- kmeans(positive_only_scaled, centers = 3, nstart = 25)

# Add the cluster labels to the original dataset
positive_only$Cluster <- kmeans_result$cluster

# Visualize the clusters using ggplot2 (e.g., with first two principal components)
fviz_cluster(kmeans_result, data = positive_only_scaled)

# Alternatively, use PCA for dimension reduction and plotting
pca_res <- prcomp(positive_only_scaled, scale = TRUE)
pca_data <- as.data.frame(pca_res$x)
pca_data$Cluster <- factor(positive_only$Cluster)

ggplot(pca_data, aes(PC1, PC2, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

# Summary of key features per cluster
cluster_summary <- positive_only %>%
  group_by(Cluster) %>%
  summarise(
    AverageSales = mean(TotalSales),
    AvgOrderSize = mean(AverageOrderSize),
    AvgPurchaseFrequency = mean(PurchaseFrequency)
  )

# Print the summary table
print(cluster_summary)

# Visualize the clusters with labels and a title
plot <- fviz_cluster(kmeans_result, data = positive_only_scaled, label = "cluster")

# Customize the plot with a title
plot <- plot +
  ggtitle("") +
  theme_minimal()

# Display the plot
print(plot)
