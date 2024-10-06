# Load necessary libraries
library(dplyr)
library(ggplot2)

# Set the working directory to the path where your CSV file is located
setwd("your_directory_path")  # Replace with your actual path

# Load the dataset
sales_data <- read.csv("LivWellSalesData.csv", header = TRUE, stringsAsFactors = FALSE)

# Check if the dataset is loaded correctly
head(sales_data)

# Convert SaleDate to Date format and create SaleMonth column
sales_data$SaleDate <- as.Date(sales_data$SaleDate)
sales_data <- sales_data %>% 
  mutate(SaleMonth = format(SaleDate, "%Y-%m"))

# Aggregate sales data by SaleMonth and ProductCategory
sales_summary <- sales_data %>%
  group_by(SaleMonth, ProductCategory) %>%
  summarize(
    TotalSales = sum(SalesQuantity),    # Sum of SalesQuantity
    TotalRevenue = sum(SalesAmount)     # Sum of SalesAmount
  )

# View the aggregated data
head(sales_summary)

—--------------------------
# Load necessary library
library(ggplot2)

# Build the linear regression model
model <- lm(SalesAmount ~ SalesQuantity, data = sales_data)

# Display the model summary
summary(model)

# Extract R-squared value
r_squared <- summary(model)$r.squared
cat("R-squared value:", r_squared, "\n")

# Plot the actual data and regression line
plot(sales_data$SalesQuantity, sales_data$SalesAmount, 
     main = "SalesAmount vs. SalesQuantity",
     xlab = "Sales Quantity", ylab = "Sales Amount",
     pch = 16, col = "blue", cex = 0.6)
abline(model, col = "red", lwd = 2)
legend("topleft", paste("R-squared =", round(r_squared, 2)), bty = "n")

# Predict future SalesAmount for new SalesQuantity values
new_data <- data.frame(SalesQuantity = c(10, 20, 30, 40, 50))
predicted_sales <- predict(model, new_data)
new_data$PredictedSalesAmount <- predicted_sales
print(new_data)
