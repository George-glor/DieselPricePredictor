# Install and load necessary libraries
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(readxl)
library(dplyr)

# Load the dataset
data <- read_excel("D:/Download/R-Programmering-main/R-Programmering-main/BlocketDatacollect.xlsx")

# Drop the 'Registration' column if it exists
if ("Registration" %in% colnames(data)) {
  data <- data %>% select(-Registration)
}

# Function to impute mean for numeric columns
impute_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

# Function to impute mode for categorical columns
impute_mode <- function(x) {
  mode_val <- names(sort(table(x), decreasing = TRUE))[1]
  replace(x, is.na(x), mode_val)
}

# Clean and impute 'Mileage' and 'Horsepower' columns
data$Mileage <- as.numeric(gsub("[^0-9]", "", data$Mileage))
data$Horsepower <- as.numeric(gsub("[^0-9]", "", data$Horsepower))

# Impute missing values for numeric columns
data$Year <- impute_mean(data$Year)
data$Mileage <- impute_mean(data$Mileage)
data$Horsepower <- impute_mean(data$Horsepower)
data$Price <- impute_mean(data$Price)

# Impute missing values for categorical columns
data$Region <- impute_mode(data$Region)
data$Brand <- impute_mode(data$Brand)
data$Model <- impute_mode(data$Model)
data$Type <- impute_mode(data$Type)
data$Drive <- impute_mode(data$Drive)
data$Color <- impute_mode(data$Color)

# Convert categorical variables to factors
data$Region <- as.factor(data$Region)
data$Brand <- as.factor(data$Brand)
data$Model <- as.factor(data$Model)
data$Type <- as.factor(data$Type)
data$Drive <- as.factor(data$Drive)
data$Color <- as.factor(data$Color)

# Summary of cleaned data
summary(data)

# Scatterplots for continuous variables Price vs. Year, Mileage, and Horsepower
plot(data$Year, data$Price, xlab = "Model Year", ylab = "Price", main = "Scatterplot: Year vs. Price")
plot(data$Mileage, data$Price, xlab = "Mileage", ylab = "Price", main = "Scatterplot: Mileage vs. Price")
plot(data$Horsepower, data$Price, xlab = "Horsepower", ylab = "Price", main = "Scatterplot: Horsepower vs. Price")

# Boxplot for categorical variables vs. Price
boxplot(Price ~ Region, data = data, main = "Boxplot: Region vs. Price")
boxplot(Price ~ Brand, data = data, main = "Boxplot: Brand vs. Price")
boxplot(Price ~ Model, data = data, main = "Boxplot: Model vs. Price")
boxplot(Price ~ Type, data = data, main = "Boxplot: Type vs. Price")
boxplot(Price ~ Drive, data = data, main = "Boxplot: Drive vs. Price")
boxplot(Price ~ Color, data = data, main = "Boxplot: Color vs. Price")

# Final multiple regression model
multiple_model_final <- lm(Price ~ Year + Mileage + Horsepower + Brand + Model + Type + Drive, data = data)
summary(multiple_model_final)
