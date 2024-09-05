dataset <- read.csv("D:/9th Semester/Mid/Data/project.csv", header=TRUE, sep=",")
# Install necessary packages if not already installed
if (!require("naniar")) install.packages("naniar")
library(naniar)

# Visualizing missing values
vis_miss(dataset)

# Assuming 'Age' is the column
summary_stats <- data.frame(
  Statistic = c("Mean", "Median", "Mode"),
  Value = c(mean(dataset$Age, na.rm = TRUE), median(dataset$Age, na.rm = TRUE), as.numeric(names(sort(-table(dataset$Age)))[1]))
)

# Using ggplot2 for visualization
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)
ggplot(summary_stats, aes(x = Statistic, y = Value)) + geom_col() + geom_text(aes(label = Value), vjust = -0.5)

dataset$AgeCategory <- cut(dataset$Age, breaks = c(-Inf, 18, 65, Inf), labels = c("Young", "Middle-aged", "Senior"))

dataset$SystolicBP_norm <- (dataset$SystolicBP - min(dataset$SystolicBP, na.rm = TRUE)) / (max(dataset$SystolicBP, na.rm = TRUE) - min(dataset$SystolicBP, na.rm = TRUE))

cleaned_dataset <- na.omit(dataset)

Q1 <- quantile(dataset$SystolicBP, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$SystolicBP, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
dataset <- dataset[dataset$SystolicBP > (Q1 - 1.5 * IQR) & dataset$SystolicBP < (Q3 + 1.5 * IQR),]

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Test the mode function
# Assuming 'Infection' is a factor or character vector with NA values
mode_infection <- get_mode(dataset$Infection[!is.na(dataset$Infection)])

# Update the 'Infection' column with the mode value
dataset$Infection[is.na(dataset$Infection)] <- mode_infection

# Convert to factor if it's not already
dataset$Infection <- as.factor(dataset$Infection)

# Now apply the mode function
mode_infection <- get_mode(dataset$Infection[!is.na(dataset$Infection)])
dataset$Infection[is.na(dataset$Infection)] <- mode_infection

library(dplyr)

mode_infection <- dataset %>%
  filter(!is.na(Infection)) %>% # Remove NAs to calculate mode correctly
  count(Infection) %>%
  top_n(n = 1, wt = n) %>%
  pull(Infection)

# Replace NA values in 'Infection' with the mode
dataset$Infection[is.na(dataset$Infection)] <- mode_infection

# Verify the changes
head(dataset)

# Ensure the dplyr package is loaded
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Assuming your data is loaded into a data frame named 'data'
# data <- read.csv("path/to/your/file.csv")

# Calculate the mode. This function finds the most common value in a vector
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Find the mode for the 'Infection' column, excluding NA values
mode_infection <- get_mode(dataset$Infection[!is.na(dataset$Infection)])

# Replace NA values in 'Infection' with the mode
dataset$Infection[is.na(dataset$Infection)] <- mode_infection

# Print the updated data to check if the changes were successful
print(head(data))













# Ensure the dplyr package is loaded
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# Assuming your data is loaded into a data frame named 'data'
# data <- read.csv("path/to/your/file.csv")

# Calculate the mode. This function finds the most common value in a vector
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Determine if the missing values are empty strings and calculate the mode for the 'Infection' column
mode_infection <- get_mode(dataset$Infection[dataset$Infection != ""])

# Replace empty string values in 'Infection' with the mode
dataset$Infection[dataset$Infection == ""] <- mode_infection

# Print the updated data to check if the changes were successful
print(head(dataset))




missing_age_rows <- which(is.na(dataset$Age))

# Display the row numbers with missing 'Age' values
print(missing_age_rows)


# Assuming 'dataset' is already loaded
# Find rows where 'Infection' is NA or an empty string
# Assuming the missing values are empty strings
missing_infection_rows <- which(dataset$Infection == "")

# Print the row numbers with missing 'Infection' values
print(missing_infection_rows)

missing_infection_rows <- which(is.na(dataset$Infection))

# Print the row numbers with missing 'Infection' values
print(missing_infection_rows)



# Check the unique values in the 'Infection' column
unique_values_infection <- unique(dataset$Infection)

# Print the unique values to identify how missing values are represented
print(unique_values_infection)




# Assuming summary_stats_Age is your data frame containing the mean, median, and mode.
summary_stats_Age <- data.frame(
  Statistic = c("Mean", "Median", "Mode"),
  Value = c(
    mean(dataset$Age, na.rm = TRUE), 
    median(dataset$Age, na.rm = TRUE), 
    as.numeric(names(sort(table(dataset$Age), decreasing = TRUE))[1])
  )
)

# Load the ggplot2 library for graphing
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(ggplot2)

# Create a bar chart
ggplot(summary_stats_Age, aes(x = Statistic, y = Value)) +
  geom_bar(stat = "identity") +
  labs(title = "Summary Statistics of Age", x = "Statistic", y = "Value")




# Replace 'ColumnName' with the actual name of your column.
# Min-Max Normalization: (value - min) / (max - min)
normalized_column <- (dataset$ColumnName - min(data$ColumnName, na.rm = TRUE)) /
  (max(data$ColumnName, na.rm = TRUE) - min(data$ColumnName, na.rm = TRUE))

# Add the normalized column back to the dataset as a new column
data$NormalizedColumnName <- normalized_column

# View the changes in the dataset
head(data)

# Assuming your data is in a data frame called 'cleaned_dataset'
# Calculate the IQR
Q1 <- quantile(dataset$Age, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$Age, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Calculate the bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify the outliers
outliers <- dataset$Age[which(dataset$Age < lower_bound | dataset$Age > upper_bound)]

# Print the outliers
print(outliers)

# Calculate the IQR and determine bounds
Q1 <- quantile(dataset$Age, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$Age, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Calculate the median
median_age <- median(dataset$Age, na.rm = TRUE)

# Replace outliers with the median value
dataset$Age <- ifelse(dataset$Age < lower_bound | dataset$Age > upper_bound, median_age, dataset$Age)

# View the modified data
print(head(dataset))

                                                                                                  