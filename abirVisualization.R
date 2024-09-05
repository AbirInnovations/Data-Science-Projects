dataset <- read.csv("D:/9th Semester/Mid/Data/Drug_clean.csv", header=TRUE, sep=",")


# Ensure the corrplot package is installed and loaded
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}
library(corrplot)

# Select the numerical data columns
numerical_data <- dataset[, c("EaseOfUse", "Effective", "Price", "Reviews", "Satisfaction")]

# Calculate the correlation matrix
corr_matrix <- cor(numerical_data, use = "complete.obs")

# Visualize the correlation matrix using corrplot
corrplot(corr_matrix, method = "circle", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


install.packages("ggplot2")

library(ggplot2)
ggplot(dataset, aes(x = Form, y = Satisfaction, fill = Form)) +
  geom_boxplot() +
  labs(title = "Satisfaction Ratings by Drug Form", x = "Form", y = "Satisfaction") +
  theme_minimal()

ggplot(dataset, aes(x = Effective, y = Price)) +
  geom_point(aes(color = Type)) +
  facet_wrap(~ Type) +
  scale_y_log10() +
  labs(title = "Price vs. Effectiveness by Type", x = "Effectiveness", y = "Price (log scale)") +
  theme_minimal()

install.packages("tidyverse")

library(tidyverse)
condition_counts <- dataset %>%
  group_by(Condition) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
ggplot(condition_counts, aes(x = reorder(Condition, Count), y = Count, fill = Condition)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Drugs per Condition", x = "Condition", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

  

ggplot(dataset, aes(x = Reviews, fill = factor(Satisfaction))) +
  geom_density(alpha = 0.5) +
  labs(title = "Review Distribution by Satisfaction Levels", x = "Reviews", y = "Density") +
  scale_x_log10() +
  theme_minimal()



library(ggplot2)
ggplot(dataset, aes(x = Type, y = Satisfaction, fill = Type)) +
  geom_violin() +
  labs(title = "Satisfaction by Drug Type", x = "Type", y = "Satisfaction") +
  theme_minimal()


library(dplyr)
library(ggplot2)
condition_form_count <- dataset %>%
  group_by(Condition, Form) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count))

ggplot(condition_form_count, aes(x = reorder(Condition, Count), y = Count, fill = Form)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Medication Forms by Condition", x = "Condition", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



ggplot(dataset, aes(x = Effective, y = EaseOfUse, color = as.factor(Satisfaction))) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_d() +
  labs(title = "Effectiveness vs. Ease of Use Colored by Satisfaction", x = "Effectiveness", y = "Ease of Use") +
  theme_minimal()

# Install the reshape2 package
install.packages("reshape2")

# Load the reshape2 package
library(reshape2)

library(reshape2)
review_satisfaction_matrix <- dcast(dataset, Drug ~ Satisfaction, value.var = "Reviews", fun.aggregate = sum, na.rm = TRUE)
library(ggplot2)
ggplot(melt(review_satisfaction_matrix), aes(x = Drug, y = variable, fill = value)) +
  geom_tile() +
  labs(title = "Heatmap of Reviews and Satisfaction by Drug", x = "Drug", y = "Satisfaction") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



ggplot(dataset, aes(x = Price, y = Reviews, size = Effective, color = Effective)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Bubble Chart of Price vs. Reviews by Effectiveness", x = "Price", y = "Reviews") +
  theme_minimal()
