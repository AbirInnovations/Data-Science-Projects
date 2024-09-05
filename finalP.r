dataset <- read.csv("D:/9th Semester/Mid/Data/project.csv", header=TRUE, sep=",")

library(naniar)
vis_miss(dataset)
missing_age_rows <- which(is.na(dataset$Age))
print(missing_age_rows)

missing_infection_rows <- which(is.na(dataset$Infection))
print(missing_infection_rows)

dataset$AgeCategory <- cut(dataset$Age, breaks = c(-Inf, 18, 65, Inf), labels = c("Young", "Middle-aged", "Senior"))

dataset$Infection <- factor(dataset$Infection, levels = c("yes", "no", "marginal"), labels = c(1, 2, 3))
dataset$Smoking <- factor(dataset$Smoking, levels = c(1, 2, 3), labels = c("yes", "no", "sometimes"))
dataset$RiskLevel <- factor(dataset$RiskLevel, levels = c("high risk", "mid risk", "low risk"), labels = c(1, 2, 3))

library(dplyr)

dataset$Age <- as.integer(as.character(dataset$Age))
Age_median <- round(median(dataset$Age, na.rm = TRUE))
dataset$Age[is.na(dataset$Age)] <- Age_median

get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_infection <- get_mode(dataset$Infection[dataset$Infection != ""])
dataset$Infection[dataset$Infection == ""] <- mode_infection
invalid_values <- !(dataset$Infection %in% c("yes", "no", "marginal"))
dataset$Infection[invalid_values] <- NA

dataset$Smoking <- as.integer(as.character(dataset$Smoking))
Smoking_median <- round(median(dataset$Smoking, na.rm = TRUE))
dataset$Smoking[is.na(dataset$Smoking)] <- Smoking_median

dataset$DiastolicBP <- as.integer(as.character(dataset$DiastolicBP))
DiastolicBP_median <- round(median(dataset$DiastolicBP, na.rm = TRUE))
dataset$DiastolicBP[is.na(dataset$DiastolicBP)] <- DiastolicBP_median

dataset$BS <- as.numeric(as.character(dataset$BS))
BS_mean <- mean(dataset$BS, na.rm = TRUE)
dataset$BS[is.na(dataset$BS)] <- BS_mean

summary_stats_Age <- data.frame(Statistic = c("Mean", "Median", "Mode"), Value = c(mean(dataset$Age, na.rm = TRUE), median(dataset$Age, na.rm = TRUE), as.numeric(names(sort(table(dataset$Age), decreasing = TRUE))[1])))
library(ggplot2)
ggplot(summary_stats_Age, aes(x = Statistic, y = Value)) + geom_bar(stat = "identity") + labs(title = "Summary Statistics of Age", x = "Statistic", y = "Value")

summary_stats_HeartRate <- data.frame(Statistic = c("Mean", "Median", "Mode"), Value = c(mean(dataset$HeartRate, na.rm = TRUE), median(dataset$HeartRate, na.rm = TRUE), as.numeric(names(sort(-table(dataset$HeartRate)))[1])))
ggplot(summary_stats_HeartRate, aes(x = Statistic, y = Value)) + geom_bar(stat = "identity") + labs(title = "Summary Statistics of HeartRate", x = "Statistic", y = "Value")

Q1 <- quantile(dataset$Age, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$Age, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
dataset <- dataset[dataset$Age >= (Q1 - 1.5 * IQR) & dataset$Age <= (Q3 + 1.5 * IQR), ]

Q1 <- quantile(dataset$Age, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$Age, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
outliers <- dataset$Age[which(dataset$Age < lower_bound | dataset$Age > upper_bound)]
print(outliers)

Q1 <- quantile(dataset$Age, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$Age, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
median_age <- median(dataset$Age, na.rm = TRUE)
dataset$Age <- ifelse(dataset$Age < lower_bound | dataset$Age > upper_bound, median_age, dataset$Age)
print(head(dataset))

dataset$DiastolicBP_norm <- (dataset$DiastolicBP - min(dataset$DiastolicBP, na.rm = TRUE)) / (max(dataset$DiastolicBP, na.rm = TRUE) - min(dataset$DiastolicBP, na.rm = TRUE))

Cleaned_dataset <- na.omit(dataset)
