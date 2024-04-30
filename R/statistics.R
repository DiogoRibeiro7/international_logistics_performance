# Load the packages
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(psych)
library(MASS)

# Load the dataset
data <- read_excel("data_transformed.xlsx")

# Exclude non-score columns
columns_to_remove <- c("Year", "Economy", "Code")
# Safely exclude non-score columns
columns_to_keep <- setdiff(names(data), columns_to_remove)
data <- data[, columns_to_keep, drop = FALSE]


# Check for missing values and remove or impute
summary(data)  # Get a summary for initial analysis

head(data)


columns_of_interest <- c("Customs Score", "Infrastructure Score", "International Shipments Score",
                         "Logistics Competence and Quality Score", "Timeliness Score", "Tracking and Tracing Score")


for (variable in columns_of_interest) {
  formulaString <- paste("`LPI Score` ~ `", variable, "`", sep="")
  formula <- as.formula(formulaString)
  print(formula)
  aov_result <- aov(formula, data=data)
  results[[variable]] <- summary(aov_result)
}

# To view the results:
results
