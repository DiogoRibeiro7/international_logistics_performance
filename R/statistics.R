# Load the packages
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(psych)
library(MASS)

# Load the dataset
data <- read_excel("data_transformed.xlsx")

# Check for missing values and remove or impute
summary(data)  # Get a summary for initial analysis

head(data)


columns_of_interest <- c("Customs Score", "Infrastructure Score", "International Shipments Score",
                         "Logistics Competence and Quality Score", "Timeliness Score", "Tracking and Tracing Score")


for (variable in columns_of_interest) {
  formula <- as.formula(paste("LPI Score ~", variable))  # dynamically create formula
  aov_result <- aov(formula, data=data)
  results[[variable]] <- summary(aov_result)
}

# To view the results:
results
