# Load the packages
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(psych)
library(MASS)

# Load the dataset
data <- read_excel("Combined_Data.xlsx")

# Check for missing values and remove or impute
summary(data)  # Get a summary for initial analysis
data <- na.omit(data)  # Remove rows with NA values; consider imputation if necessary

# Optionally, convert factors if needed
data$Year <- as.factor(data$Year)  # Convert Year to a factor if it's not already

# Basic statistics
summary(data)

# Create histograms for all numeric columns
lapply(names(data), function(column_name) {
  if (is.numeric(data[[column_name]])) {
    g <- ggplot(data, aes(x = .data[[column_name]])) +
      geom_histogram(bins = 30, fill = "blue", color = "black") +
      ggtitle(paste("Histogram of", column_name)) +
      xlab(column_name) +
      ylab("Frequency")
    print(g)
  }
})

# Create boxplots for each numeric variable by Year
lapply(names(data), function(column_name) {
  if (is.numeric(data[[column_name]])) {
    g <- ggplot(data, aes(x = Year, y = .data[[column_name]])) +  # Correctly referencing columns with spaces
      geom_boxplot() +
      ggtitle(paste("Boxplot of", column_name, "by Year")) +
      xlab("Year") +
      ylab(column_name)
    print(g)
  }
})


# ANOVA to test if means of a numeric variable are the same across years
if ("VariableName" %in% names(data) && "Year" %in% names(data)) {
  anova_results <- aov(VariableName ~ Year, data = data)
  summary(anova_results)
}

# Correlation analysis between two variables
if (all(c("Var1", "Var2") %in% names(data))) {
  cor_results <- cor.test(data$Var1, data$Var2)
  print(cor_results)
}

# Linear Regression Analysis if you want to predict a variable based on others
lm_results <- lm(VariableName ~ Var1 + Var2, data = data)
summary(lm_results)

# Principal Component Analysis for dimensionality reduction
if (ncol(data) > 3) {  # PCA makes sense for multidimensional data
  pca_results <- prcomp(data[, sapply(data, is.numeric)], scale. = TRUE)
  print(summary(pca_results))
  plot_pca_biplot(pca_results)  # Assume plot_pca_biplot is defined as per previous discussions
}


# Save plots and results to files
# Save the plot as a PNG file
ggsave("histogram.png", plot = last_plot(), width = 10, height = 6, dpi = 300)
write.csv(summary(lm_results), "linear_model_summary.csv")

