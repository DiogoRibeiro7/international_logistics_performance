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

columns_of_interest <- c("LPI Score", "Customs Score", "Infrastructure Score", "International Shipments Score",
                         "Logistics Competence and Quality Score", "Timeliness Score", "Tracking and Tracing Score")


# Loop through each variable and perform ANOVA
for (variable in columns_of_interest) {
  if (variable %in% names(data)) {
    # Perform ANOVA to check if means are different across years for each variable
    anova_results <- aov(reformulate("Year", response = variable), data = data)
    print(paste("ANOVA results for", variable))
    print(summary(anova_results))

    # Optionally, create a boxplot for visual inspection
    p <- ggplot(data, aes(x = Year, y = .data[[variable]])) +
      geom_boxplot() +
      ggtitle(paste("Boxplot of", variable, "by Year")) +
      xlab("Year") +
      ylab(variable)
    print(p)

    # Save the plot (optional)
    ggsave(paste0("data_analyses/boxplot_", gsub(" ", "_", variable), ".png"), plot = p, width = 10, height = 6, dpi = 300)
  } else {
    print(paste("Variable", variable, "not found in dataset."))
  }
}

# Loop through each pair of variables to perform correlation analysis
for (i in 1:(length(columns_of_interest) - 1)) {
  for (j in (i + 1):length(columns_of_interest)) {
    var1 <- columns_of_interest[i]
    var2 <- columns_of_interest[j]

    # Ensure both variables are in the dataset
    if (all(c(var1, var2) %in% names(data))) {
      # Compute Pearson correlation
      cor_results <- cor.test(data[[var1]], data[[var2]], method = "pearson")
      cat("Correlation results between", var1, "and", var2, ":\n")
      print(cor_results)

      # Optionally, plot the relationship
      p <- ggplot(data, aes_string(x = var1, y = var2)) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE, color = "blue") +
        ggtitle(paste("Scatter Plot between", var1, "and", var2)) +
        theme_minimal()
      print(p)

      # Save the plot (optional)
      ggsave(filename = paste("scatter_", gsub(" ", "_", var1), "_", gsub(" ", "_", var2), ".png", sep = ""),
             plot = p, width = 8, height = 6, dpi = 300)
    } else {
      cat("One or both variables", var1, "and", var2, "are not found in the dataset.\n")
    }
  }
}

run_linear_regression <- function(data, dependent_var, independent_vars) {
  # Construct the formula for linear regression
  formula <- as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + ")))

  # Run linear regression
  lm_results <- lm(formula, data = data)

  # Print the summary of the linear model
  print(summary(lm_results))

  # Optionally, return the results for further analysis
  return(lm_results)
}

run_linear_regression <- function(data, dependent_var, independent_vars) {
  # Safely construct the formula by wrapping variable names in backticks
  formula_text <- paste0("`", dependent_var, "`", "~", paste0("`", independent_vars, "`", collapse = " + "))
  formula <- as.formula(formula_text)

  # Run linear regression
  lm_results <- lm(formula, data = data)

  # Print and return the summary of the linear model
  print(summary(lm_results))
  return(lm_results)
}

# Changing column names to lower case and replacing spaces with underscores
data_1 <- data %>%
  rename_with(~ tolower(gsub(" ", "_", .x)), .cols = everything())
# Example usage
dependent_var <- "lpi_score"
independent_vars <- c("customs_score", "infrastructure_score", "international_shipments_score",
                      "logistics_competence_and_quality_score", "timeliness_score", "tracking_and_tracing_score")

# Ensure you remove the dependent_var from the independent_vars if it's accidentally included
independent_vars <- setdiff(independent_vars, dependent_var)

# Run the regression analysis
model_results <- run_linear_regression(data_1, dependent_var, independent_vars)

# Plot diagnostics (optional)
plot_diagnostics <- function(lm_results) {
  par(mfrow = c(2, 2))
  plot(lm_results)
  par(mfrow = c(1, 1))  # Reset to default
}

# Run diagnostics
plot_diagnostics(model_results)


# Perform PCA on numeric data only if appropriate
run_custom_pca <- function(data, scale = TRUE, n_components = NULL) {
  # Filter only numeric columns
  numeric_data <- data[, sapply(data, is.numeric)]

  # Perform PCA with options for scaling and number of principal components
  if (!is.null(n_components)) {
    pca_results <- prcomp(numeric_data, scale. = scale, rank. = n_components)
  } else {
    pca_results <- prcomp(numeric_data, scale. = scale)
  }

  return(pca_results)
}


plot_pca_biplot_plotly <- function(pca_results) {
  scores <- pca_results$x

  # Check if there are at least two components
  if (ncol(scores) < 2) {
    stop("PCA results do not contain enough components.")
  }

  # Ensure there are row names, provide default if not
  row_names <- rownames(scores)
  if (is.null(row_names)) {
    row_names <- as.character(seq_len(nrow(scores)))
  }

  # Create data frame for Plotly
  pca_data <- data.frame(PC1 = scores[,1], PC2 = scores[,2], Label = row_names)

  # Creating the plot
  p <- plot_ly() %>%
    add_trace(data = pca_data, x = ~PC1, y = ~PC2, type = 'scatter', mode = 'markers', text = ~Label,
              marker = list(size = 12, color = 'rgba(255, 182, 193, .9)', line = list(color = 'rgba(152, 0, 0, .8)', width = 0.5)),
              name = 'Scores') %>%
    layout(title = "PCA Biplot",
           xaxis = list(title = 'PC1'),
           yaxis = list(title = 'PC2'),
           legend = list(x = 0.1, y = 0.9))

  return(p)
}


analyze_pca_variance <- function(pca_results) {
  variance_explained <- summary(pca_results)$importance[2,]
  cumulative_variance_explained <- summary(pca_results)$importance[3,]

  print(data.frame(PrincipalComponent = names(variance_explained), VarianceExplained = variance_explained, CumulativeVariance = cumulative_variance_explained))
}

# Assuming 'data' is your DataFrame loaded with appropriate data
pca_results <- run_custom_pca(data, scale = TRUE, n_components = 6)  # Customize the number of components as needed

# Analyze variance explained
analyze_pca_variance(pca_results)
print(pca_results$x)
# Create interactive PCA biplot
pca_plot <- plot_pca_biplot_plotly(pca_results)
pca_plot


# Save plots and results to files
# Save the plot as a PNG file
# ggsave("histogram.png", plot = last_plot(), width = 10, height = 6, dpi = 300)
# write.csv(summary(lm_results), "linear_model_summary.csv")

#
