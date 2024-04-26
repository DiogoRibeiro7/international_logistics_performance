# Load required libraries
library(readxl)
library(MVN)
library(QuantPsyc)
library(energy)
library(Hmisc)
library(plotly)
library(psych)
library(ltm)
library(webshot)
library(reticulate)
library(car)
library(cluster)
library(psych)
library(readr)

# Set the number of digits to display (setting to 6 or more is usually sufficient to ensure at least 4 decimal places)
options(digits=3)


# Import the full dataset
dados <- read_excel("Combined_Data.xlsx")

print(colnames(dados))
str(dados)

# Exclude non-score columns
columns_to_remove <- c("LPI Grouped Rank", "Customs Grouped Rank", "Infrastructure Grouped Rank","International Shipments Grouped Rank", "Logistics Competence and Quality Grouped Rank","Timeliness Grouped Rank", "Tracking and Tracing Grouped Rank")
# Safely exclude non-score columns
columns_to_keep <- setdiff(names(dados), columns_to_remove)
dados <- dados[, columns_to_keep, drop = FALSE]

calculate_reliability <- function(data) {
  # Calculate Cronbach's Alpha for the full set of items
  full_alpha <- cronbach.alpha(data)
  cat("Cronbach's Alpha for full data: ", full_alpha$alpha, "\n")

  # Initialize a list to hold results
  results <- list(Full = full_alpha$alpha)

  # Loop over each column in the data frame
  for (i in 1:ncol(data)) {
    # Create a subset by excluding the current column
    subset_data <- data[, -i, drop = FALSE]
    print(colnames(subset_data))
    # Calculate Cronbach's Alpha for the subset
    alpha_value <- cronbach.alpha(subset_data)
    # Store the result
    column_name <- names(data)[i]
    results[[column_name]] <- alpha_value$alpha
    cat("Cronbach's Alpha without '", column_name, "': ", alpha_value$alpha, "\n")
  }

  # Return a list containing all alpha values
  return(results)
}

create_loadings_heatmap <- function(pca_result) {
  loadings <- pca_result$loadings
  heatmap_fig <- plot_ly(x = colnames(loadings), y = rownames(loadings), z = as.matrix(loadings), type = "heatmap", colors = colorRamp(c("blue", "white", "red")))
  heatmap_fig %>% layout(title = "PCA Loadings Heatmap")
}

create_cumulative_explained_variance_plot <- function(pca_result, year) {
  eigenvalues <- pca_result$sdev^2
  cumulative_pct <- cumsum(eigenvalues) / sum(eigenvalues)
  plot_ly(x = 1:length(eigenvalues), y = cumulative_pct, type = 'scatter', mode = 'lines+markers',
          marker = list(size = 10, color = 'red'), line = list(color = 'red')) %>%
    layout(title = "Cumulative Explained Variance", xaxis = list(title = "Number of Components"), yaxis = list(title = "Cumulative Proportion of Variance Explained"))
}

plot_cluster_analysis <- function(pca_scores) {
  clusters <- kmeans(pca_scores[, 1:2], centers = 3)  # Adjust number of centers according to need
  plot_ly(x = pca_scores[, 1], y = pca_scores[, 2], type = 'scatter', mode = 'markers', color = clusters$cluster) %>%
    layout(title = "Cluster Analysis of PCA Scores")
}

perform_shapiro_test <- function(data) {
  shapiro_results <- apply(data, 2, shapiro.test)
  lapply(shapiro_results, function(x) x$p.value)
}

perform_bartletts_test <- function(data) {
  cor_matrix <- cor(data)
  bartlett.test(cor_matrix)
}

perform_levenes_test <- function(data, group) {
  leveneTest(data, group)
}

perform_bartletts_sphericity_test <- function(data) {
  # The function cor.test requires raw data as input, not the correlation matrix
  test_results <- cortest.bartlett(data)
  return(test_results)
}


create_scree_plot <- function(pca_result, year) {
  eigenvalues <- pca_result$sdev^2
  pct_explained <- eigenvalues / sum(eigenvalues)
  scree_plot <- plot_ly(x = 1:length(eigenvalues), y = pct_explained, type = 'scatter', mode = 'lines+markers',
                        marker = list(size = 10, color = 'blue'), line = list(color = 'blue')) %>%
    layout(title = "Scree Plot", xaxis = list(title = "Principal Component"),
           yaxis = list(title = "Percentage of Variance Explained"))
  save_image(scree_plot, file = paste("data/scree_plot_", year, ".png", sep=""))
}

plot_pca_biplot <- function(pca_result) {
  scores_df <- data.frame(pca_result$scores[, 1:2])
  colnames(scores_df) <- c("PC1", "PC2")
  scores_df$ID <- rownames(scores_df)

  loadings_df <- data.frame(pca_result$loadings[, 1:2])
  colnames(loadings_df) <- c("PC1", "PC2")
  loadings_df$Variable <- rownames(loadings_df)

  pca_fig <- plot_ly() %>%
    add_markers(data = scores_df, x = ~PC1, y = ~PC2, text = ~ID, name = 'Scores', marker = list(color = 'blue', size = 10)) %>%
    add_text(data = loadings_df, x = ~PC1, y = ~PC2, text = ~Variable, name = 'Loadings', textposition = 'top right', textfont = list(color = 'red')) %>%
    layout(title = "PCA Biplot", xaxis = list(title = 'PC1'), yaxis = list(title = 'PC2'), legend = list(orientation = 'h', x = 0.1, y = 1.1))
  pca_fig
}

calculate_reliability <- function(data) {
  full_alpha <- cronbach.alpha(data)
  results <- list(Full = full_alpha$alpha)

  for (i in 1:ncol(data)) {
    subset_data <- data[, -i, drop = FALSE]
    alpha_value <- cronbach.alpha(subset_data)
    column_name <- names(data)[i]
    results[[column_name]] <- alpha_value$alpha
  }
  results
}

create_histogram <- function(data, column_name, nbinsx = 30, color = 'rgba(100, 200, 102, 0.7)', title = "Histogram") {
  # Ensure the column exists in the dataframe
  if (!column_name %in% names(data)) {
    stop("Column name '", column_name, "' does not exist in the dataframe")
  }

  # Safely create the histogram by properly handling the column name in the formula
  histogram <- plot_ly(data, x = as.formula(paste0("~`", column_name, "`")), type = "histogram",
                       histnorm = "", nbinsx = nbinsx,
                       marker = list(color = color),
                       name = column_name) %>%
    layout(title = paste(title, "of", column_name),
           xaxis = list(title = column_name),
           yaxis = list(title = "Count"))

  return(histogram)
}


process_data_for_year <- function(data, year) {
  # Filter data for the given year
  year_data <- data[data$Year == year,]
  colnames(year_data) <- trimws(colnames(year_data))
  columns_of_interest <- c("Customs Score", "Infrastructure Score", "International Shipments Score",
                           "Logistics Competence and Quality Score", "Timeliness Score", "Tracking and Tracing Score")

  for (column_name in columns_of_interest) {
    histogram_plot <- create_histogram(year_data, column_name, nbinsx = 20, color = 'rgba(100, 150, 250, 0.8)', title = "Histogram")
    print(histogram_plot)  # This will display the plot in an interactive R environment

    # Optionally, save each histogram to a file
    plotly::save_image(histogram_plot, file = paste("data/histogram_", column_name, year,".png", sep = ""))
  }


  # Exclude non-score columns
  columns_to_remove <- c("Year", "Economy", "LPI Score", "Code")
  # Safely exclude non-score columns
  columns_to_keep <- setdiff(names(year_data), columns_to_remove)
  dados_ACP <- year_data[, columns_to_keep, drop = FALSE]
  mydata <- dados_ACP

  # Perform the analyses as before
  print(mult.norm(mydata))
  multi_norm_data <- mult.norm(mydata)
  mult_norm_output_path <- paste("data/multi_norm_data_", year, ".txt", sep="")
  writeLines(capture.output(multi_norm_data), mult_norm_output_path)
  print(mvnorm.etest(mydata, R=100))
  multi_etest_data <- mvnorm.etest(mydata, R=100)
  mult_etest_output_path <- paste("data/multi_etest_data_", year, ".txt", sep="")
  writeLines(capture.output(multi_etest_data), mult_etest_output_path)

  # Correlation and visualization using Plotly
  res <- cor(mydata)
  fig <- plot_ly(z = res, x = colnames(res), y = colnames(res), type = "heatmap", colors = colorRamp(c("blue", "white", "red")))
  fig <- fig %>% layout(title = "Correlation Matrix", xaxis = list(tickangle = 45))
  save_image(fig, file = paste("data/cor_", year, ".png", sep=""))


  # Homoscedasticity checks
  variances <- sapply(mydata, var, na.rm = TRUE)
  standard_devs <- sapply(mydata, sd, na.rm = TRUE)

  standard_devs_output_path <- paste("data/standard_devs_results_", year, ".txt", sep="")
  writeLines(capture.output(standard_devs), standard_devs_output_path)

  # Save Bartlett test results to a file
  bartlett_results <- bartlett.test(mydata)
  print(bartlett_results)

  # Save Bartlett test results to a file
  bartlett_output_path <- paste("data/bartlett_test_results_", year, ".txt", sep="")
  writeLines(capture.output(bartlett_results), bartlett_output_path)


  kmo_mydata <- KMO(mydata)
  print(kmo_mydata)
  kmo_output_path <- paste("data/kmo_data_", year, ".txt", sep="")
  writeLines(capture.output(kmo_mydata), kmo_output_path)

  # PCA and Factor Analysis
  fit <- princomp(mydata, cor = TRUE)
  print(summary(fit))
  princomp_output_path <- paste("data/princomp_results_", year, ".txt", sep="")
  writeLines(capture.output(summary(fit)), princomp_output_path)

  principal_output_path <- paste("data/principal_loadings_", year, ".txt", sep="")
  writeLines(capture.output(fit$loadings), principal_output_path)

  # Convert loadings to a data frame and make sure it is numeric
  loadings_df <- as.data.frame(fit$loadings[, "Comp.1", drop = FALSE])

  # If the column is a complex number, take only the real part
  if ("complex" %in% sapply(loadings_df, class)) {
    loadings_df$Comp.1 <- Re(loadings_df$Comp.1)
  }

  # Ensure the column is numeric
  loadings_df$Comp.1 <- as.numeric(loadings_df$Comp.1)

  # Order the loadings by the first component, descending
  ordered_loadings <- loadings_df[order(-loadings_df$Comp.1), , drop = FALSE]

  ordered_loadings_path <- paste("data/ordered_loadings_", year, ".txt", sep="")
  writeLines(capture.output(ordered_loadings), ordered_loadings_path)

  create_scree_plot(fit, year)

  # Plot PCA biplot using plotly
  pca_fig <- plot_pca_biplot(fit)
  save_image(pca_fig, file = paste("data/plot_pca_", year, ".png", sep=""))

  heatmap_fig <- create_loadings_heatmap(fit)
  cumulative_variance_fig <- create_cumulative_explained_variance_plot(fit, year)
  cluster_fig <- plot_cluster_analysis(fit$scores[, 1:2])

  shapiro_results <- perform_shapiro_test(mydata)

  # Perform Bartlett's test of sphericity
  sphericity_results <- perform_bartletts_sphericity_test(mydata)
  print(sphericity_results)


  mydata2 <- cbind(mydata, fit$scores)

  fit2 <- principal(mydata, nfactors = 1, rotate = "varimax")
  print(summary(fit2))

  principal_output_path <- paste("data/principal_results_", year, ".txt", sep="")
  writeLines(capture.output(summary(fit2)), principal_output_path)

  principal_pc1_output_path <- paste("data/principal_results_pc1_", year, ".txt", sep="")
  writeLines(capture.output(fit2$loadings, decimal=4), principal_pc1_output_path)

  # Assuming fit2 is already your PCA result object
  loadings_matrix <- loadings(fit2)  # Get loadings matrix

  # Convert matrix to data frame for better handling
  loadings_df <- as.data.frame(loadings_matrix)

  # Optional: Format the dataframe to ensure all numerical values have four decimal places
  loadings_df[] <- lapply(loadings_df, function(x) if(is.numeric(x)) round(x, 4) else x)

  # Rename the columns to include the year. Assuming that your PCA loadings typically include multiple PCs:
  colnames(loadings_df) <- paste(colnames(loadings_df), year, sep="_")

  # Define the path for saving the CSV file
  principal_pc1_output_path <- paste("data/principal_results_pc1_", year, ".csv", sep="")

  # Save the DataFrame to a CSV file with row names
  write.csv(loadings_df, principal_pc1_output_path, row.names = TRUE)


  # Assuming mydata is already loaded and prepared
  # Define the columns of interest for the reliability analysis
  columns_of_interest <- c("Customs Score", "Infrastructure Score", "International Shipments Score",
                           "Logistics Competence and Quality Score", "Timeliness Score", "Tracking and Tracing Score")

  # Subset mydata for the reliability analysis
  survey_data <- mydata[columns_of_interest]

  # Calculate reliability statistics
  reliability_results <- calculate_reliability(survey_data)

  # Save reliability results to a text file
  reliability_output_path <- paste("data/reliability_results_", year, ".txt", sep="")
  write.table(reliability_results, file = reliability_output_path, append = TRUE, sep = "\t", col.names = NA)


  print(year)
  # print(cronbach_alpha_full)
  print(reliability_results)
}


# Extract unique years from the data
unique_years <- unique(dados$Year)

# Apply the function to each year
results <- lapply(unique_years, function(year) {
  process_data_for_year(dados, year)
})

base_path <- "data/"
years <- unique(dados$Year)

# Define score names to be used as row names
score_names <- c("Customs Score", "Infrastructure Score", "International Shipments Score",
                 "Logistics Competence and Quality Score", "Timeliness Score", "Tracking and Tracing Score")


# Initialize an empty list to store data frames for each year
data_frames <- list()

# Loop over each year, read the CSV file, and prepare the DataFrame
for (year in years) {
  # Construct file path
  file_path <- paste(base_path, "principal_results_pc1_", year, ".csv", sep = "")

  # Read the CSV file
  df <- read_csv(file_path, col_types = cols(.default = "c"))

  # Remove the first unnamed column which is just the row index from the CSV
  df <- df[-1]

  # Add the scores as a new column at the start of the dataframe
  df <- tibble(Score = score_names, Value = as.numeric(df[[1]]))

  # Store the dataframe with a named list
  data_frames[[as.character(year)]] <- df
}

# Combine all DataFrames into a single DataFrame by 'Score' column
final_dataframe <- bind_rows(data_frames, .id = "Year") %>%
  pivot_wider(names_from = Year, values_from = Value)

# Print or inspect the final dataframe
print(final_dataframe)



