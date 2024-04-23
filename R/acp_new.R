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

# Import the full dataset
dados <- read_excel("Combined_Data.xlsx")

print(colnames(dados))
str(dados)

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


plot_pca_biplot <- function(pca_result) {
  # Extract scores
  scores_df <- data.frame(pca_result$scores[, 1:2])
  colnames(scores_df) <- c("PC1", "PC2")
  scores_df$ID <- rownames(scores_df)

  # Extract loadings
  loadings_df <- data.frame(pca_result$loadings[, 1:2])
  colnames(loadings_df) <- c("PC1", "PC2")
  loadings_df$Variable <- rownames(loadings_df)

  # Create the plotly plot
  pca_fig <- plot_ly() %>%
    add_markers(data = scores_df, x = ~PC1, y = ~PC2, text = ~ID, name = 'Scores', marker = list(color = 'blue', size = 10)) %>%
    add_text(data = loadings_df, x = ~PC1, y = ~PC2, text = ~Variable, name = 'Loadings', textposition = 'top right', textfont = list(color = 'red')) %>%
    layout(title = "PCA Biplot",
           xaxis = list(title = 'PC1'),
           yaxis = list(title = 'PC2'),
           legend = list(orientation = 'h', x = 0.1, y = 1.1))

  return(pca_fig)
}




process_data_for_year <- function(data, year) {
  # Filter data for the given year
  year_data <- data[data$Year == year,]


  # Exclude non-score columns
  columns_to_remove <- c("Year", "Economy")
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
  bartlett_results <- bartlett.test(mydata)
  print(bartlett_results)

  # Save Bartlett test results to a file
  bartlett_output_path <- paste("data/bartlett_test_results_", year, ".txt", sep="")
  writeLines(capture.output(bartlett_results), bartlett_output_path)

  # PCA and Factor Analysis
  fit <- princomp(mydata, cor = TRUE)
  print(summary(fit))
  princomp_output_path <- paste("data/princomp_results_", year, ".txt", sep="")
  writeLines(capture.output(summary(fit)), princomp_output_path)

  # Plot PCA biplot using plotly
  pca_fig <- plot_pca_biplot(fit)
  save_image(pca_fig, file = paste("data/plot_pca_", year, ".png", sep=""))


  mydata2 <- cbind(mydata, fit$scores)

  fit2 <- principal(mydata, nfactors = 1, rotate = "varimax")
  print(summary(fit2))
  principal_output_path <- paste("data/principal_results_", year, ".txt", sep="")
  writeLines(capture.output(summary(fit2)), principal_output_path)

  # Reliability analysis
  # F1 <- data.frame(mydata)
  # cronbach_alpha_full <- cronbach.alpha(F1)
  # results <- sapply(names(mydata), function(x) {
  #   cronbach.alpha(F1[, names(F1) != x])
  # })

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

