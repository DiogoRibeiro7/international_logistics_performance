perform_longitudinal_pca <- function(data, id_col = "Economy", year_col = "Year") {
  library(dplyr)
  library(tidyr)
  library(psych)
  library(plotly)
  
  # Select only relevant score columns + identifiers
  score_cols <- c("Customs Score", "Infrastructure Score", "International Shipments Score",
                  "Logistics Competence and Quality Score", "Timeliness Score", "Tracking and Tracing Score")
  selected_data <- data %>% select(all_of(c(id_col, year_col, score_cols)))
  
  # Pivot to wide format so each indicator has columns per year
  wide_data <- selected_data %>%
    pivot_longer(cols = all_of(score_cols), names_to = "Indicator", values_to = "Score") %>%
    unite("Indicator_Year", Indicator, !!sym(year_col)) %>%
    pivot_wider(names_from = Indicator_Year, values_from = Score)
  
  # Remove any rows with NA (optional: consider imputation instead)
  wide_data <- na.omit(wide_data)

  # Store row IDs
  row_ids <- wide_data[[id_col]]
  wide_data <- wide_data %>% select(-all_of(id_col))
  
  # Run PCA
  pca_model <- principal(wide_data, nfactors = 2, rotate = "none")
  
  # Scree Plot
  scree_plot <- plot_ly(x = 1:length(pca_model$values), y = pca_model$values, type = 'scatter', mode = 'lines+markers') %>%
    layout(title = "Longitudinal PCA - Scree Plot", xaxis = list(title = "Principal Component"),
           yaxis = list(title = "Eigenvalue"))
  print(scree_plot)
  
  # PCA Biplot
  scores_df <- data.frame(pca_model$scores[, 1:2])
  colnames(scores_df) <- c("PC1", "PC2")
  scores_df$Economy <- row_ids
  
  loadings_df <- data.frame(pca_model$loadings[, 1:2])
  colnames(loadings_df) <- c("PC1", "PC2")
  loadings_df$Variable <- rownames(loadings_df)
  
  biplot_fig <- plot_ly() %>%
    add_markers(data = scores_df, x = ~PC1, y = ~PC2, text = ~Economy, name = 'Scores') %>%
    add_text(data = loadings_df, x = ~PC1, y = ~PC2, text = ~Variable, textfont = list(color = 'red'), name = 'Loadings') %>%
    layout(title = "Longitudinal PCA - Biplot", xaxis = list(title = "PC1"), yaxis = list(title = "PC2"))
  print(biplot_fig)
  
  return(list(model = pca_model, scores = scores_df, loadings = loadings_df))
}


# Assuming your original dataset is loaded into 'dados'
lpca_results <- perform_longitudinal_pca(dados)
