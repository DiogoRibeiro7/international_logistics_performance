# Load required libraries
library(readxl)
library(MVN)
library(QuantPsyc)
library(energy)
library(Hmisc)
library(corrplot)
library(ggcorrplot)
library(psych)
library(ltm)

# Import the full dataset
dados <- read_excel("Combined_Data.xlsx")
View(dados)
print(colnames(dados))
str(dados)

process_data_for_year <- function(data, year) {
  # Filter data for the given year
  year_data <- data[data$Year == year,]
  View(year_data)
  # Exclude non-score columns
  columns_to_remove <- c("Year", "Economy")
  # Safely exclude non-score columns
  columns_to_keep <- setdiff(names(year_data), columns_to_remove)
  dados_ACP <- year_data[, columns_to_keep, drop = FALSE]
  View(dados_ACP)
  mydata <- dados_ACP

  # Perform the analyses as before
  print(mult.norm(mydata))
  print(mvnorm.etest(mydata, R=100))

  # Correlation and visualization
  matrix_corr <- rcorr(as.matrix(mydata))
  corrplot(cor(mydata), type = "upper", tl.col = "black", tl.srt = 45)
  ggcorrplot(cor(mydata))

  # Homoscedasticity checks
  variances <- sapply(mydata, var, na.rm = TRUE)
  standard_devs <- sapply(mydata, sd, na.rm = TRUE)
  print(bartlett.test(mydata))

  # PCA and Factor Analysis
  fit <- princomp(mydata, cor = TRUE)
  print(summary(fit))
  loadings(fit)
  biplot(fit)
  mydata2 <- cbind(mydata, fit$scores)
  View(mydata2)

  fit2 <- principal(mydata, nfactors = 1, rotate = "varimax")
  print(summary(fit2))

  # Reliability analysis
  F1 <- data.frame(mydata)
  cronbach_alpha_full <- cronbach.alpha(F1)
  results <- sapply(names(mydata), function(x) {
    cronbach.alpha(F1[, names(F1) != x])
  })
  print(year)
  print(cronbach_alpha_full)
  print(results)
}

# Extract unique years from the data
unique_years <- unique(dados$Year)


# Apply the function to each year
results <- lapply(unique_years, function(year) {
  process_data_for_year(dados, year)
})


