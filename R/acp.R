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

# Import data
dados2023 <- read_excel("International_LPI_from_2007_to_2023.xlsx", sheet = "2023")
View(dados2023)

# Select relevant columns by excluding non-score columns
dados_ACP <- subset(dados2023, select = -c("LPI Grouped Rank", "Customs Grouped Rank",
                                           "Infrastructure Grouped Rank", "International Shipments Grouped Rank",
                                           "Logistics Competence and Quality Grouped Rank",
                                           "Timeliness Grouped Rank", "Tracking and Tracing Grouped Rank"))
View(dados_ACP)
mydata <- dados_ACP

# Assumption Checks
# ------------------
# Check for multivariate normality

# Mardia's test for multivariate normality
mult.norm(mydata)

# Energy Test for multivariate normality
dim(mydata)
mvnorm.etest(mydata, R=100)

# Linear Correlations Check
# Calculate and display the correlation matrix and significance
matrix_corr <- rcorr(as.matrix(mydata))
corrplot(cor(mydata), type = "upper", tl.col = "black", tl.srt = 45)
ggcorrplot(cor(mydata))

# Homoscedasticity Check
# Compute variances and standard deviations
variances <- sapply(mydata, var, na.rm = TRUE)
standard_devs <- sapply(mydata, sd, na.rm = TRUE)

# Bartlett's test for homogeneity of variances
bartlett.test(mydata)

# Principal Components Analysis (PCA)
# ------------------------------------
# Kaiser-Meyer-Olkin test for sampling adequacy
KMO(mydata)

# Run PCA
fit <- princomp(mydata, cor = TRUE)
summary(fit)

# Explore PCA results
loadings(fit)
biplot(fit)  # Biplot for the first two principal components

# Bind PCA scores to the original dataset
mydata2 <- cbind(mydata, fit$scores)
View(mydata2)

# Determine number of components to retain based on various criteria
screeplot(fit, type = "lines")
plot(fit, type = "lines")

# Eigenvalue and scree plot analysis
eigen_info <- eigen(cor(mydata))
plot(cumsum(eigen_info$values) / sum(eigen_info$values), type = "o", xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained", main = "Scree Plot")

# Factor Analysis - Feature Extraction
# -------------------------------------
fit2 <- principal(mydata, nfactors = 1, rotate = "varimax")
summary(fit2)

# Reliability Analysis
# --------------------
# Compute Cronbach's Alpha for different subsets of items
F1 <- data.frame(mydata)
cronbach_alpha_full <- cronbach.alpha(F1)

# Remove one item at a time and compute Cronbach's Alpha
results <- sapply(names(mydata), function(x) {
  cronbach.alpha(F1[, names(F1) != x])
})

# Print results
print(cronbach_alpha_full)
print(results)
