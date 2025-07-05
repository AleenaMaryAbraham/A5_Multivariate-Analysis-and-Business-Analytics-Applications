# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
}

# List of packages to install and load
packages <- c("dplyr", "psych", "tidyr", "GPArotation", "FactoMineR", "factoextra", "pheatmap")

# Install and load necessary packages
install_and_load(packages)

# Load the necessary libraries
library(dplyr)
library(psych)
library(tidyr)
library(GPArotation)
library(FactoMineR)
library(factoextra)
library(pheatmap)

# Load the dataset
dataset_path <- "C:/Users/Aleena Mary Abraham/OneDrive/Desktop/SCMA632_2025/DATA/Survey.csv"
survey_data <- read.csv(dataset_path)

# Inspect the dataset
str(survey_data)
summary(survey_data)

# Dataset contains both categorical and numerical variables, we will focus on the numerical variables for PCA and FA
# Select only the numerical variables for PCA and FA
# Assuming numerical variables are those that are integers or numeric
numerical_data <- survey_data %>% select(where(is.numeric))

# Standardize the data
survey_data_scaled <- scale(numerical_data)

# Perform PCA using FactoMineR
pca_result <- FactoMineR::PCA(survey_data_scaled, graph = FALSE)

# Summary of PCA results
print(summary(pca_result))

# Visualize the scree plot
factoextra::fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50), main = "Scree Plot")

# Visualize the variables on the principal component map (Correlation Circle)
factoextra::fviz_pca_var(pca_result, col.var = "cos2", 
                         gradient.cols = c("blue", "yellow", "red"),
                         repel = TRUE, title = "PCA - Correlation Circle")

# Visualize individuals on the principal component map
factoextra::fviz_pca_ind(pca_result, col.ind = "cos2", 
                         gradient.cols = c("blue", "yellow", "red"),
                         repel = TRUE, title = "PCA - Individuals")

# Determine the number of factors for FA using parallel analysis
fa_parallel <- psych::fa.parallel(survey_data_scaled, fa = "fa")
print(fa_parallel)

# Perform Factor Analysis with the chosen number of factors 
fa_result <- psych::fa(survey_data_scaled, nfactors = 4, rotate = "varimax")

# Print FA results
print(fa_result)

# Factor Loadings
fa_loadings <- fa_result$loadings
print(fa_loadings)

# Plot Factor Analysis results
psych::fa.diagram(fa_result, main = "Factor Analysis Diagram")

# Heatmap of Factor Loadings using pheatmap
loadings_matrix <- as.matrix(fa_loadings)
pheatmap::pheatmap(loadings_matrix, cluster_rows = TRUE, cluster_cols = TRUE, main = "Heatmap of Factor Loadings")