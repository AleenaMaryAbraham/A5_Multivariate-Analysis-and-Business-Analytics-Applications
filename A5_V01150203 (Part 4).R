# Load necessary packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# Read CSV file
df <- read_csv("C:/Users/Aleena Mary Abraham/OneDrive/Desktop/SCMA632_2025/DATA/pizza_data.csv")

# Convert relevant variables to factors
conjoint_attributes <- c("brand","price","weight","crust","cheese","size","toppings","spicy")
df[conjoint_attributes] <- lapply(df[conjoint_attributes], as.factor)

# Fit linear model with effects coding (sum contrasts)
options(contrasts = c("contr.sum", "contr.poly"))

model <- lm(ranking ~ brand + price + weight + crust + cheese + size + toppings + spicy, data = df)
summary(model)

part_worth_list <- list()
level_names <- list()
part_worth_ranges <- c()
important_levels <- list()

start_index <- 2  # Skip intercept

for (attr in conjoint_attributes) {
  levels_attr <- levels(df[[attr]])
  k <- length(levels_attr)
  
  # Extract effect-coded coefficients
  coefs <- coef(model)[start_index:(start_index + k - 2)]
  last_coef <- -sum(coefs)
  part_worths <- c(coefs, last_coef)
  
  part_worth_list[[attr]] <- part_worths
  level_names[[attr]] <- levels_attr
  
  # Identify most preferred level
  important_levels[[attr]] <- which.max(part_worths)
  part_worth_ranges <- c(part_worth_ranges, max(part_worths) - min(part_worths))
  
  start_index <- start_index + k - 1
}

attribute_importance <- round(100 * part_worth_ranges / sum(part_worth_ranges), 2)

# Create importance data frame
importance_df <- data.frame(
  Attribute = conjoint_attributes,
  Importance = attribute_importance
)

# Bar plot of attribute importance
ggplot(importance_df, aes(x = Attribute, y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  ggtitle("Relative Importance of Attributes") +
  xlab("Attributes") + ylab("Importance (%)") +
  theme_minimal()

# Create lookup table
part_worth_dict <- unlist(part_worth_list)
names(part_worth_dict) <- unlist(lapply(names(part_worth_list), function(attr) {
  paste(attr, level_names[[attr]], sep = ":")
}))

# Compute utility for each row
df$utility <- apply(df, 1, function(row) {
  sum(sapply(conjoint_attributes, function(attr) {
    part_worth_dict[[paste(attr, row[[attr]], sep = ":")]]
  }))
})

# Show profile with highest utility
df[which.max(df$utility), ]

for (attr in conjoint_attributes) {
  cat("Preferred level in", attr, "is:", level_names[[attr]][important_levels[[attr]]], "\n")
}