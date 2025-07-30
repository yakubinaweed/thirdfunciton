# serversecond/plotting.R

# Load necessary libraries (ensure they are installed: install.packages("ggplot2"))
library(ggplot2)

# Function to plot Age vs HGB with clusters
# df: Data frame containing Age, HGB, and cluster columns
# age: Name of the age column (string)
# hgb: Name of the HGB column (string)
plot_age_hgb <- function(df, age, hgb) {
  if (!age %in% colnames(df) || !hgb %in% colnames(df) || !"cluster" %in% colnames(df)) {
    stop("Data frame must contain 'Age', 'HGB', and 'cluster' columns for plotting.")
  }
  if (!is.numeric(df[[age]]) || !is.numeric(df[[hgb]])) {
    stop("Age and HGB columns must be numeric for plotting.")
  }

  p <- ggplot(df, aes_string(x = age, y = hgb, color = "cluster")) +
    geom_point(alpha = 0.6, size = 2) +
    labs(
      title = "HGB vs. Age with GMM Subpopulations",
      x = paste("Age (", age, ")"),
      y = paste("Hemoglobin (", hgb, ")"),
      color = "Cluster"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  return(p)
}