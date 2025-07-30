# serversecond/gmms.R

# Load necessary libraries (ensure they are installed: install.packages("mclust"))
library(mclust)
library(dplyr) # For data manipulation if needed, often used with mclust results

# Function to run GMM and select optimal model/number of components
# data_mat: A numeric matrix or data frame with columns for GMM (e.g., HGB and Age)
# G_range: A vector specifying the range of number of components (G) to consider
# modelNames: A vector of model types to consider (e.g., "VVV", "EII", etc.)
run_gmm <- function(data_mat, G_range = 1:9, modelNames = NULL) {
  if (!is.matrix(data_mat) && !is.data.frame(data_mat)) {
    stop("data_mat must be a numeric matrix or data frame.")
  }
  if (!all(sapply(data_mat, is.numeric))) {
    stop("All columns in data_mat must be numeric.")
  }
  if (is.null(modelNames)) {
    # Default multivariate models for 2D data
    # VVV: variable volume, shape, and orientation
    # EEE: equal volume, shape, and orientation
    # VEI: variable volume, equal shape, identity orientation
    # etc. See ?mclust.options for full list
    modelNames <- c("VVV", "EII", "VII", "EEI", "VEV", "EVE", "VEE", "EEV")
    # You can adjust these based on your data characteristics
  }

  print(paste("DEBUG: Running Mclust with G_range =", paste(G_range, collapse = ", "), "and modelNames =", paste(modelNames, collapse = ", ")))

  mclust_fit <- tryCatch({
    Mclust(data = data_mat, G = G_range, modelNames = modelNames)
  }, error = function(e) {
    stop(paste("Error fitting Mclust model:", e$message))
  })

  if (is.null(mclust_fit)) {
    stop("Mclust did not return a valid fit.")
  }

  return(mclust_fit)
}

# Function to assign clusters back to the original data frame
# df: The original (non-transformed) data frame used for plotting/summary
# gmm_model: The result object from Mclust
assign_clusters <- function(df, gmm_model) {
  if (is.null(gmm_model) || !inherits(gmm_model, "Mclust")) {
    stop("Invalid Mclust model object provided.")
  }
  if (is.null(gmm_model$classification)) {
    stop("Mclust model does not contain cluster classifications.")
  }

  # Ensure the number of rows matches between the input df and the classification results
  if (nrow(df) != length(gmm_model$classification)) {
    warning("Row count mismatch between input data frame and Mclust classification. Attempting to align by assuming original order.")
    # This scenario should ideally be prevented by ensuring data passed to Mclust
    # and data for df are aligned (e.g., by handling NAs consistently)
    # If df was the source of data_for_mclust, and NAs were removed,
    # you might need to re-align using an original row index if you want
    # to assign clusters to the *original* full dataset including NAs.
    # For now, we assume df here is the same 'data_for_mclust' used in server.R
    # which has already been cleaned (na.omit) and should match the classification length.
  }

  # Add the cluster assignments to the data frame
  df$cluster <- as.factor(gmm_model$classification)
  return(df)
}