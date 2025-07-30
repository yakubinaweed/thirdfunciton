# serversecond/utils.R

# Load necessary libraries (ensure they are installed: install.packages("car"))
# The car package contains the powerTransform function which implements Yeo-Johnson
library(car) # For yeo_johnson_transform

# Function to guess column names based on a list of keywords
# col_names: Vector of column names from the data frame
# keywords: Vector of keywords to search for
guess_column <- function(col_names, keywords) {
  for (keyword in keywords) {
    if (any(grepl(keyword, col_names, ignore.case = TRUE))) {
      return(col_names[grep(keyword, col_names, ignore.case = TRUE)][1])
    }
  }
  return("") # Return empty string if no match
}

# Function to generate a safe and unique filename
# base_name: The base name for the file (e.g., "gmm_plot")
# extension: The file extension (e.g., "png", "csv")
generate_safe_filename <- function(base_name, extension) {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0(base_name, "_", timestamp, ".", extension)
  return(filename)
}

# Function to perform Yeo-Johnson transformation
# x: A numeric vector
# The lambda parameter is estimated internally if not provided.
# Note: This function is provided as requested, but is not currently
# explicitly used in the GMM analysis flow as Z-normalization is applied.
yeo_johnson_transform <- function(x) {
  if (!is.numeric(x)) {
    stop("Input to yeo_johnson_transform must be a numeric vector.")
  }
  # powerTransform can handle negative values.
  # It estimates lambda internally.
  transformed_x <- tryCatch({
    # powerTransform returns an object, we need the transformed data
    # For Yeo-Johnson, it automatically determines the best lambda
    # and applies the transformation.
    car::powerTransform(x, family="yj")$y
  }, error = function(e) {
    warning(paste("Error during Yeo-Johnson transformation:", e$message, "Returning original vector."))
    return(x) # Return original if transformation fails
  })
  return(transformed_x)
}