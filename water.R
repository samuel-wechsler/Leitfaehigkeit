# Clear the workspace
rm(list=ls())

# Source the functions
source("functions.R")

# Function to parse temperature and kappa values from a dataframe
parseTempKappa <- function(df, id1, id2) {
  # Extract temperature and kappa for the given index range
  return(list(temp = df[, 2][id1:id2], kappa = df[, 3][id1:id2]))
}

# Read the data from CSV file
df <- read.table("Wasser/wasser.csv", sep=";", header=TRUE)

# Parse data for Deionized water (rows 1 to 3)
df1 <- parseTempKappa(df, 1, 3)
temp.deionized <- df1$temp
kappa.deionized <- df1$kappa

# Parse data for Tap water (rows 4 to 6)
df2 <- parseTempKappa(df, 4, 6)
temp.tap <- df2$temp
kappa.tap <- df2$kappa

# Parse data for Conductivity water (rows 7 to 9)
df3 <- parseTempKappa(df, 7, 9)
temp.conduct <- df3$temp
kappa.conduct <- df3$kappa

# Calculate summary statistics and format them
summary_stats <- data.frame(
  Water_Type = c("Deionized", "Tap", "Conductivity"),
  Kappa = c(format_mean_ci(mean_ci = calculate_mean_ci(kappa.deionized), r = 3),
            format_mean_ci(mean_ci = calculate_mean_ci(kappa.tap), r = 3),
            format_mean_ci(mean_ci = calculate_mean_ci(kappa.conduct), r = 3)),
  Temperature = c(format_mean_ci(mean_ci = calculate_mean_ci(temp.deionized), r = 3),
                  format_mean_ci(mean_ci = calculate_mean_ci(temp.tap), r = 3),
                  format_mean_ci(mean_ci = calculate_mean_ci(temp.conduct), r = 3))
)

# Print the summary statistics
print(summary_stats)
