# tabula rasa
rm(list=ls())

# import custom functions for data analysis
source("functions.R")

parseTempKappa <- function(df, id1, id2) {
  # Function is not abstract enough to be reused, so it stays in water.R
  # Function parses list of temp and conductivity for the given idx range.
  return (list(temp = df[, 2][id1:id2], kappa = df[, 3][id1:id2]))
}

meanCIMolarConduct <- function(kappa, conc) {
  # This functions calculates the confidence intervall and mean of the molar conducitivty
  # For deviation of the error propagation, see section on error propagation in 
  # laboratory report.
  mean.Lambda <- mean(kappa)
  sd.kappa <- sd(kappa)
  se.kappa <- sd.kappa / length(kappa)
  
  CI.Lambda <- se.kappa / conc
  
  return (list(mean=mean.Lambda, ci=CI.Lambda))
}

# read data
df <- read.table("Molare_Leitfähigkeit/molar.csv", sep=";", header=TRUE)

df1 <- parseTempKappa(df, 1, 3)
temp.K <- df1$temp
kappa.K <- df1$kappa

df2 <- parseTempKappa(df, 4, 6)
temp.N <- df2$temp
kappa.N <- df2$kappa

print(temp.N)
# obtain concentrations
c.K <- 0.1002
n.N <- 0.53 / 100.58988
c.N <- (0.53 / 100.59888) / 0.05

# Calculate the mean and confidence interval for temperature and kappa for Potassium (K)
kappa.K <- meanCIMolarConduct(kappa.K, c.K)
temp.K <- calculate_mean_ci(temp.K)

# Calculate the mean and confidence interval for temperature and kappa for Nitrogen (N)
temp.N <- calculate_mean_ci(temp.N)
kappa.N <- meanCIMolarConduct(kappa.N, c.N)

# Combine the results into a summary data frame
summary_stats <- data.frame(
  Element = c("K", "N"),
  Temperature = c(format_mean_ci(temp.K, 5), format_mean_ci(temp.N, 5)),
  Kappa = c(format_mean_ci(kappa.K, 5), format_mean_ci(kappa.N, 5)),
  Concentration = c(c.K, c.N)
)

# Print the summary statistics
print(summary_stats)

