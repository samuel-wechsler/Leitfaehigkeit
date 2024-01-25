# Clean workspace
rm(list=ls())

# Read data
df <- read.table("Temperatur/temperatur.csv", sep=";", header=TRUE)
temp <- df[, 1]
kappa <- df[, 2]

# Perform linear regression analysis
fit <- lm(kappa ~ temp)

# Set up plotting parameters
par(mar = c(5, 5, 2, 2))

# Plot
plot(temp, kappa, type = "n",
     xlab = expression(theta * " / " * degree * "C"),  # x-axis label
     ylab = expression(kappa * " / " * mS~cm^-1),     # y-axis label
     cex.axis = 1.3,  # Adjust the size of tick labels
     cex.lab = 1.5    # Adjust the size of axis labels
)

# Add line of best fit and data points
abline(fit, lwd = 3, col = "black")
points(temp, kappa, pch = 21, cex = 1.0, col = "black", bg = "white")  # Add pretty data points

# Add lines to highlight conductivity at 40 degrees
kappa40 <- kappa[which(temp == 40)]
segments(x0 = 40, y0 = 0, x1 = 40, y1 = kappa40, lwd = 1.5, lty = 2)
segments(x0 = 0, y0 = kappa40, x1 = 40, y1 = kappa40, lwd = 1.5, lty = 2)

# Evaluate the linear model
summary <- summary(fit)
freedom_deg <- df.residual(fit)
conf_intervals <- confint(fit)
slope <- coef(fit)["temp"]
slope.sd <- summary$coefficients["temp", "Std. Error"]

# Extrapolate conductivity at normal temperature, get confidence interval,
# as well as the standard deviation for error propagation of alpha

# Obtain standard deviation
kappa.T0 <- predict(fit, newdata = data.frame(temp = 25), se.fit = TRUE)
standard_errors <- kappa.T0$se.fit
mean_squared_se <- mean(standard_errors^2)
kappa.T0.sd <- sqrt(mean_squared_se)

# Obtain confidence interval
kappa.T0 <- predict(fit, newdata = data.frame(temp = 25), interval = "confidence", level = 0.95)
kappa.T0.ci <- diff(kappa.T0[1, c(2, 3)]) / 2
kappa.T0 <- kappa.T0[1]

# Obtain confidence interval for linear temperature coefficient of conductivity alpha
alpha <- slope / (kappa.T0)
s_alpha <- sqrt((1 / kappa.T0)**2 * slope.sd**2 + (slope / kappa.T0) ** 2 * kappa.T0.sd **2)
ci_alpha <- s_alpha * qt(0.975, freedom_deg)

# Obtain R-squared value
Rsquared <- paste("\n\nR-squared:", round(summary$r.squared, 10))

# Display results
cat("\nConfidence intervals for predicted values at 25 degrees:\n")
cat(paste("Predicted kappa: ", kappa.T0, " ± ", kappa.T0.ci, "\n"))
cat(Rsquared)

# Calculate alpha
cat("\n\nConfidence interval for alpha:\n")
cat(paste("Alpha: ", alpha, " ± ", ci_alpha, "\n"))
