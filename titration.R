# Delete memory
rm(list=ls())

#----------------
# some functions

# returns indices for regions that are chosen for linear regressions
chooseRegion <- function(volume, conduct) {
  idx <- identify(volume, conduct, n=4)
  
  # Check if any points are identified
  if (length(idx) < 4) {
    stop("Not enough points identified for regression. Please choose regions manually.")
  }
  
  return (list(region1=c(idx[1], idx[2]), region2 = c(idx[3], idx[4])))
}

plotTitration <- function() {
  
  # add measurements as points
  points(volume, conductivity, pch=23, cex=1.0, col="black", bg="white")
  
  # draw measurement points in the evaluated regions with circle symbol
  points(volume[c(region1, region2)], conductivity[c(region1, region2)],
         pch=21, cex=1.2, col="black", bg="gray")
  
  # draw model curves and their confidence intervals
  matlines(Vtit1.c, kappa1.c, lty =c(1,1,1), lwd=c(3,1,1), col="black")
  matlines(Vtit2.c, kappa2.c, lty =c(1,1,1), lwd=c(3,1,1), col="black")
  
  # add measurements as points
  points(volume, conductivity, pch=23, cex=1.0, col="black", bg="white")
  
  # draw measurement points in the evaluated regions with circle symbol
  points(volume[c(region1, region2)], conductivity[c(region1, region2)],
         pch=21, cex=1.2, col="black", bg="gray")
  
  # fill the area between upper and lower bounds with gray color and increased transparency
  polygon(c(Vtit1.c, rev(Vtit1.c)), c(kappa1.c[, 2], rev(kappa1.c[, 3])),
          col = rgb(0.5, 0.5, 0.5, alpha = 0.2), border = NA)
  polygon(c(Vtit2.c, rev(Vtit2.c)), c(kappa2.c[, 2], rev(kappa2.c[, 3])),
          col = rgb(0.5, 0.5, 0.5, alpha = 0.2), border = NA)
  
  
  lines(c(x1, x1), c(0, y1), lwd=1)
  lines(c(x2, x2), c(0, y2), lwd=2)
  lines(c(x3, x3), c(0, y3), lwd=1)
}


getConc <- function(deltaV, c0, V) {
  # calculate according to the following equation: c_a * V_a = c_b * V_b
  # ==> c_a = c_b * V_b / V_a
  (c0 * deltaV) / V
}
#----------------

# Read file
df <- read.table("Titration/titration.csv", header=TRUE, sep=";")

# Parse data
volume <- df[,1]
conductivity <- df[,2]

region1 <- which(volume > 2.0 & volume < 8)
region2 <- which(volume > 10)  #& volume != 18.502)
  
# perform linear regression analysis
line1 <- lm(conductivity~volume, subset=region1)
line2 <- lm(conductivity~volume, subset=region2)

# infer equivalence point from intersection of lines
a1 <- coef(line1)[1]
a2 <- coef(line2)[1]
b1 <- coef(line1)[2]
b2 <- coef(line2)[2]
intersection <- (a1-a2) / (b2-b1)

# Calculate model lines for both regions
Vtit1.c <- seq(0.0,  intersection+1, length=50)
Vtit2.c <- seq(intersection-1, 20.0, length=50)
kappa1.c <- predict(line1, list(volume=Vtit1.c), interval="confidence")
kappa2.c <- predict(line2, list(volume=Vtit2.c), interval="confidence")

# determine uncertainties manually
x1 <- 8.979734
x2 <- 9.169379
x3 <- 9.354993
y1 <- 2.286513
y2 <- 2.30309
y3 <- 2.320706

conc <- getConc(x2 / 1000, 0.09998, 0.1)
conc_ci <- c(getConc(x1 / 1000, 0.09998, 0.1), getConc(x3/1000, 0.09998, 0.1))
ci <- mean(abs(conc_ci - conc))

# prin results
r = 5
cat("================\n")
cat("Results of titration\n")
cat(paste(round(conc, 5), "±", round(ci, 5), "molL^-1\n"))
cat("\n================\n")
cat("Equivalence point at\n")
cat(paste("V=", conc * 1000, "±", ci * 1000))

# Create a new figure with multiple plots
pdf("plots/titration.pdf", width = 16, height = 8)
par(mfrow =c(1,2))
par(mar = c(5, 5, 2, 2))

# make a nicer plot
plot(0,0, type="n",
     xlim=c(0.0, 20.0),
     ylim=c(2.2, 3.9),
     xlab=expression(italic(V)[tit]*" / mL"),
     ylab=expression(italic(kappa)*" / "*mS~cm^-1),
     cex.axis = 1.3,  # Adjust the size of tick labels
     cex.lab = 1.5   # Adjust the size of axis labels
)

plotTitration() # Plot data

# Plot the second region on the right with a zoomed view
plot(0, 0, type="n",
     xlim=c(8.5, 10),
     ylim=c(2.0, 2.5),
     xlab=expression(italic(V)[tit]*" / mL"),
     ylab=expression(italic(kappa)*" / "*mS~cm^-1),
     cex.axis = 1.3,  # Adjust the size of tick labels
     cex.lab = 1.5   # Adjust the size of axis labels
)

plotTitration() # Plot data

mtext("A", side =3, cex=2, line =-2, at=0, outer = TRUE, adj = 0)
mtext("B", side=3, cex=2, line=-2, at=0.5, outer=TRUE, adj=0)

# Close the PDF device
dev.off()
