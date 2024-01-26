# Conductivity as a function of concentration

# Clear the workspace
rm(list=ls())

# Load data from the CSV file
Data <- read.csv("Konzentration/konzentration.csv", header=TRUE, sep=";")
vol <- Data[,1]
Vol <- vol[-1]
vol <- Vol * 0.001  # Convert volume to liters
Ltf <- Data$Leitfähigkeit

# Extract initial values
idx <- 1
klsm <- Ltf[1]

# Uncomment the lines below to reproduce the analysis for a hypothetical value of klsm=10
# klsm <- 10
# idx <- 5

# Calculate the difference between subsequent conductivity values and subtract klsm
kappa <- Ltf[-1]
kappa1 <- kappa - klsm

# Constants related to the solution
MNa <- 105.99  # Molecular mass of Na2Co3
mNa <- 0.5300  # Mass of Na2CO3 measured
nNa <- mNa / MNa  # Amount of Na2CO3
c0 <- nNa / 0.5  # Initial concentration

# Calculate concentration, square root of concentration, and conductivities
c <- c0 * vol / (0.1 + vol)
wuc <- sqrt(c)
Lambdaa <- (kappa1 / 1000) / c
Lambdaaa <- (kappa / 1000) / c

# Function to generate pretty log scales
PrettyLog <- function(begin=1e-10, end=1e10) {
  d <- 10^c(-99:99)
  d <- d[d >= begin & d <= end]
  dd <- outer(c(1:9), 10^c(-99:99))
  dd <- dd[dd >= begin & dd <= end]
  dlab <- do.call("expression",
                  lapply(seq(along=log10(d)), function(i) substitute(10^E, list(E=log10(d)[i]))))
  list("big"=d, "small"=dd, "big.lab"=dlab)
}

# Plot conductivity vs. concentration
par(mfrow =c(1,2))
par(mar = c(5, 5, 2, 2))
plot(c, kappa1, log="xy",
     type="n",
     xlim=c(1e-5,2.5e-3), ylim=c(1,500),
     xaxs="i", yaxs="i",
     xaxt="n", yaxt="n",
     xlab=expression(italic(c)*"Na"[2]*"CO"[3]*" / "*M),
     ylab=expression(italic(kappa)*" / "*mu*S~cm^-1),
     cex.axis = 1.3,
     cex.lab = 1.5
)
PL <- PrettyLog()
axis(1, at=PL$small, labels=FALSE, tcl=-0.25)
axis(1, at=PL$big, labels=PL$big.lab)
axis(2, at=PL$small, labels=FALSE, tcl=-0.25)
axis(2, las=1)
x <- 10^seq(-4.5,-2.9,length=100)
y <- 100000 * x
lines(x, y, lty="dashed")
points(c, kappa, pch=21, bg="white")
points(c, kappa1, pch=21, bg="grey")

# Plot square root of concentration vs. conductivity
plot(wuc, Lambdaa, type = "n",
     ylim=c(100,350),
     xlim=c(0,0.05),
     xaxs="i",yaxs="i",
     xlab=expression(sqrt(c)*"/M"^(1/2)),
     ylab=expression(Lambda*"/S cm"^2~"mol"^-1),
     cex.axis = 1.3,
     cex.lab = 1.5
)

points(wuc, Lambdaaa, pch=21, bg="white")
points(wuc[1:idx], Lambdaa[1:idx], pch=23, bg="grey")
points(wuc[idx:length(Lambdaa)], Lambdaa[idx:length(Lambdaa)], pch=21, bg="grey", )

# Add labels to the plots
mtext("A", side =3, cex=2, line =-2, at=0, outer = TRUE, adj = 0)
mtext("B", side=3, cex=2, line=-2, at=0.5, outer=TRUE, adj=0)

# Perform linear regression and plot the regression line
Lambda0 <- lm(Lambdaa[idx:length(Lambdaa)] ~ wuc[idx:length(wuc)])
summary(Lambda0)
a <- summary(Lambda0)$coef[1]
b <- summary(Lambda0)$coef[2]
abline(a, b)

# Calculate and print the confidence intervals for the coefficients
conf_interval <- confint(Lambda0)
conf_interval_intercept <- conf_interval["(Intercept)", ]
CI <- diff(conf_interval_intercept) / 2
print(a)
print(CI)
