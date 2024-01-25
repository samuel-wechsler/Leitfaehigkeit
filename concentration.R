#Leitfähigkeit in Abhänigkeit der Konzentraiton
rm(list=ls())

Data <- read.csv("Konzentration/konzentration.csv",header=TRUE, sep=";")
vol <- Data[,1]
print(Data)
Vol <- vol[-1]
vol <- (Vol) * 0.001  # vol in L
Ltf <- Data$Leitfähigkeit

klsm <- Ltf[1]
kappa <- Ltf[-1]
kappa1 <- (kappa-klsm) #Abzug von KappaLösungsmittel 

MNa <- 105.99 #Mol Mass of Na2Co3
mNa <- 0.5300 #mass of Na2CO3 weighted
nNa <-  mNa/MNa # amount of Na2CO3
c0 <- nNa/0.5

c <- c0*vol/(0.1+vol)
wuc <- sqrt(c)
Lambdaa <- (kappa1/1000)/c
Lambdaaa <- (kappa/1000)/c


PrettyLog <- function(begin=1e-10, end=1e10) {
  d <- 10^c(-99:99)
  d <- d[d>=begin & d<=end]
  dd <- outer(c(1:9), 10^c(-99:99))
  dd <- dd[dd>=begin & dd<=end]
  dlab <- do.call("expression",
                  lapply(seq(along=log10(d)),function(i) substitute(10^E,list(E=log10(d)[i]))))
  list("big"=d, "small"=dd, "big.lab"=dlab)
}

# pdf("plots/concentration.pdf", width = 16, height = 8)
par(mfrow =c(1,2))
par(mar = c(5, 5, 2, 2))
plot(c, kappa1, log="xy", # doppeltlogarithmische Achsenskalierung
     type="n", # Messdaten nicht einzeichnen
     xlim=c(1e-5,2.5e-3), ylim=c(1,500), # x- und y-Achsenbereiche festlegen
     xaxs="i", yaxs="i", # Achsenbereiche exakt
     xaxt="n", yaxt="n", # keine Achsenskalen
     xlab=expression(italic(c)*"Na"[2]*"CO"[3]*" / "*M), # Beschriftung x-Achse
     ylab=expression(italic(kappa)*" / "*mu*S~cm^-1), # Beschriftung y-Achse
     cex.axis = 1.3,  # Adjust the size of tick labels
     cex.lab = 1.5   # Adjust the size of axis labels
)
PL <- PrettyLog() # f ̈ur logarithmische skalierte Zahlenwerte, vgl. Seite 168
axis(1, at=PL$small, labels=FALSE, tcl=-0.25) # x-Achsenstriche unbeschriftet
axis(1, at=PL$big, labels=PL$big.lab) # x-Achsenstriche beschriftet
axis(2, at=PL$small, labels=FALSE, tcl=-0.25) # y-Achsenstriche unbeschriftet
axis(2, las=1) # y-Achsenstriche unbeschriftet
x <- 10^seq(-4.5,-2.9,length=100) # Berechnung der Geraden
y <- 100000 * x
lines(x, y, lty="dashed")
points(c, kappa, pch=21, bg="white") # unkorrigierte Messdaten
points(c, kappa1, pch=21, bg="grey") # korrigierte Messdaten


plot(wuc,Lambdaa, type = "n",
    ylim=c(0,500),
    xlim=c(0,0.05),
    xaxs="i",yaxs="i",
    xlab =expression(sqrt(c)*"/M"^(1/2)),
    ylab=expression(Lambda*"/S cm"^2~"mol"^-1),
    cex.axis = 1.3,  # Adjust the size of tick labels
    cex.lab = 1.5   # Adjust the size of axis labels
)

points(wuc, Lambdaaa, pch=21, bg="white") # unkorrigierte Messdaten
points(wuc, Lambdaa, pch=21, bg="grey") # korrigierte Messdaten

mtext("A", side =3, cex=2, line =-2, at=0, outer = TRUE, adj = 0)
mtext("B", side=3, cex=2, line=-2, at=0.5, outer=TRUE, adj=0)


Lambda0 <- lm(Lambdaa~wuc)
summary(Lambda0)
a <- summary(Lambda0)$coef[1]
b <- summary(Lambda0)$coef[2]
abline(a,b)

