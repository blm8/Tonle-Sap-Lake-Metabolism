

##===============================  MOX for Tonle Sap Lake  ===============================##


##  Author(s):  B. Miller

##  Required dataframes:  Master_MOX-Models.csv


##=======================================  Contents  ========================================##

##  1.  

##=======================================  Contents  ========================================##


#rm(list=ls())

#install.packages("readr")
library(readr) 
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr)
#install.packages("mgcv")
library(mgcv)
#install.packages("AICcmodavg")
library(AICcmodavg)
#install.packages("rLakeAnalyzer")
library(rLakeAnalyzer) # Produced by GLEON
#install.packages("lubridate")
library(lubridate) # Includes functions for time
#install.packages("LakeMetabolizer")
library(LakeMetabolizer) # Produced by GLEON
#install.packages("plotrix")
library(plotrix)

setwd("~/Desktop/Working")

data <- read_csv("Master_MOX-Models.csv")
names(data)
View(data)
length(data$D13C_CH4)

wilcox.test(data$MEAN_FRAC_1, data$MEAN_FRAC_2, paired=FALSE) # No significant difference between the Happel et al. model and Tyler et al. model

PrKo <- data[data$LOCATION == "Northwest", ] 
AnRe <- data[data$LOCATION == "Central", ] 
KaPr <- data[data$LOCATION == "Southwest", ] 


###################################################
##  Kampong Preah High-water Oxidation Profiles  ##
###################################################

Isotopes <- KaPr %>% 
  dplyr::select(SITE, STAGE, TRANSECT_POINT, ENVIRON, ACTUAL_Z, MEAN_D13_CH4, SE_D13C_CH4, O2) # Change site
Isotopes <- Isotopes[complete.cases(Isotopes), ]
length(Isotopes$MEAN_D13_CH4)
head(Isotopes)
#View(Isotopes)
range(Isotopes$MEAN_D13_CH4) #-171.7527   -9.3260 
range(Isotopes$ACTUAL_Z) # 0.1 5.0
range(Isotopes$O2) # 0.0 6.4

High <- subset(Isotopes, STAGE=="High")
#View(High)
High1 <- subset(High, TRANSECT_POINT=="1")
High2 <- subset(High, TRANSECT_POINT=="2")
High3 <- subset(High, TRANSECT_POINT=="3")
High4 <- subset(High, TRANSECT_POINT=="4")
High5 <- subset(High, TRANSECT_POINT=="5")
High6 <- subset(High, TRANSECT_POINT=="6")

# T1
par(mfrow=c(1, 6)) # Rows, columns
par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High1$MEAN_D13_CH4, High1$ACTUAL_Z, xlim=c(-77, -9), ylim=rev(c(0.1, 5.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.4), lwd=2.25)
arrows(High1$MEAN_D13_CH4 - High1$SE_D13C_CH4, High1$ACTUAL_Z, High1$MEAN_D13_CH4 + High1$SE_D13C_CH4, High1$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High1$ACTUAL_Z~High1$MEAN_D13_CH4, labels=c(expression("+12" ~ "%"), expression("0" ~ "%"), expression("-100" ~ "%"^a), expression("+100" ~ "%"^a), ""), data=High1, cex=1.6, font=2, pos=c(1, 1, 1, 4), offset=c(1, 1, 1, 1), col=c("darkorchid", "black", "black", "darkorchid", "black"))
#points(median(-77:-9), min(High1$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High1$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-77, -9), at=c(-76, -10), labels=c("-76", "-10"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
mtext(expression(paste(delta^13, C-CH[4], "(\u2030)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T1"), side=3, line=1, cex=1.8, font=1, col="black")
mtext(expression("Open-water"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High2$O2, High2$ACTUAL_Z, xlim=c(-0.1, 6.6), ylim=rev(c(0, 5.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
     labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 6.6), at=c(0.0, 6.5), labels=c("0.0", "6.5"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")

# T2
#par(mfrow=c(2, 6)) # Rows, columns
#par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High2$MEAN_D13_CH4, High2$ACTUAL_Z, xlim=c(-77, -9), ylim=rev(c(0.1, 5.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.4), lwd=2.25)
arrows(High2$MEAN_D13_CH4 - High2$SE_D13C_CH4, High2$ACTUAL_Z, High2$MEAN_D13_CH4 + High2$SE_D13C_CH4, High2$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High2$ACTUAL_Z~High2$MEAN_D13_CH4, labels=c(expression("0" ~ "%"), expression("+3" ~ "%"), expression("+11" ~ "%"), expression("-70" ~ "%"), ""), data=High2, cex=1.6, font=2, pos=c(1, 1, 1, 1), offset=c(1, 1, 1, 1), col=c("black", "darkorchid", "darkorchid", "black", "black"))
#points(median(-77:-9), min(High2$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High2$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-77, -9), at=c(-76, -10), labels=c("-76", "-10"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression(paste(delta^13, C-CH[4], "(\u2030 VPDB)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T2"), side=3, line=1, cex=1.8, font=1, col="black")
mtext(expression("Edge"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High2$O2, High2$ACTUAL_Z, xlim=c(-0.1, 6.6), ylim=rev(c(0, 5.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
#axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
     #labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
#mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 6.6), at=c(0.0, 6.5), labels=c("0.0", "6.5"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")

# T3
#par(mfrow=c(2, 6)) # Rows, columns
#par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High3$MEAN_D13_CH4, High3$ACTUAL_Z, xlim=c(-77, -9), ylim=rev(c(0.1, 5.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.4), lwd=2.25)
arrows(High3$MEAN_D13_CH4 - High3$SE_D13C_CH4, High3$ACTUAL_Z, High3$MEAN_D13_CH4 + High3$SE_D13C_CH4, High3$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High3$ACTUAL_Z~High3$MEAN_D13_CH4, labels=c(expression("-2" ~ "%"), expression("+30" ~ "%"), ""), data=High3, cex=1.6, font=2, pos=c(1, 1, 1), offset=c(1, 1, 1), col=c("black", "darkorchid", "black"))
#points(median(-77:-9), min(High3$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High3$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-77, -9), at=c(-76, -10), labels=c("-76", "-10"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression(paste(delta^13, C-CH[4], "(\u2030 VPDB)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T3"), side=3, line=1, cex=1.8, font=1, col="black")
mtext(expression("Floodplain"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High3$O2, High3$ACTUAL_Z, xlim=c(-0.1, 6.6), ylim=rev(c(0, 5.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
#axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
#labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
#mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 6.6), at=c(0.0, 6.5), labels=c("0.0", "6.5"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")

arrows(2.1, 4.95, 0.1, 4.95, lty=1, lwd=1.6, length=0.1, xpd=TRUE, col="darkorchid")
text(4.1, 4.95, labels=expression("Methanogenesis"), cex=1.8, font=1, xpd=TRUE, col="darkorchid")

# T4
#par(mfrow=c(2, 6)) # Rows, columns
#par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High4$MEAN_D13_CH4, High4$ACTUAL_Z, xlim=c(-77, -9), ylim=rev(c(0.1, 5.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.4), lwd=2.25)
arrows(High4$MEAN_D13_CH4 - High4$SE_D13C_CH4, High4$ACTUAL_Z, High4$MEAN_D13_CH4 + High4$SE_D13C_CH4, High4$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High4$ACTUAL_Z~High4$MEAN_D13_CH4, labels=c(expression("+16" ~ "%"), expression("-100" ~ "%"^a), ""), data=High4, cex=1.6, font=2, pos=c(1, 1, 1), offset=c(1, 1, 1), col=c("darkorchid", "black", "black"))
#points(median(-77:-9), min(High4$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High4$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-77, -9), at=c(-76, -10), labels=c("-76", "-10"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression(paste(delta^13, C-CH[4], "(\u2030 VPDB)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T4"), side=3, line=1, cex=1.8, font=1, col="black")
#mtext(expression("Floodplain"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High4$O2, High4$ACTUAL_Z, xlim=c(-0.1, 6.6), ylim=rev(c(0, 5.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
#axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
#labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
#mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 6.6), at=c(0.0, 6.5), labels=c("0.0", "6.5"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")

arrows(4.6, 4.95, 6.5, 4.95, lty=1, lwd=1.8, length=0.1, xpd=TRUE, col="black")
text(2.6, 4.95, labels=expression("Methanotrophy"), cex=1.8, font=1, xpd=TRUE, col="black")

# T5
#par(mfrow=c(2, 6)) # Rows, columns
#par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High5$MEAN_D13_CH4, High5$ACTUAL_Z, xlim=c(-77, -9), ylim=rev(c(0.1, 5.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.8), lwd=1.6)
arrows(High5$MEAN_D13_CH4 - High5$SE_D13C_CH4, High5$ACTUAL_Z, High5$MEAN_D13_CH4 + High5$SE_D13C_CH4, High5$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High5$ACTUAL_Z~High5$MEAN_D13_CH4, labels=c(expression("+19" ~ "%"), expression("-29" ~ "%"), expression("-100" ~ "%"^a), ""), data=High5, cex=1.6, font=2, pos=c(2, 2, 2), offset=c(0.5, 0.5, 0.5), col=c("darkorchid", "black", "black"))
#points(median(-77:-9), min(High5$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High5$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-77, -9), at=c(-76, -10), labels=c("-76", "-10"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression(paste(delta^13, C-CH[4], "(\u2030 VPDB)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T5"), side=3, line=1, cex=1.8, font=1, col="black")
#mtext(expression("Floodplain"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High5$O2, High5$ACTUAL_Z, xlim=c(-0.1, 6.6), ylim=rev(c(0, 5.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
#axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
#labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
#mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 6.6), at=c(0.0, 6.5), labels=c("0.0", "6.5"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")

# T6
#par(mfrow=c(2, 6)) # Rows, columns
#par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High6$MEAN_D13_CH4, High6$ACTUAL_Z, xlim=c(-77, -9), ylim=rev(c(0.1, 5.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.4), lwd=2.25)
arrows(High6$MEAN_D13_CH4 - High6$SE_D13C_CH4, High6$ACTUAL_Z, High6$MEAN_D13_CH4 + High6$SE_D13C_CH4, High6$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High6$ACTUAL_Z~High6$MEAN_D13_CH4, labels=c(expression("-4" ~ "%"), expression("-3" ~ "%"), ""), data=High6, cex=1.6, font=2, pos=c(4, 4), offset=c(0.5, 0.5), col=c("black", "black", "black"))
#points(median(-77:-9), min(High6$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High6$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-77, -9), at=c(-76, -10), labels=c("-76", "-10"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression(paste(delta^13, C-CH[4], "(\u2030 VPDB)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T6"), side=3, line=1, cex=1.8, font=1, col="black")
#mtext(expression("Floodplain"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High6$O2, High6$ACTUAL_Z, xlim=c(-0.1, 6.6), ylim=rev(c(0, 5.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
#axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
#labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
#mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 6.6), at=c(0.0, 6.5), labels=c("0.0", "6.5"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")
#legend("center", inset=0, legend="Incubation", text.col="black", text.font=1,  pch=8, col="black", bg="white", pt.cex=2, cex=1.8, bty="n")


##################################################
##  Anlang Reang High-water Oxidation Profiles  ##
##################################################

Isotopes <- AnRe %>% 
  dplyr::select(SITE, STAGE, TRANSECT_POINT, ENVIRON, ACTUAL_Z, MEAN_D13_CH4, SE_D13C_CH4, O2) # Change site
Isotopes <- Isotopes[complete.cases(Isotopes), ]
head(Isotopes)
length(Isotopes$MEAN_D13_CH4)
#View(Isotopes)
range(Isotopes$MEAN_D13_CH4) # -99.07067 -10.09500 
range(Isotopes$ACTUAL_Z) # 0.1 4.5
range(Isotopes$O2) # 0.00 3.68

High <- subset(Isotopes, STAGE=="High")
#View(High)
High1 <- subset(High, TRANSECT_POINT=="1")
High2 <- subset(High, TRANSECT_POINT=="2")
High3 <- subset(High, TRANSECT_POINT=="3")
High4 <- subset(High, TRANSECT_POINT=="4")
High5 <- subset(High, TRANSECT_POINT=="5")
High6 <- subset(High, TRANSECT_POINT=="6")

# T1
par(mfrow=c(1, 6)) # Rows, columns
par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High1$MEAN_D13_CH4, High1$ACTUAL_Z, xlim=c(-100, -9), ylim=rev(c(0.1, 5.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.4), lwd=2.25)
arrows(High1$MEAN_D13_CH4 - High1$SE_D13C_CH4, High1$ACTUAL_Z, High1$MEAN_D13_CH4 + High1$SE_D13C_CH4, High1$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High1$ACTUAL_Z~High1$MEAN_D13_CH4, labels=c(expression("-1" ~ "%"), expression("-73" ~ "%"), expression("-23" ~ "%"^a), expression("+100" ~ "%"^a), ""), data=High1, cex=1.6, font=2, pos=c(1, 1, 1, 4), offset=c(1, 1, 1, 1), col=c("black", "black", "black", "darkorchid", "black"))
#points(median(-77:-9), min(High1$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High1$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-100, -9), at=c(-99, -10), labels=c("-99", "-10"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
mtext(expression(paste(delta^13, C-CH[4], "(\u2030)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T1"), side=3, line=1, cex=1.8, font=1, col="black")
mtext(expression("Open-water"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High2$O2, High2$ACTUAL_Z, xlim=c(-0.1, 6.6), ylim=rev(c(0, 5.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
     labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 6.6), at=c(0.0, 6.5), labels=c("0.0", "6.5"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")

# T2
#par(mfrow=c(2, 6)) # Rows, columns
#par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High2$MEAN_D13_CH4, High2$ACTUAL_Z, xlim=c(-99, -9), ylim=rev(c(0.1, 5.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.4), lwd=2.25)
arrows(High2$MEAN_D13_CH4 - High2$SE_D13C_CH4, High2$ACTUAL_Z, High2$MEAN_D13_CH4 + High2$SE_D13C_CH4, High2$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High2$ACTUAL_Z~High2$MEAN_D13_CH4, labels=c(expression("+34" ~ "%"), expression("-100" ~ "%"^a), expression("+100" ~ "%"^a), expression("-100" ~ "%"^a), ""), data=High2, cex=1.6, font=2, pos=c(1, 1, 1, 1), offset=c(1, 1, 1, 1), col=c("darkorchid", "black", "darkorchid", "black"))
#points(median(-77:-9), min(High2$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High2$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-77, -9), at=c(-76, -10), labels=c("-76", "-10"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression(paste(delta^13, C-CH[4], "(\u2030 VPDB)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T2"), side=3, line=1, cex=1.8, font=1, col="black")
mtext(expression("Edge"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High2$O2, High2$ACTUAL_Z, xlim=c(-0.1, 6.6), ylim=rev(c(0, 5.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
#axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
#labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
#mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 6.6), at=c(0.0, 6.5), labels=c("0.0", "6.5"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")

# T3
#par(mfrow=c(2, 6)) # Rows, columns
#par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High3$MEAN_D13_CH4, High3$ACTUAL_Z, xlim=c(-99, -9), ylim=rev(c(0.1, 5.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.4), lwd=2.25)
arrows(High3$MEAN_D13_CH4 - High3$SE_D13C_CH4, High3$ACTUAL_Z, High3$MEAN_D13_CH4 + High3$SE_D13C_CH4, High3$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High3$ACTUAL_Z~High3$MEAN_D13_CH4, labels=c(expression("-5" ~ "%"), expression("-4" ~ "%"), expression("-7" ~ "%"), ""), data=High3, cex=1.6, font=2, pos=c(1, 1, 1), offset=c(1, 1, 1), col=c("black", "black", "black"))
#points(median(-77:-9), min(High3$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High3$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-77, -9), at=c(-76, -10), labels=c("-76", "-10"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression(paste(delta^13, C-CH[4], "(\u2030 VPDB)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T3"), side=3, line=1, cex=1.8, font=1, col="black")
mtext(expression("Floodplain"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High3$O2, High3$ACTUAL_Z, xlim=c(-0.1, 6.6), ylim=rev(c(0, 5.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
#axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
#labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
#mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 6.6), at=c(0.0, 6.5), labels=c("0.0", "6.5"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")

arrows(2.1, 4.95, 0.1, 4.95, lty=1, lwd=1.6, length=0.1, xpd=TRUE, col="darkorchid")
text(4.1, 4.95, labels=expression("Methanogenesis"), cex=1.8, font=1, xpd=TRUE, col="darkorchid")

# T4
#par(mfrow=c(2, 6)) # Rows, columns
#par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High4$MEAN_D13_CH4, High4$ACTUAL_Z, xlim=c(-99, -9), ylim=rev(c(0.1, 5.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.4), lwd=2.25)
arrows(High4$MEAN_D13_CH4 - High4$SE_D13C_CH4, High4$ACTUAL_Z, High4$MEAN_D13_CH4 + High4$SE_D13C_CH4, High4$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High4$ACTUAL_Z~High4$MEAN_D13_CH4, labels=c(expression("-49" ~ "%"), expression("-100" ~ "%"^a), expression("-30" ~ "%"), expression("-7" ~ "%"), ""), data=High4, cex=1.6, font=2, pos=c(1, 1, 1), offset=c(1, 1, 1), col=c("black", "black", "black", "black"))
#points(median(-77:-9), min(High4$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High4$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-77, -9), at=c(-76, -10), labels=c("-76", "-10"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression(paste(delta^13, C-CH[4], "(\u2030 VPDB)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T4"), side=3, line=1, cex=1.8, font=1, col="black")
#mtext(expression("Floodplain"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High4$O2, High4$ACTUAL_Z, xlim=c(-0.1, 6.6), ylim=rev(c(0, 5.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
#axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
#labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
#mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 6.6), at=c(0.0, 6.5), labels=c("0.0", "6.5"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")

arrows(4.6, 4.95, 6.5, 4.95, lty=1, lwd=1.8, length=0.1, xpd=TRUE, col="black")
text(2.6, 4.95, labels=expression("Methanotrophy"), cex=1.8, font=1, xpd=TRUE, col="black")

# T5
#par(mfrow=c(2, 6)) # Rows, columns
#par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High5$MEAN_D13_CH4, High5$ACTUAL_Z, xlim=c(-99, -9), ylim=rev(c(0.1, 5.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.8), lwd=1.6)
arrows(High5$MEAN_D13_CH4 - High5$SE_D13C_CH4, High5$ACTUAL_Z, High5$MEAN_D13_CH4 + High5$SE_D13C_CH4, High5$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High5$ACTUAL_Z~High5$MEAN_D13_CH4, labels=c(expression("+16" ~ "%"), expression("-33" ~ "%"), expression("-86" ~ "%"), expression("-7" ~ "%"), ""), data=High5, cex=1.6, font=2, pos=c(2, 2, 2), offset=c(0.5, 0.5, 0.5), col=c("darkorchid", "black", "black", "black"))
#points(median(-77:-9), min(High5$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High5$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-77, -9), at=c(-76, -10), labels=c("-76", "-10"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression(paste(delta^13, C-CH[4], "(\u2030 VPDB)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T5"), side=3, line=1, cex=1.8, font=1, col="black")
#mtext(expression("Floodplain"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High5$O2, High5$ACTUAL_Z, xlim=c(-0.1, 6.6), ylim=rev(c(0, 5.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
#axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
#labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
#mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 6.6), at=c(0.0, 6.5), labels=c("0.0", "6.5"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")

# T6
#par(mfrow=c(2, 6)) # Rows, columns
#par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High6$MEAN_D13_CH4, High6$ACTUAL_Z, xlim=c(-99, -9), ylim=rev(c(0.1, 5.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.4), lwd=2.25)
arrows(High6$MEAN_D13_CH4 - High6$SE_D13C_CH4, High6$ACTUAL_Z, High6$MEAN_D13_CH4 + High6$SE_D13C_CH4, High6$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High6$ACTUAL_Z~High6$MEAN_D13_CH4, labels=c(expression("-23" ~ "%"), expression("+20" ~ "%"), expression("-19" ~ "%"), ""), data=High6, cex=1.6, font=2, pos=c(4, 4), offset=c(0.5, 0.5), col=c("black", "darkorchid", "black"))
#points(median(-77:-9), min(High6$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High6$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-77, -9), at=c(-76, -10), labels=c("-76", "-10"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression(paste(delta^13, C-CH[4], "(\u2030 VPDB)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T6"), side=3, line=1, cex=1.8, font=1, col="black")
#mtext(expression("Floodplain"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High6$O2, High6$ACTUAL_Z, xlim=c(-0.1, 6.6), ylim=rev(c(0, 5.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
#axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
#labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
#mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 6.6), at=c(0.0, 6.5), labels=c("0.0", "6.5"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")
#legend("center", inset=0, legend="Incubation", text.col="black", text.font=1,  pch=8, col="black", bg="white", pt.cex=2, cex=1.8, bty="n")


##################################################
##  Prek Konteil High-water Oxidation Profiles  ##
##################################################

Isotopes <- PrKo %>% 
  dplyr::select(SITE, STAGE, TRANSECT_POINT, ENVIRON, ACTUAL_Z, MEAN_D13_CH4, SE_D13C_CH4, O2) # Change site
Isotopes <- Isotopes[complete.cases(Isotopes), ]
head(Isotopes)
length(Isotopes$MEAN_D13_CH4)
#View(Isotopes)
range(Isotopes$MEAN_D13_CH4) # -50.72100 -14.03067
range(Isotopes$ACTUAL_Z) # 0.1 3.5
range(Isotopes$O2) # 0.27 1.04

High <- subset(Isotopes, STAGE=="High")
#View(High)
High1 <- subset(High, TRANSECT_POINT=="1")
High2 <- subset(High, TRANSECT_POINT=="2")
High3 <- subset(High, TRANSECT_POINT=="3")
High4 <- subset(High, TRANSECT_POINT=="4")
High5 <- subset(High, TRANSECT_POINT=="5")
High6 <- subset(High, TRANSECT_POINT=="6")

# T1
par(mfrow=c(1, 6)) # Rows, columns
par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High1$MEAN_D13_CH4, High1$ACTUAL_Z, xlim=c(-51, -13), ylim=rev(c(0.1, 4.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.4), lwd=2.25)
arrows(High1$MEAN_D13_CH4 - High1$SE_D13C_CH4, High1$ACTUAL_Z, High1$MEAN_D13_CH4 + High1$SE_D13C_CH4, High1$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High1$ACTUAL_Z~High1$MEAN_D13_CH4, labels=c(expression("-3" ~ "%"), expression("-5" ~ "%"), expression("+15" ~ "%"), expression("0" ~ "%"), ""), data=High1, cex=1.6, font=2, pos=c(1, 1, 1, 4), offset=c(1, 1, 1, 1), col=c("black", "black", "black", "darkorchid", "black"))
#points(median(-77:-9), min(High1$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High1$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-51, -13), at=c(-50, -14), labels=c("-50", "-14"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
mtext(expression(paste(delta^13, C-CH[4], "(\u2030)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T1"), side=3, line=1, cex=1.8, font=1, col="black")
mtext(expression("Open-water"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High2$O2, High2$ACTUAL_Z, xlim=c(-0.1, 1.1), ylim=rev(c(0, 4.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
axis(2, ylim=c(-0.1, 4.1), at=rev(c(0, 1, 2, 3, 4)), 
     labels=rev(c("0", "1", "2", "3", "4")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 1.1), at=c(0.0, 1.0), labels=c("0.0", "1.0"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")

# T2
#par(mfrow=c(2, 6)) # Rows, columns
#par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High2$MEAN_D13_CH4, High2$ACTUAL_Z, xlim=c(-51, -13), ylim=rev(c(0.1, 4.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.4), lwd=2.25)
arrows(High2$MEAN_D13_CH4 - High2$SE_D13C_CH4, High2$ACTUAL_Z, High2$MEAN_D13_CH4 + High2$SE_D13C_CH4, High2$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High2$ACTUAL_Z~High2$MEAN_D13_CH4, labels=c(expression("-4" ~ "%"), expression("+1" ~ "%"), expression("-3" ~ "%"), expression("0" ~ "%"), ""), data=High2, cex=1.6, font=2, pos=c(4, 4, 4, 4), offset=c(1, 1, 1, 1), col=c("black", "darkorchid", "black", "black"))
#points(median(-77:-9), min(High2$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High2$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-51, -13), at=c(-50, -14), labels=c("-50", "-14"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression(paste(delta^13, C-CH[4], "(\u2030 VPDB)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T2"), side=3, line=1, cex=1.8, font=1, col="black")
mtext(expression("Edge"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High2$O2, High2$ACTUAL_Z, xlim=c(-0.1, 1.1), ylim=rev(c(0, 4.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
#axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
#labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
#mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 1.1), at=c(0.0, 1.0), labels=c("0.0", "1.0"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")

# T3
#par(mfrow=c(2, 6)) # Rows, columns
#par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High3$MEAN_D13_CH4, High3$ACTUAL_Z, xlim=c(-51, -13), ylim=rev(c(0.1, 4.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.4), lwd=2.25)
arrows(High3$MEAN_D13_CH4 - High3$SE_D13C_CH4, High3$ACTUAL_Z, High3$MEAN_D13_CH4 + High3$SE_D13C_CH4, High3$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High3$ACTUAL_Z~High3$MEAN_D13_CH4, labels=c(expression("+5" ~ "%"), expression("-38" ~ "%"), expression("-12" ~ "%"), expression("-100" ~ "%"^a), ""), data=High3, cex=1.6, font=2, pos=c(1, 1, 1), offset=c(1, 1, 1), col=c("darkorchid", "black", "black", "black"))
#points(median(-77:-9), min(High3$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High3$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-51, -13), at=c(-50, -14), labels=c("-50", "-14"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression(paste(delta^13, C-CH[4], "(\u2030 VPDB)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T3"), side=3, line=1, cex=1.8, font=1, col="black")
mtext(expression("Floodplain"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High3$O2, High3$ACTUAL_Z, xlim=c(-0.1, 1.1), ylim=rev(c(0, 4.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
#axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
#labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
#mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 1.1), at=c(0.0, 1.0), labels=c("0.0", "1.0"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")

arrows(2.1, 4.95, 0.1, 4.95, lty=1, lwd=1.6, length=0.1, xpd=TRUE, col="darkorchid")
text(4.1, 4.95, labels=expression("Methanogenesis"), cex=1.8, font=1, xpd=TRUE, col="darkorchid")

# T4
#par(mfrow=c(2, 6)) # Rows, columns
#par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High4$MEAN_D13_CH4, High4$ACTUAL_Z, xlim=c(-51, -13), ylim=rev(c(0.1, 4.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.4), lwd=2.25)
arrows(High4$MEAN_D13_CH4 - High4$SE_D13C_CH4, High4$ACTUAL_Z, High4$MEAN_D13_CH4 + High4$SE_D13C_CH4, High4$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High4$ACTUAL_Z~High4$MEAN_D13_CH4, labels=c(expression("-4" ~ "%"), expression("-10" ~ "%"), expression("-6" ~ "%"), ""), data=High4, cex=1.6, font=2, pos=c(1, 1, 1), offset=c(1, 1, 1), col=c("black", "black", "black"))
#points(median(-77:-9), min(High4$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High4$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-51, -13), at=c(-50, -14), labels=c("-50", "-14"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression(paste(delta^13, C-CH[4], "(\u2030 VPDB)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T4"), side=3, line=1, cex=1.8, font=1, col="black")
#mtext(expression("Floodplain"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High4$O2, High4$ACTUAL_Z, xlim=c(-0.1, 1.1), ylim=rev(c(0, 4.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
#axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
#labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
#mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 1.1), at=c(0.0, 1.0), labels=c("0.0", "1.0"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")

arrows(4.6, 4.95, 6.5, 4.95, lty=1, lwd=1.8, length=0.1, xpd=TRUE, col="black")
text(2.6, 4.95, labels=expression("Methanotrophy"), cex=1.8, font=1, xpd=TRUE, col="black")

# T5
#par(mfrow=c(2, 6)) # Rows, columns
#par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High5$MEAN_D13_CH4, High5$ACTUAL_Z, xlim=c(-51, -13), ylim=rev(c(0.1, 4.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.8), lwd=1.6)
arrows(High5$MEAN_D13_CH4 - High5$SE_D13C_CH4, High5$ACTUAL_Z, High5$MEAN_D13_CH4 + High5$SE_D13C_CH4, High5$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High5$ACTUAL_Z~High5$MEAN_D13_CH4, labels=c(expression("+4" ~ "%"), expression("0" ~ "%"), expression("0" ~ "%"), expression("-2" ~ "%"), ""), data=High5, cex=1.6, font=2, pos=c(2, 2, 2), offset=c(0.5, 0.5, 0.5), col=c("darkorchid", "black", "black", "black"))
#points(median(-77:-9), min(High5$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High5$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-51, -13), at=c(-50, -14), labels=c("-50", "-14"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression(paste(delta^13, C-CH[4], "(\u2030 VPDB)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T5"), side=3, line=1, cex=1.8, font=1, col="black")
#mtext(expression("Floodplain"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High5$O2, High5$ACTUAL_Z, xlim=c(-0.1, 1.1), ylim=rev(c(0, 4.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
#axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
#labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
#mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 1.1), at=c(0.0, 1.0), labels=c("0.0", "1.0"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")

# T6
#par(mfrow=c(2, 6)) # Rows, columns
#par(oma=c(12, 6, 0, 0))
par(mar=c(0.5, 2, 7, 2)) # Bottom, left, top, right
par(xpd=FALSE)
plot(High6$MEAN_D13_CH4, High6$ACTUAL_Z, xlim=c(-51, -13), ylim=rev(c(0.1, 4.1)), type="b", xaxt="n", yaxt="n", xlab="", ylab="", pch=21, 
     bty="n", cex=3, col="blue", bg=adjustcolor("blue", alpha.f=0.4), lwd=2.25)
arrows(High6$MEAN_D13_CH4 - High6$SE_D13C_CH4, High6$ACTUAL_Z, High6$MEAN_D13_CH4 + High6$SE_D13C_CH4, High6$ACTUAL_Z, length=0.05, angle=90, code=3, col="blue", lwd=1.6)
text(High6$ACTUAL_Z~High6$MEAN_D13_CH4, labels=c(expression("-25" ~ "%"), expression("-6" ~ "%"), expression("+3" ~ "%"), ""), data=High6, cex=1.6, font=2, pos=c(4, 4), offset=c(0.5, 0.5), col=c("black", "black", "darkorchid"))
#points(median(-77:-9), min(High6$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
#points(median(-77:-9), max(High6$ACTUAL_Z), type="p", pch=8, col="black", cex=2)
axis(1, xlim=c(-51, -13), at=c(-50, -14), labels=c("-50", "-14"), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression(paste(delta^13, C-CH[4], "(\u2030 VPDB)", sep="")), side=1, line=4.5, cex=1.8, font=1, col="black")
mtext(expression("T6"), side=3, line=1, cex=1.8, font=1, col="black")
#mtext(expression("Floodplain"), side=3, line=3.5, cex=1.8, font=1, col="black")

par(new=TRUE)
plot(High6$O2, High6$ACTUAL_Z, xlim=c(-0.1, 1.1), ylim=rev(c(0, 4.1)), type="b", pch=21, xaxt="n", yaxt="n", xlab="", ylab="", 
     bty="n", cex=2, col="gray40", bg=adjustcolor("gray40", alpha.f=0.4), lwd=2.25)
box(lty=1, lwd=1, col="black")
#axis(2, ylim=c(-0.1, 5.1), at=rev(c(0, 1, 2, 3, 4, 5)), 
#labels=rev(c("0", "1", "2", "3", "4", "5")), col="black", las=1, cex=2, cex.axis=2, cex.lab=2, font=1) 
#mtext(expression("Depth" ~ (m)), side=2, line=4.5, cex=1.8, font=1)
axis(1, xlim=c(-0.1, 1.1), at=c(0.0, 1.0), labels=c("0.0", "1.0"), col="gray40", col.axis="gray40", line=6.5, las=1, cex=2, cex.axis=2, cex.lab=2, font=1)
#mtext(expression("Dissolved" ~ O[2] ~ (mg ~ L^{-1})), side=1, line=11, cex=1.8, font=1, col="gray40")
#legend("center", inset=0, legend="Incubation", text.col="black", text.font=1,  pch=8, col="black", bg="white", pt.cex=2, cex=1.8, bty="n")


#######################
##  Oxidation Rates  ##
#######################

Profile <- PrKo %>% 
  dplyr::select(SITE, ACTUAL_Z, STAGE, DATE, MEAN_FRAC_1) %>% 
  group_by(SITE, ACTUAL_Z, DATE) # Group data by these columns
names(Profile)
View(Profile)

