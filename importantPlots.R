library(pastecs)
library(caret)
library(ggplot2)
library(Hmisc)

exp_Data <- read.csv("/Volumes/PhD/BijelData/Bijel_Data_Cleaner_ToRead.csv", na.strings = "?")
exp_Data$Sample.Number <- as.character(exp_Data$Sample.Number)
rownames(exp_Data) <- exp_Data$Sample.Number

partFiles <- list.files("/Volumes/PhD/BijelData/ParticleChannel/autoCorr", pattern=".txt", full.names = TRUE)
partFileNames <- list.files("/Volumes/PhD/BijelData/ParticleChannel/autoCorr", pattern=".txt")

autoCorrPart <- do.call(cbind, lapply(partFiles, read.csv, header=FALSE))

partFileID <- sapply(strsplit(partFileNames,"_"), `[`,1) #`[` is a function that takes the subset of x, the input to this function is x (strsplit...) and the element of x that I want, ie the 1st one
colnames(autoCorrPart) <- partFileID

autoCorrPart_transpose <- data.frame(t(autoCorrPart))
exp_Data$Autocorrelation.Particle <- autoCorrPart_transpose[match(row.names(exp_Data),row.names(autoCorrPart_transpose)),c(1:256)]


liqFiles <- list.files("/Volumes/PhD/BijelData/LiquidChannel/autoCorr", pattern = ".txt", full.names = TRUE)
liqFileNames <- list.files("/Volumes/PhD/BijelData/LiquidChannel/autoCorr", pattern = ".txt")

autoCorrLiq <- do.call(cbind, lapply(liqFiles, read.csv, header=FALSE))
colnames(autoCorrLiq) <- liqFileNames

liqFileID <- sapply(strsplit(liqFileNames,"_"), `[`,1)
colnames(autoCorrLiq) <- liqFileID

autoCorrLiq_transpose <- data.frame(t(autoCorrLiq))
exp_Data$Autocorrelation.Liquid <- autoCorrLiq_transpose[match(row.names(exp_Data),row.names(autoCorrLiq_transpose)),c(1:256)]

#example SF plots (particle channel)
bijelSF_p <- unlist(read.csv("/Volumes/PhD/BijelData/SF_bothChannels/52ii_Image23.tif_radProf_channel1.txt"))
noBijelSF_p <- unlist(read.csv("/Volumes/PhD/BijelData/SF_bothChannels/54i_Image9.tif_radProf_channel1.txt"))
x=c(1:255)
xSF=x*640.17/512
png('~/SFexamples_part.png', res=300, width=1800, height=1200)
plot(xSF, bijelSF_p, type="l", lwd=2, col="red", xlab = "q (1/μm)", ylab="Structure Factor", ylim=c(0.4,2.2),cex.axis=1.5,cex.lab=2,mgp=c(2.2,0.7,0))
lines(xSF, noBijelSF_p, lwd=2)
legend("bottomright", legend = c("Bijel", "Non-bijel"), col=c("red", "black"), lwd=2)#, cex=1.4)
dev.off()

#example SF plots (liquid channel)
bijelSF_l <- unlist(read.csv("/Volumes/PhD/BijelData/SF_bothChannels/52ii_Image23.tif_radProf_channel0.txt"))
noBijelSF_l <- unlist(read.csv("/Volumes/PhD/BijelData/SF_bothChannels/54i_Image9.tif_radProf_channel0.txt"))
png('~/SFexamples_liq.png', res=300, width=1800, height=1200)
plot(xSF, bijelSF_l, type="l", lwd=2, col="red", xlab = "q (1/μm)", ylab="Structure Factor", ylim=c(0.4,2.2),cex.axis=1.5,cex.lab=2,mgp=c(2.2,0.7,0))
lines(xSF, noBijelSF_l, lwd=2)
legend("topright", legend = c("Bijel", "Non-bijel"), col=c("red", "black"), lwd=2)#, cex=1.4)
dev.off()

#example ACF plots (liquid channel)
bijelACF_l <- unlist(read.csv("/Volumes/PhD/BijelData/LiquidChannel/autoCorr/52ii_Image23.tif_autoCorr_channel0.txt"))
noBijelACF_l <- unlist(read.csv("/Volumes/PhD/BijelData/LiquidChannel/autoCorr/54i_Image9.tif_autoCorr_channel0.txt"))
x=c(1:255)
xACF=x*pixel_to_micron
png('~/ACFexamples_liq.png', res=300, width=1800, height=1200)
plot(xACF, bijelACF_l, type="l", lwd=2, col="red", xlab = "r (μm)", ylab="Autocorrelation Function",cex.axis=1.5,cex.lab=2,mgp=c(2.2,0.7,0))
lines(xACF, noBijelACF_l, lwd=2)
legend("topright", legend = c("Bijel", "Non-bijel"), col=c("red", "black"), lwd=2)
dev.off()

#example ACF plots (particle channel)
bijelACF_p <- unlist(read.csv("/Volumes/PhD/BijelData/ParticleChannel/autoCorr/52ii_Image23.tif_autoCorr_channel1.txt"))
noBijelACF_p <- unlist(read.csv("/Volumes/PhD/BijelData/ParticleChannel/autoCorr/54i_Image9.tif_autoCorr_channel1.txt"))
png('~/ACFexamples_part.png', res=300, width=1800, height=1200)
plot(xACF, noBijelACF_p, type="l", lwd=2, col="black", xlab = "r (μm)", ylab="Autocorrelation Function",cex.axis=1.5,cex.lab=2,mgp=c(2.2,0.7,0))
lines(xACF, bijelACF_p, lwd=2, col="red")
legend("topright", legend = c("Bijel", "Non-bijel"), col=c("red", "black"), lwd=2)
dev.off()


#gradients of particle channel ACF
r <- c(1:256)
L <- 640.17
pixel_to_micron <- 2*pi/L
r_scaled <- r*pixel_to_micron
y <- exp_Data$Autocorrelation.Particle[1:20] * pixel_to_micron
lineFits <- lapply(1:135, function(n) lm(unlist(y[n,]) ~ r_scaled[1:20]))
lineCoeffs <- lapply(lineFits, function(m) m$coefficients)
lineGradients <- lapply (1:135, function(p) unname(lineCoeffs[[p]][2]))
exp_Data$Particle.Gradients.20 <- unlist(lineGradients)
exp_Data$Bijel.label <- ifelse(exp_Data$Bijel=="y", "Yes", "No")

#20 point gradient plot
png('~/bj_partGrad20.png', res=300, height=1200, width=1800)
ggplot(exp_Data, aes(x=Bijel.label, y=Particle.Gradients.20, fill=Bijel.label)) + geom_boxplot(alpha=0.3) + geom_jitter(alpha=0.5) + xlab("Bijel?") + ylab("Gradient") + ggtitle("Gradient of first 20 points of particle ACF")+theme(plot.title = element_text(hjust = 0.5), legend.position="none", text = element_text(size=16), axis.title = element_text(size=20))
dev.off()

y2 <- exp_Data$Autocorrelation.Particle[1:10] * pixel_to_micron
lineFits2 <- lapply(1:135, function(n) lm(unlist(y2[n,]) ~ r_scaled[1:10]))
lineCoeffs2 <- lapply(lineFits2, function(m) m$coefficients)
lineGradients2 <- lapply (1:135, function(p) unname(lineCoeffs2[[p]][2]))
exp_Data$Particle.Gradients.10 <- unlist(lineGradients2)

#10 point gradient plot
png('~/bj_partGrad10.png', res=300, height=1200, width=1800)
ggplot(exp_Data, aes(x=Bijel.label, y=Particle.Gradients.10, fill=Bijel.label)) + geom_boxplot(alpha=0.3) + geom_jitter(alpha=0.5) + xlab("Bijel?") + ylab("Gradient") + ggtitle("Gradient of first 10 points of particle ACF")+theme(plot.title = element_text(hjust = 0.5), legend.position="none", text = element_text(size=16), axis.title = element_text(size=20))
dev.off()


#turning point of liquid channel ACF
liquidTurns <- lapply(1:135, function(y) turnpoints(unlist(exp_Data$Autocorrelation.Liquid[y,])))
firstTurn <- lapply(1:135, function(y) liquidTurns[[y]]$tppos[1])
exp_Data$Liquid.First.Turn <- unlist(firstTurn) * pixel_to_micron

png('~/bj_liqTurn.png', res=300, height=1200, width=1800)
ggplot(exp_Data, aes(x=Bijel.label, y=Liquid.First.Turn, fill=Bijel.label)) + geom_boxplot(alpha=0.3) + geom_jitter(alpha=0.5) + xlab("Bijel?") + ylab("Position (μm)") + ggtitle("Position of first turning point of liquid ACF")+theme(plot.title = element_text(hjust = 0.5), legend.position="none", text = element_text(size=16), axis.title = element_text(size=20))
dev.off()


#machine learning stuff
attach(exp_Data)
dat=data.frame(Particle.Gradients.20, Particle.Gradients.10, Liquid.First.Turn, Bijel)
set.seed(1234)
trCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(1234)
knnFitLiquid <- train(Bijel ~ Liquid.First.Turn, data=dat, method="knn", trControl=trCtrl, tuneLength=30)

#liquid cross-val plot
png('~/knn_xVal_liq.png', res=300, height=1200, width=1400)
plot(knnFitLiquid)
dev.off()

set.seed(1234)
y=jitter(rep(0, each=135))

#liquid results plot
png('~/results_liq.png', res=300, height=1200, width=2400)
par(ps=20, mar=c(5,1,1,1), cex=1, cex.axis=0.8)
plot(y~Liquid.First.Turn, col=Bijel, pch=16, yaxt='n', xlab="Position of first turning point in Liquid Channel ACF (μm)", ylab="", log="x")
minor.tick(10,0,0.5)
points(y~Liquid.First.Turn, col=predict(knnFitLiquid), pch=1, cex=1.5)
legend("topright", legend=c("Bijel", "Non-bijel", "Pred bijel", "Pred non-bijel"), pch=c(16,16,1,1), col=c("red", "black", "red", "black"), cex=0.7)
dev.off()


set.seed(1234)
logRegFitParticle <- train(Bijel ~ Particle.Gradients.10 + Particle.Gradients.20, data=dat, method="glm", trControl=trCtrl)

#particle results plot
png('~/results_part.png', res=300, height=1400, width=1400)
par(ps=20, mar=c(5,5,2,1), cex=1, cex.axis=0.8)
plot(Particle.Gradients.10, Particle.Gradients.20, col=Bijel, pch=16, xlab="Particle ACF 10-point gradient", ylab="Particle ACF 20-point gradient")
points(Particle.Gradients.10, Particle.Gradients.20, col=predict(logRegFitParticle), pch=1, cex=1.5)
legend("topleft", legend=c("Bijel", "Non-bijel", "Pred bijel", "Pred non-bijel"), pch=c(16,16,1,1), col=c("red", "black", "red", "black"), cex=0.7)
dev.off()



set.seed(1234)
logRegFitBoth <- train(Bijel~., data=dat, method="glm", trControl=trCtrl)

#final results plot (3D)
# library(plot3D)
# scatter3d(Particle.Gradients.10, Particle.Gradients.20, Liquid.First.Turn, col=Bijel, grid=FALSE, surface=FALSE)
# scatter3d(Particle.Gradients.10, Particle.Gradients.20, Liquid.First.Turn, groups=predict(logRegFitBoth), grid=FALSE, surface=FALSE)
# #future: try sec 4.1 of https://cran.r-project.org/web/packages/plot3D/vignettes/plot3D.pdf
# 
# resultDat <- data.frame(Bijel, liqPred=predict(knnFitLiquid), partPred=predict(logRegFitParticle, type="prob")[[2]], bothPred=predict(logRegFitBoth, type="prob")[[2]])


#final results plots
png('~/results_final.png', res=300, width=4200, height=1400)
par(mfrow=c(1,3), ps=20, mar=c(5,5,2,1), cex=1, cex.axis=0.8)
plot(Particle.Gradients.10, Liquid.First.Turn, col=Bijel, pch=16, xlab="Particle ACF 10-point gradient", ylab="Liquid ACF turning point")
points(Particle.Gradients.10, Liquid.First.Turn, col=predict(logRegFitBoth), pch=1, cex=1.5)
legend("topleft", legend=c("Bijel", "Non-bijel", "Pred bijel", "Pred non-bijel"), pch=c(16,16,1,1), col=c("red", "black", "red", "black"), cex=0.7)

plot(Particle.Gradients.20, Liquid.First.Turn, col=Bijel, pch=16, xlab="Particle ACF 20-point gradient", ylab="Liquid ACF turning point")
points(Particle.Gradients.20, Liquid.First.Turn, col=predict(logRegFitBoth), pch=1, cex=1.5)
legend("topleft", legend=c("Bijel", "Non-bijel", "Pred bijel", "Pred non-bijel"), pch=c(16,16,1,1), col=c("red", "black", "red", "black"), cex=0.7)

plot(Particle.Gradients.10, Particle.Gradients.20, col=Bijel, pch=16, xlab="Particle ACF 10-point gradient", ylab="Particle ACF 20-point gradient")
points(Particle.Gradients.10, Particle.Gradients.20, col=predict(logRegFitBoth), pch=1, cex=1.5)
legend("topleft", legend=c("Bijel", "Non-bijel", "Pred bijel", "Pred non-bijel"), pch=c(16,16,1,1), col=c("red", "black", "red", "black"), cex=0.7)

dev.off()

#ROC plot
# library(plotROC)
# ggplot(resultDat, aes(truth=Bijel, pred=bothPred)) + geom_roc(n.cuts=0)



#random sampling histogram
set.seed(1234)
bijelLabs <- replicate(1000, sample(c("y","n"), 135, replace=TRUE, prob=c(.68, .32)))
bijelLabs[,1]

errors <- vector("list", 1000)
for(i in c(1:1000)){
 dat=data.frame(Liquid.First.Turn, Particle.Gradients.20, Particle.Gradients.10, y=bijelLabs[,i])
 set.seed(1234)
 fit <- train(y ~., data=dat, method="glm", trControl=trCtrl)
 errors[i] <- 1-fit$results$Accuracy
}

errorDF = data.frame(c(1:1000), data.frame(unlist(errors)))
png('~/random_hist.png', res=300, height=1400, width=1600)
ggplot(data=errorDF, aes(errorDF$unlist.errors.)) + geom_histogram()  + scale_y_continuous(expand=c(0,0),limits=c(0,120))+ scale_x_continuous(expand=c(0,0),limits=c(0.1,0.45)) + geom_vline(aes(xintercept=0.1455189, linetype="Model error"), show.legend= TRUE, color="purple") + xlab("Error for randomly labelled samples") + ylab("Frequency") + theme(legend.title = element_blank())
dev.off()



#result tables
table(predict(logRegFitBoth), Bijel)
table(predict(logRegFitParticle), Bijel)
table(predict(knnFitLiquid), Bijel)

