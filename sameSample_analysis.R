library(pastecs)
library(caret)

# _____________________Final model fit___________________________________________________
exp_Data <- read.csv("/Volumes/PhD/BijelData/Bijel_Data_Cleaner_ToRead.csv", na.strings = "?")
exp_Data$Sample.Number <- as.character(exp_Data$Sample.Number)
rownames(exp_Data) <- exp_Data$Sample.Number

partFiles <- list.files("/Volumes/PhD/BijelData/ParticleChannel/autoCorr", pattern=".txt", full.names = TRUE)
partFileNames <- list.files("/Volumes/PhD/BijelData/ParticleChannel/autoCorr", pattern=".txt")

autoCorrPart <- do.call(cbind, lapply(partFiles, read.csv, header=FALSE))
#colnames(autoCorrPart) <- partFileNames

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

#gradients of particle channel ACF
r <- c(1:256)
y <- exp_Data$Autocorrelation.Particle[1:20]
lineFits <- lapply(1:135, function(n) lm(unlist(y[n,]) ~ r[1:20]))
lineCoeffs <- lapply(lineFits, function(m) m$coefficients)
lineGradients <- lapply (1:135, function(p) unname(lineCoeffs[[p]][2]))
exp_Data$Particle.Gradients.20 <- unlist(lineGradients)

y2 <- exp_Data$Autocorrelation.Particle[1:10]
lineFits2 <- lapply(1:135, function(n) lm(unlist(y2[n,]) ~ r[1:10]))
lineCoeffs2 <- lapply(lineFits2, function(m) m$coefficients)
lineGradients2 <- lapply (1:135, function(p) unname(lineCoeffs2[[p]][2]))
exp_Data$Particle.Gradients.10 <- unlist(lineGradients2)

#turning point of liquid channel ACF
liquidTurns <- lapply(1:135, function(y) turnpoints(unlist(exp_Data$Autocorrelation.Liquid[y,])))
firstTurn <- lapply(1:135, function(y) liquidTurns[[y]]$tppos[1])
exp_Data$Liquid.First.Turn <- unlist(firstTurn)


attach(exp_Data)
fitDat=data.frame(Particle.Gradients.20, Particle.Gradients.10, Liquid.First.Turn, Bijel)

set.seed(1234)
trCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(1234)
finalFit <- train(Bijel~., data=fitDat, method="glm", trControl=trCtrl)




# _____________________Testing the model on same sample data__________________________________________


bijelFilesL <- list.files("/Volumes/PhD/BijelData/sample52ii/liquid", pattern=".txt", full.names = TRUE)
bijelFileNames <- list.files("/Volumes/PhD/BijelData/sample52ii/liquid", pattern=".txt")
bijelFilesP <- list.files("/Volumes/PhD/BijelData/sample52ii/particle", pattern=".txt", full.names = TRUE)


failFilesL <- list.files("/Volumes/PhD/BijelData/sample54i/liquid", pattern=".txt", full.names = TRUE)
failFileNames <- list.files("/Volumes/PhD/BijelData/sample54i/liquid", pattern=".txt")
failFilesP <- list.files("/Volumes/PhD/BijelData/sample54i/particle", pattern=".txt", full.names = TRUE)

autoCorrBL <- do.call(cbind, lapply(bijelFilesL, read.csv, header=FALSE))
autoCorrBP <- do.call(cbind, lapply(bijelFilesP, read.csv, header=FALSE))
bijelID <- sapply(strsplit(bijelFileNames,"_"), `[`,1)
#colnames(autoCorrBL) <- colnames(autoCorrBP) <- bijelID


bijelLabs <- rep_len("y", length(bijelFileNames))
bDat <- data.frame(Bijel=bijelLabs)
rownames(bDat) <- bijelID
bDat$Liq <- data.frame(t(autoCorrBL))
bDat$Par <- data.frame(t(autoCorrBP))


autoCorrNBL <- do.call(cbind, lapply(failFilesL, read.csv, header=FALSE))
autoCorrNBP <- do.call(cbind, lapply(failFilesP, read.csv, header=FALSE))
failID <- sapply(strsplit(failFileNames,"_"), `[`,1)
#colnames(autoCorrNBL) <- colnames(autoCorrNBP) <- failID

failLabs <- rep_len("n", length(failFileNames))
nbDat <- data.frame(Bijel=failLabs)
#rownames(nbDat) <- failID
nbDat$Liq <- data.frame(t(autoCorrNBL))
nbDat$Par <- data.frame(t(autoCorrNBP))

# testDat <- merge(bDat, nbDat)
# 
# y <- testDat$Par[,1:20]
# lineFits <- lapply(1:19, function(n) lm(unlist(y[n,]) ~ r[1:20]))
# lineCoeffs <- lapply(lineFits, function(m) m$coefficients)
# lineGradients <- lapply (1:19, function(p) unname(lineCoeffs[[p]][2]))
# testDat$Particle.Gradients.20 <- unlist(lineGradients)
# 
# y2 <- testDat$Par[,1:10]
# lineFits <- lapply(1:19, function(n) lm(unlist(y[n,]) ~ r[1:10]))
# lineCoeffs <- lapply(lineFits, function(m) m$coefficients)
# lineGradients <- lapply (1:19, function(p) unname(lineCoeffs[[p]][2]))
# testDat$Particle.Gradients.10 <- unlist(lineGradients)
# 
# lTurns <- lapply(1:19, function(y) turnpoints(unlist(testDat$Liq[y,])))
# firstTurn <- lapply(1:19, function(y) lTurns[[y]]$tppos[1])
# testDat$Liquid.First.Turn <- unlist(firstTurn)
# 
# attach(testDat)
# testing <- data.frame(Particle.Gradients.10, Particle.Gradients.20, Liquid.First.Turn)
# 
# predictA <- predict(finalFit)
# predictB <- extractPrediction(list(finalFit), testX=testing, testY=testDat$Bijel)
# 
# predict(finalFit, newdata = testDat)

yB <- bDat$Par[1:20]
lineFitsB <- lapply(1:8, function(n) lm(unlist(yB[n,]) ~ r[1:20]))
lineCoeffsB <- lapply(lineFitsB, function(m) m$coefficients)
lineGradientsB <- lapply(1:8, function(p) unname(lineCoeffsB[[p]][2]))
bDat$Particle.Gradients.20 <- unlist(lineGradientsB)

yB2 <- bDat$Par[1:10]
lineFitsB2 <- lapply(1:8, function(n) lm(unlist(yB2[n,]) ~ r[1:10]))
lineCoeffsB2 <- lapply(lineFitsB2, function(m) m$coefficients)
lineGradientsB2 <- lapply(1:8, function(p) unname(lineCoeffsB2[[p]][2]))
bDat$Particle.Gradients.10 <- unlist(lineGradientsB2)

lTurnsB <- lapply(1:8, function(n) turnpoints(unlist(bDat$Liq[n,])))
firstTurnB <- lapply(1:8, function(m) lTurnsB[[m]]$tppos[1])
bDat$Liquid.First.Turn <- unlist(firstTurnB)


yNB <- nbDat$Par[1:20]
lineFitsNB <- lapply(1:11, function(n) lm(unlist(yNB[n,]) ~ r[1:20]))
lineCoeffsNB <- lapply(lineFitsNB, function(m) m$coefficients)
lineGradientsNB <- lapply(1:11, function(p) unname(lineCoeffsNB[[p]][2]))
nbDat$Particle.Gradients.20 <- unlist(lineGradientsNB)

yNB2 <- nbDat$Par[1:10]
lineFitsNB2 <- lapply(1:11, function(n) lm(unlist(yNB2[n,]) ~ r[1:10]))
lineCoeffsNB2 <- lapply(lineFitsNB2, function(m) m$coefficients)
lineGradientsNB2 <- lapply(1:11, function(p) unname(lineCoeffsNB2[[p]][2]))
nbDat$Particle.Gradients.10 <- unlist(lineGradientsNB2)

lTurnsNB <- lapply(1:11, function(n) turnpoints(unlist(nbDat$Liq[n,])))
firstTurnNB <- lapply(1:11, function(m) lTurnsNB[[m]]$tppos[1])
nbDat$Liquid.First.Turn <- unlist(firstTurnNB)

attach(bDat)
testDat1 <- data.frame(Particle.Gradients.10, Particle.Gradients.20, Liquid.First.Turn)

attach(nbDat)
testDat2 <- data.frame(Particle.Gradients.10, Particle.Gradients.20, Liquid.First.Turn)

testDat <- rbind(testDat1, testDat2)
predict(finalFit, newdata = testDat)

