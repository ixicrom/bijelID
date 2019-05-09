
library(pastecs)
library(caret)

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

ggplot(exp_Data, aes(x=as.factor(Bijel), y=Particle.Gradients.20, fill=Bijel)) + geom_boxplot(alpha=0.3) + geom_jitter(alpha=0.5) + xlab("Bijel?") + ylab("Gradient") + ggtitle("Gradient of first 20 points of particle ACF")+theme(plot.title = element_text(hjust = 0.5))


y2 <- exp_Data$Autocorrelation.Particle[1:10]
lineFits2 <- lapply(1:135, function(n) lm(unlist(y2[n,]) ~ r[1:10]))
lineCoeffs2 <- lapply(lineFits2, function(m) m$coefficients)
lineGradients2 <- lapply (1:135, function(p) unname(lineCoeffs2[[p]][2]))
exp_Data$Particle.Gradients.10 <- unlist(lineGradients2)

ggplot(exp_Data, aes(x=as.factor(Bijel), y=Particle.Gradients.10, fill=Bijel)) + geom_boxplot(alpha=0.3) + geom_jitter(alpha=0.5) + xlab("Bijel?") + ylab("Gradient") + ggtitle("Gradient of first 10 points of particle ACF")+theme(plot.title = element_text(hjust = 0.5))


#turning point of liquid channel ACF
liquidTurns <- lapply(1:135, function(y) turnpoints(unlist(exp_Data$Autocorrelation.Liquid[y,])))
firstTurn <- lapply(1:135, function(y) liquidTurns[[y]]$tppos[1])
exp_Data$Liquid.First.Turn <- unlist(firstTurn)

ggplot(exp_Data, aes(x=as.factor(Bijel), y=Liquid.First.Turn, fill=Bijel)) + geom_boxplot(alpha=0.3) + geom_jitter(alpha=0.5) + xlab("Bijel?") + ylab("Position") + ggtitle("Position of first turning points of liquid ACF (pixels)")


attach(exp_Data)
dat=data.frame(Particle.Gradients.20, Particle.Gradients.10, Liquid.First.Turn, Bijel)
set.seed(1234)
trCtrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)


set.seed(1234)
logRegFitParticle <- train(Bijel ~ Particle.Gradients.10 + Particle.Gradients.20, data=dat, method="glm", trControl=trCtrl)
print("Logistic regression particle channel:")
print(logRegFitParticle)

set.seed(1234)
knnFitLiquid <- train(Bijel ~ Liquid.First.Turn, data=dat, method="knn", trControl=trCtrl, tuneLength=30)
print("KNN liquid channel:")
print(knnFitLiquid)

set.seed(1234)
logRegFitBoth <- train(Bijel~., data=dat, method="glm", trControl=trCtrl)
print("Logistic regression both channels:")
print(logRegFitBoth)

set.seed(1234)
knnFitBoth <- train(Bijel~., data=dat, method="knn", trControl=trCtrl, tuneLength=30)
print("KNN both channels:")
print(knnFitBoth)

set.seed(1234)
dTreeFitBoth <- train(Bijel~., data=dat, method="rpart", trControl=trCtrl)
print("Decision tree both channels:")
print(dTreeFitBoth)

set.seed(1234)
svmFitBoth <- train(Bijel~., data=dat, method="svmLinear", trControl=trCtrl)
print("SVM both channels:")
print(svmFitBoth)
