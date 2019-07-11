library(pastecs)
library(caret)


corrFiles <- list.files("/Volumes/PhD/BijelData/Batch_II_full_resolution_ED_images/Autocorrelation", pattern=".txt", full.names = TRUE)
corrFileNames <- list.files("/Volumes/PhD/BijelData/Batch_II_full_resolution_ED_images/Autocorrelation", pattern=".txt")

autoCorr <- do.call(cbind, lapply(corrFiles, read.csv, header=FALSE))
colnames(autoCorr) <- corrFileNames

exp_Data <- read.csv("/Volumes/PhD/BijelData/Bijel_Data_Cleaner_ToRead.csv", na.strings = "?")
exp_Data$Sample.Number <- as.character(exp_Data$Sample.Number)

corrFileID <- sapply(strsplit(corrFileNames,"_"), `[`,1) #`[` is a function that takes the subset of x, the input to this function is x (strsplit...) and the element of x that I want, ie the 1st one
colnames(autoCorr) <- corrFileID

rownames(exp_Data) <- exp_Data$Sample.Number
autoCorr_transpose <- data.frame(t(autoCorr))
exp_Data$Autocorrelation <- autoCorr_transpose[match(row.names(exp_Data),row.names(autoCorr_transpose)),c(1:256)]


turningPoints <- lapply(1:135, function(y) turnpoints(unlist(exp_Data$Autocorrelation[y,])))
exp_Data$Auto.Turning.Points <- turningPoints


peakDepth <- lapply(1:135, function(y) exp_Data$Autocorrelation[[y,exp_Data$Auto.Turning.Points[[y]]$tppos[[2]]]]-exp_Data$Autocorrelation[[y,exp_Data$Auto.Turning.Points[[y]]$tppos[[1]]]])
exp_Data$Auto.Peak.Depth <- unlist(peakDepth)


exp_Data$Auto.Num.Turns <- unlist(lapply(1:135, function(y) length(exp_Data$Auto.Turning.Points[[y]]$tppos)))


radFiles <- list.files("/Volumes/PhD/BijelData/Batch_II_full_resolution_ED_images/Profiles", pattern=".txt", full.names = TRUE)
radFileNames <- list.files("/Volumes/PhD/BijelData/Batch_II_full_resolution_ED_images/Profiles", pattern=".txt")

radProf_data <- do.call(cbind, lapply(radFiles, read.csv, header=FALSE))
colnames(radProf_data) <- radFileNames

radFileID <- sapply(strsplit(radFileNames,"_"), `[`,1) #`[` is a function that takes the subset of x, the input to this function is x (strsplit...) and the element of x that I want, ie the 1st one
colnames(radProf_data) <- radFileID

rownames(exp_Data) <- exp_Data$Sample.Number
radProf_data_transpose <- data.frame(t(radProf_data))
exp_Data$Radial.Profile <- radProf_data_transpose[match(row.names(exp_Data),row.names(radProf_data_transpose)),c(1:256)]



r <- c(1:256)
y <- exp_Data$Radial.Profile[2:20]
lineFits <- lapply(1:135, function(n) lm(unlist(y[n,]) ~ r[2:20]))
lineCoeffs <- lapply(lineFits, function(m) m$coefficients)
lineGradients <- lapply (1:135, function(p) unname(lineCoeffs[[p]][2]))
exp_Data$Rad.Line.Gradients <- unlist(lineGradients)


avVal <- lapply(1:135, function(n) mean(unlist(exp_Data$Radial.Profile[n,29:31])))
exp_Data$Rad.Val.30 <- unlist(avVal)


jumps <- function(y,n){
  a <- exp_Data$Radial.Profile[[y,n+5]]
  b <- exp_Data$Radial.Profile[[y,n]]
  out <- a-b
  return(out)
}

#nMax <- lapply(1:135, function(y) length(exp_Data$Radial.Profile[[y]])-1)
jumpSizes <- lapply(1:135, function(y) mapply(jumps, y, 1:251))
maxJump <- lapply(1:135, function(y) max(jumpSizes[[y]]))
exp_Data$Max.Rad.Jump <- unlist(maxJump)


maxJumpX <- lapply(1:135, function(y) which.max(jumpSizes[[y]]))
exp_Data$X.Max.Rad.Jump <- unlist(maxJumpX)


firstTurn <- lapply(1:135, function(y) exp_Data$Auto.Turning.Points[[y]]$tppos[[1]])
exp_Data$Auto.First.Turn <- unlist(firstTurn)


attach(exp_Data)
dat=data.frame(Auto.Num.Turns, Auto.Peak.Depth, Rad.Val.30, Rad.Line.Gradients, Max.Rad.Jump, X.Max.Rad.Jump, Auto.First.Turn)

means = unlist(lapply(1:7, function(n) mean(unlist(dat[n][]))))

datScaled = data.frame(lapply(1:7, function(n) dat[n][]*means[1]/means[n]))
datScaled$Bijel = Bijel
head(datScaled)

set.seed(1234)
knn_fit_scaled <- train(Bijel ~., data=datScaled, method="knn", trControl=trCtrl, tuneLength=42)
error=1-knn_fit_scaled$results[row.names(knn_fit_scaled$bestTune),]$Accuracy
error



vars <- names(datScaled)
var_fstat <- sapply(vars, function(x) {
  summary(lm(as.formula(paste(x, " ~ Bijel")), data = dat))$fstatistic[1]})
sort(unlist(var_fstat), decreasing=TRUE)


errors=list(0,0,0,0,0,0)
set.seed(1234)
knn_fit6 <- train(Bijel ~., data=datScaled, method="knn", trControl=trCtrl, tuneLength=42)
errors[1]=1-knn_fit6$results[row.names(knn_fit6$bestTune),]$Accuracy
#plot(knn_fit_turn)

set.seed(1234)
knn_fit5 <- train(Bijel ~., data=datScaled[c("Bijel", "Auto.First.Turn", "Rad.Line.Gradients", "X.Max.Rad.Jump", "Rad.Val.30", "Max.Rad.Jump")], method="knn", trControl=trCtrl, tuneLength=42)
errors[2]=1-knn_fit5$results[row.names(knn_fit5$bestTune),]$Accuracy

set.seed(1234)
knn_fit4 <- train(Bijel ~., data=datScaled[c("Bijel", "Auto.First.Turn", "Rad.Line.Gradients", "X.Max.Rad.Jump", "Rad.Val.30")], method="knn", trControl=trCtrl, tuneLength=42)
errors[3]=1-knn_fit4$results[row.names(knn_fit4$bestTune),]$Accuracy

set.seed(1234)
knn_fit3 <- train(Bijel ~., data=datScaled[c("Bijel", "Auto.First.Turn", "Rad.Line.Gradients", "X.Max.Rad.Jump")], method="knn", trControl=trCtrl, tuneLength=42)
errors[4]=1-knn_fit3$results[row.names(knn_fit3$bestTune),]$Accuracy

set.seed(1234)
knn_fit2 <- train(Bijel ~., data=datScaled[c("Bijel", "Auto.First.Turn", "Rad.Line.Gradients")], method="knn", trControl=trCtrl, tuneLength=42)
errors[5]=1-knn_fit2$results[row.names(knn_fit2$bestTune),]$Accuracy

set.seed(1234)
knn_fit1 <- train(Bijel ~., data=datScaled[c("Bijel", "Auto.First.Turn")], method="knn", trControl=trCtrl, tuneLength=42)
errors[6]=1-knn_fit1$results[row.names(knn_fit1$bestTune),]$Accuracy

errors

plot(c(6,5,4,3,2,1), errors, xlab="Number of variables in model", ylab="Classification error", pch=19, ylim=c(.16, .22))
