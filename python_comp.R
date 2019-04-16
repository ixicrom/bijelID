library(caret)

dat=iris
head(dat)

train_control <- trainControl(method="cv", number=10)
set.seed(1234)
knn_fit <- train(Species ~., data=dat, method="knn", trControl=train_control, tuneLength=34)
print(knn_fit)
plot(knn_fit)

print(paste0("Accuracy: ",knn_fit$results[row.names(knn_fit$bestTune),]$Accuracy, " (k=", knn_fit$bestTune[[1]], ")"))


dat1=data.frame(Sepal.Width=dat$Sepal.Width, Species=dat$Species)
head(dat1)
set.seed(1234)
knn_fit1 <- train(Species ~., data=dat1, method="knn", trControl=train_control, tuneLength=34)
print(knn_fit1)
plot(knn_fit1)

print(paste0("Accuracy: ",knn_fit1$results[row.names(knn_fit1$bestTune),]$Accuracy, " (k=", knn_fit1$bestTune[[1]], ")"))

smp_size <- floor(0.67 * nrow(dat))
set.seed(1234)
train_ind <- sample(seq_len(nrow(dat)), size=smp_size)

dat_train <- dat[train_ind,]
head(dat_train)
dat_test <- dat[-train_ind,]

knn_plain <- train(Species ~., data=dat, method="knn")
print(paste0("Accuracy: ",knn_plain$results[row.names(knn_plain$bestTune),]$Accuracy, " (k=", knn_plain$bestTune[[1]], ")"))

