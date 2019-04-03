library(caret)

dat=iris
head(dat)

train_control <- trainControl(method="cv", number=10)

knn_fit <- train(Species ~., data=dat, method="knn", trControl=train_control, tuneLength=34)
print(knn_fit)
plot(knn_fit)


print(paste0("Accuracy: ",knn_fit$results[row.names(knn_fit$bestTune),]$Accuracy, " (k=", knn_fit$bestTune[[1]], ")"))
