

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

set.seed(1234)
logRegFitAlt2 <- train(Bijel~Liquid.First.Turn+I(Particle.Gradients.10^2)+I(Particle.Gradients.20^2), data=dat, method="glm", trControl=trCtrl)
print(logRegFitAlt2)

set.seed(1234)
logRegFitAlt3 <- train(Bijel~Liquid.First.Turn+I(Particle.Gradients.10^3)+I(Particle.Gradients.20^3), data=dat, method="glm", trControl=trCtrl)
print(logRegFitAlt3)

set.seed(1234)
logRegFit2var <- train(Bijel~Liquid.First.Turn+Particle.Gradients.10, data=dat, method="glm", trControl=trCtrl)
print(logRegFit2var)

set.seed(1234)
logRegFit2varB <- train(Bijel~Liquid.First.Turn+Particle.Gradients.20, data=dat, method="glm", trControl=trCtrl)
print(logRegFit2varB)

set.seed(1234)
knnFit2var <- train(Bijel~Liquid.First.Turn+Particle.Gradients.10, data=dat, method="knn", trControl=trCtrl, tuneLength=30)
print(knnFit2var)

set.seed(1234)
knnFit2varB <- train(Bijel~Liquid.First.Turn+Particle.Gradients.20, data=dat, method="knn", trControl=trCtrl, tuneLength=30)
print(knnFit2varB)
