import numpy as np
import matplotlib.pyplot as pl
import pandas as pd
from sklearn.datasets import load_iris
import sklearn.neighbors as nn
import sklearn.model_selection as cv
from sklearn.metrics import accuracy_score

iris = load_iris()
data = pd.DataFrame(iris.data, columns=iris.feature_names)
data['target']=pd.Series(iris.target)
len(data['target'])


x=data.drop(columns=['target'])
y=data['target'].values


knn=nn.KNeighborsClassifier()
param_grid={'n_neighbors': np.arange(1,69)}
knn_gscv = cv.GridSearchCV(knn, param_grid, cv=10)
knn_gscv.fit(x,y)
plotx=knn_gscv.cv_results_['param_n_neighbors']
plotx=np.ma.filled(plotx)
ploty=knn_gscv.cv_results_['mean_test_score']
pl.plot(plotx, ploty)

print knn_gscv.best_params_
print knn_gscv.best_score_

x1=data['sepal width (cm)'].values
x1[0:5]
len(x1)
len(y)

knn1=nn.KNeighborsClassifier()
knn1_gscv = cv.GridSearchCV(knn1, param_grid, cv=10)
knn1_gscv.fit(x1.reshape(-1,1),y)
print(knn1_gscv.best_params_)
print(knn1_gscv.best_score_)

# trainInd = [18,93,  91,  92, 126, 149, 2,  34,  95,  73,  98,  76,  40, 127, 138, 114,  39,  36,  25, 31,  42, 136,  21,   6,  28, 102,  66, 113, 125, 137,  55,  32, 133,  60,  22,  88,  23,  30, 112,  90,  61,  71, 143,  67,  35,  53, 109,  50, 132,  78,   8, 131, 104,  49,  15,  48,  47,  70,  17, 101, 148,   4, 146, 144, 128, 110,  26,  43,   5,  46,  10, 140,  87,  85,   7, 134,  29, 121,  24, 142,  65,  33,  80,  37,  13,  59, 122,  20,  68, 120,  62,  54, 100,  58, 141,  74, 147, 111, 145,  38]
#
# trainInd = trainInd-np.repeat(1,len(trainInd))
# xArr=np.array(x)
# xArr[18]
# xTrain = xArr[trainInd]
# xTrain[0:5]
#
# xTest[0:5]
# yTrain=y[trainInd]
#
# knnplain=nn.KNeighborsClassifier()
# knnplain.fit(xTrain,yTrain)
# plainTest = knnplain.predict()
