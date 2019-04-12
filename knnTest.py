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
