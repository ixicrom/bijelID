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
data.head()

x=data.drop(columns=['target'])
x.head()
y=data['target'].values
y[0:5]

knn=nn.KNeighborsClassifier()
param_grid={'n_neighbors': np.arange(1,69)}
knn_gscv = cv.GridSearchCV(knn, param_grid, cv=10)
knn_gscv.fit(x,y)
print knn_gscv.best_params_
print knn_gscv.best_score_
