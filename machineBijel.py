
import os
import numpy as np
import matplotlib.pyplot as pl
import scipy.fftpack as fftim
# from scipy.misc.pilutil import Image
import pandas as pd
import sklearn.neighbors as nn
import sklearn.model_selection as cv
from sklearn.metrics import accuracy_score
from skimage import io
#from Tkinter import Tk
#from tkinter.filedialog import askopenfilename

def radial_profile(data,centre):
    y, x = np.indices((data.shape))
    r = np.sqrt((x-centre[0])**2+(y-centre[1])**2)
    r = r.astype(np.int)
    tbin = np.bincount(r.ravel(),data.ravel())
    nr = np.bincount(r.ravel())
    radialprofile = tbin/nr
    return radialprofile

def correlate(x,y):
    fr = fftim.fft2(x)
    fr2 = fftim.fft2(np.flipud(np.fliplr(y)))
    m,n = fr.shape
    cc = np.real(fftim.ifft2(fr*fr2))
    cc = np.roll(cc, int(-m/2+1),axis=0)
    cc = np.roll(cc, int(-m/2+1),axis=1)
    return cc

def listTif_nohidden(path):
    out=[]
    for f in os.listdir(path):
        if not f.startswith('.'):
            if f.endswith('.tif'):
                out.append(f)
                #yield f
    return out

def turnpoints(lst):
    x=np.array(lst)
    n=len(x)
    x0=x[0]-1.
    x2=np.concatenate(([x0], x[:-1]))
    diffs=x!=x2
    uniques=x[diffs]
    uniques
    n2=len(uniques)
    poss=np.arange(n)[diffs]
    exaequos=np.concatenate((poss[1:n2],[n+1]))-poss-1
    #at some point need to add in if statements to catch when things are wrong as with the R package
    m = n2-2
    vals=np.concatenate((np.arange(m)+2, np.arange(m)+1, np.arange(m)))
    ex = np.array(uniques[vals])
    ex=np.reshape(ex,(-1,m))
    ex=np.transpose(ex)
    peaks=[False]
    pits=[False]
    for i in range(m):
        peaks.append(ex[i,1]==max(ex[i,]))
        pits.append(ex[i,1]==min(ex[i,]))
    peaks.append(False)
    pits.append(False)
    tpts= [a or b for a,b in zip(peaks, pits)]
    if sum(tpts)==0:
        tppos=np.nan
        peaks = [False]*n2
        pits = [False]*n2
    else:
        tppos = (poss+exaequos)[tpts]
    return tppos


#Tk().withdraw()
#datFile = askopenfilename()
datFile=os.path.normpath('/Volumes/PhD/BijelData/Bijel_Data_Cleaner_ToRead.csv')
exp_Dat = pd.read_csv(datFile) #read in experimental data file
exp_Dat.index=exp_Dat['Sample Number'] #rename rows as sample numbers


autocorr_Dat = pd.DataFrame(index=exp_Dat.index, columns=['AutoTurn', 'AutoTurnPart', 'AutoTurnCount'])
#calculate autocorrelation of each image
imageDir='/Volumes/PhD/BijelData/Python/TIFs/'
#workingDir='/Volumes/PhD/BijelData/Test'
images=listTif_nohidden(imageDir)

imChannel=int(input("Which image channel? (Index starts at 0) "))

for image in images:
    a = io.imread(os.path.join(imageDir,image))[imChannel]
    b = np.asarray(a)

    ac3 = correlate(b,b)

    autoCorr = (radial_profile(ac3,(b.shape[0]/2,b.shape[1]/2))[:256]/radial_profile(ac3,(b.shape[0]/2.,b.shape[1]/2.))[:256][0]) #256 should be replaced by half image size, i.e. this is for a 512x512 image

    turns=turnpoints(autoCorr)
    sample=image.split("_")[0]

#autocorr_Dat
exp_Dat=pd.concat([exp_Dat, autocorr_Dat['AutoTurnPart']],axis=1)

print(exp_Dat.head())
x=np.asarray(exp_Dat['AutoTurnPart'])
y=np.asarray(exp_Dat['Bijel'])


# x_train, x_test, y_train, y_test=cv.train_test_split(x,y,test_size=0.33, random_state=42)
# x_train=x_train.reshape(-1,1)
# x_test=x_test.reshape(-1,1)

x=exp_Dat[['AutoTurn','AutoTurnCount']]
x.head()
y=exp_Dat['Bijel'].values

knn=nn.KNeighborsClassifier()
param_grid={'n_neighbors': np.arange(1,69)}
knn_gscv = cv.GridSearchCV(knn, param_grid, cv=10)
knn_gscv.fit(x.reshape(-1,1),y.reshape(-1,1))
print(knn_gscv.best_params_)
print(knn_gscv.best_score_)
