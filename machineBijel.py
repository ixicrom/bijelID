#hi Sam
import os
import numpy as np
import matplotlib.pyplot as pl
import scipy.fftpack as fftim
from scipy.misc.pilutil import Image
import pandas as pd
import sklearn.neighbors as nn
import sklearn.cross_validation as cv

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
    cc = np.roll(cc, -m/2+1,axis=0)
    cc = np.roll(cc, -n/2+1,axis=1)
    return cc

def listTif_nohidden(path):
    for f in os.listdir(path):
        if not f.startswith('.'):
            if f.endswith('.tif'):
                yield f

def turningpoints(lst):
    dx = np.diff(lst)
    return np.sum(dx[1:] * dx[:-1] < 0)

def turnpoints(lst):
    x=np.array(lst)
    n=len(x)
    x0=x[0]-1.
    x2=np.concatenate(([x0], x[:-1]))
    diffs=x!=x2
    uniques=x[diffs]
    #n2=len(uniques)
    #poss=np.arange(1:n)[diffs]
    #exaequos=np.concatenate(poss[1:n2],[n+1])-poss-1




exp_Dat = pd.read_csv('M:\PhD\BijelData\Bijel_Data_Cleaner_ToRead.csv') #read in experimental data file
exp_Dat.index=exp_Dat['Sample Number'] #rename rows as sample numbers
#print(exp_Dat.ix[:5,:5])
exp_Dat.head()

autocorr_Dat = pd.DataFrame(index=exp_Dat.index, columns=['AutoTurn'])

#calculate autocorrelation of each image
imageDir='M:\PhD\BijelData\Batch_II_full_resolution_ED_images\TIFs'
workingDir='M:\PhD\BijelData\Test'
images=listTif_nohidden('M:\PhD\BijelData\Batch_II_full_resolution_ED_images\TIFs')
print(images)
for image in images:
    a = Image.open(os.path.join(imageDir,image))
    b = np.asarray(a)

    ac3 = correlate(b,b)

    autoCorr = (radial_profile(ac3,(b.shape[0]/2,b.shape[1]/2))[:256]/radial_profile(ac3,(b.shape[0]/2.,b.shape[1]/2.))[:256][0]) #256 should be replaced by half image size, i.e. this is for a 512x512 image

    output = autoCorr
    #txtFile = '%s_autoCorr.txt'%image
    #graphFile = '%s_autoCorr.png'%image

    #out = open(os.path.join(workingDir, txtFile), 'wb')
    #for j in range(len(output)):
        #buf = '%s \n' %(str(output[j]))
        #out.write(buf)

    turn=turningpoints(output)

    sample=image.split("_")[0]
    autocorr_Dat['AutoTurn'][sample] = turn


exp_Dat=pd.concat([exp_Dat, autocorr_Dat['AutoTurn']])
print len(autocorr_Dat.index)
print len(exp_Dat.index)

x=np.array(exp_Dat['AutoTurn'])
y=np.array(exp_Dat['Bijel'])


x_train, x_test, y_train, y_test=cv.train_test_split(x,y,test_size=0.33, random_state=42)

knn=nn.KNeighborsClassifier(n_neighbors=19)

knn.fit(x_train, y_train)

pred=knn.predict(x_test)

#print accuracy_score(y_test, pred)
