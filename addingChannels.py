import os
import numpy as np
import matplotlib.pyplot as pl
import scipy.fftpack as fftim
# from scipy.misc.pilutil import Image
import pandas as pd
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
    cc = np.roll(cc, int(-n/2+1),axis=1)
    return cc

def listTif_nohidden(path):
    out=[]
    for f in os.listdir(path):
        if not f.startswith('.'):
            if f.endswith('.tif'):
                out.append(f)
                #yield f
    return out


datFile=os.path.normpath('/Volumes/PhD/BijelData/Bijel_Data_Cleaner_ToRead.csv')
exp_Dat = pd.read_csv(datFile) #read in experimental data file
exp_Dat.index=exp_Dat['Sample Number'] #rename rows as sample numbers


autocorr_Dat = pd.DataFrame(index=exp_Dat.index, columns=['AutoTurn', 'AutoTurnPart'])
imageDir='/Volumes/PhD/BijelData/Python/TIFs/'
workingDir='/Volumes/PhD/BijelData/ChannelsAdded/'
images=listTif_nohidden(imageDir)

for image in images:
    a0 = io.imread(os.path.join(imageDir,image))[0]
    a1 = io.imread(os.path.join(imageDir,image))[0]
    a=a0+a1
    b = np.asarray(a)
    ac3 = correlate(b,b)

    autoCorr = (radial_profile(ac3,(b.shape[0]/2,b.shape[1]/2))[:256]/radial_profile(ac3,(b.shape[0]/2.,b.shape[1]/2.))[:256][0]) #256 should be replaced by half image size, i.e. this is for a 512x512 image
    radProf = (radial_profile(b,(b.shape[0]/2,b.shape[1]/2))[:256]/radial_profile(b,(b.shape[0]/2.,b.shape[1]/2.))[:256][0]) #256 should be replaced by half image size, i.e. this is for a 512x512 image

    outA = open(os.path.join(workingDir,'{}_autoCorr_bothChannels.txt').format(image),'w') #outputs to file called autoCorr.txt
    outR = open(os.path.join(workingDir,'{}_radProf_bothChannels.txt').format(image),'w') #outputs to file called autoCorr.txt

    for j in range(len(autoCorr)):
        bufA = '%s \n' %(str(autoCorr[j]))
        outA.write(bufA)
        bufR = '%s \n' %(str(radProf[j]))
        outR.write(bufR)

    # pl.plot(autoCorr)
    # pl.xlabel('r/pixels')
    # pl.ylabel('Intensity')
    # pl.savefig('%s_autoCorr.png'%image)
    # pl.cla()
