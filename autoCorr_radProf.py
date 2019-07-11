import os
import numpy as np
import matplotlib.pyplot as pl
import scipy.fftpack as fftim
# from scipy.misc.pilutil import Image
import pandas as pd
from skimage import io
import math
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


#Tk().withdraw()
#datFile = askopenfilename()
# datFile=os.path.normpath('/Volumes/PhD/BijelData/Bijel_Data_Cleaner_ToRead.csv')
# exp_Dat = pd.read_csv(datFile) #read in experimental data file
# exp_Dat.index=exp_Dat['Sample Number'] #rename rows as sample numbers


# autocorr_Dat = pd.DataFrame(index=exp_Dat.index, columns=['AutoTurn', 'AutoTurnPart'])

imageDir='../../Documents/Dropbox/sample54i/'
workingDir='../../../../Volumes/PhD/BijelData/sample54i/liquid'
images=listTif_nohidden(imageDir)

imChannel=int(input("Which image channel? (Index starts at 0) "))

for image in images:
    a = io.imread(os.path.join(imageDir,image))[imChannel]
    b = np.asarray(a)
    ac3 = correlate(b,b)

    autoCorr = (radial_profile(ac3,(b.shape[0]/2,b.shape[1]/2))[:256]/radial_profile(ac3,(b.shape[0]/2.,b.shape[1]/2.))[:256][0]) #256 should be replaced by half image size, i.e. this is for a 512x512 image
    radProf = (radial_profile(b,(b.shape[0]/2,b.shape[1]/2))[:256]/radial_profile(b,(b.shape[0]/2.,b.shape[1]/2.))[:256][0]) #256 should be replaced by half image size, i.e. this is for a 512x512 image

    # unscaledX=np.arange(0,256,1.)
    # L=640.17
    # scaleFactorA=2*math.pi/L
    # scaledXA=unscaledX*scaleFactorA

    outA = open(os.path.join(workingDir,'{}_autoCorr_channel{}_microns.txt').format(image, imChannel),'w')
    outR = open(os.path.join(workingDir,'{}_radProf_channel{}_microns.txt').format(image, imChannel),'w')

    for j in range(len(autoCorr)):
        bufA = '%s \n' %(str(autoCorr[j]))
        outA.write(bufA)
        bufR = '%s \n' %(str(radProf[j]))
        outR.write(bufR)

    # pl.plot(scaledXA, autoCorr)
    # pl.xlabel('r (μm)')
    # pl.ylabel('Autocorrelation Function')
    # pl.savefig(os.path.join(workingDir, str(imChannel),'%s_autoCorr_microns.png'%image))
    # pl.cla()
    #
    # scaleFactorR=640.17/512
    # scaledXR=unscaledX*scaleFactorR
    #
    # pl.plot(scaledXR, radProf)
    # pl.xlabel('r (1/μm)')
    # pl.ylabel('Structure Factor')
    # pl.savefig(os.path.join(workingDir, str(imChannel), '%s_radProf_microns.png'%image))
    # pl.cla()
