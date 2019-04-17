import numpy as np
import os
import glob
import pandas as pd
import matplotlib.pyplot as pl

x=np.arange(257)
dir="/Volumes/PhD/BijelData/ParticleChannel/autoCorr"
imList = ["54i_", "57i_", "58i_", "65i_", "67ii_", "68i_"]


filename=glob.glob('/Volumes/PhD/BijelData/ParticleChannel/autoCorr/54i_*')
y=np.genfromtxt(filename[0], delimiter=' ', dtype='float64')

pl.plot(x,y)
pl.show()
