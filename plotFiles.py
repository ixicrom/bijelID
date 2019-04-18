import numpy as np
import os
import glob
import pandas as pd
import matplotlib.pyplot as pl



filename=glob.glob('/Volumes/PhD/BijelData/ParticleChannel/radProf/76i_*')
y=np.genfromtxt(filename[0], delimiter=' ', dtype='float64')
x=np.arange(len(y))

pl.plot(x,y)
pl.xlabel('q (pixels)')
pl.ylabel('SF')
pl.title('76i, a bijel')
pl.savefig('/Volumes/PhD/BijelData/ParticleChannel/radProf/B_SF_76i')
pl.show()
