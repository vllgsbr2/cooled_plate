import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors
import matplotlib.animation as animation
import types
from matplotlib import cm
np.set_printoptions(threshold=np.nan)

#DATA contains many matricies in 3d list of NXxNYxEPOCH in shape
DATA = np.loadtxt('hPlate_Data.txt', delimiter=',', dtype=str)
shape = np.loadtxt('shape.txt', delimiter=' ', dtype=str)
shape = shape.astype(dtype=int)

#cast elements of DATA into floats and throw out any '' in DATA
data=[]
for i in range(len(DATA)):
    for j in range(len(DATA[i])):
        if DATA[i][j]!='':
            data.append(float(DATA[i][j]))

#reshape data into DATA by shape of results
DATA = np.reshape(data, (shape[0]-1, shape[1]-1, shape[2]+1), order='F')

#make an animated GIF of hot plate being cooled down
fig,ax = plt.subplots()
def animate(i):
       ax.clear()
       ax.contourf(DATA[:,:,i], vmin=0.0001, vmax=1000,  \
        norm=matplotlib.colors.PowerNorm(gamma=1./6.), cmap = 'jet')
       ax.set_title('%03d'%(i))

interval = 1/20#in seconds
ani = animation.FuncAnimation(fig,animate,interval=interval*1e+3,blit=False)

plt.show()
