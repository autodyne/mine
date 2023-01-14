import os,sys
import webbrowser
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pylab as plt
from matplotlib.colors import LinearSegmentedColormap

plt.rcParams['font.family'] = 'monospace'
fig = plt.figure()
ax = fig.add_subplot(111, aspect='equal')
data0 = np.loadtxt('data0.dat', delimiter=',')
data1 = np.loadtxt('data1.dat', delimiter=',')
data2 = np.loadtxt('data2.dat', delimiter=',')
paint = np.loadtxt('class.dat', delimiter=',')
ID = sys.argv[1]
X = np.arange(-100, 101)
Y = np.arange(-100, 101)
Xm, Ym = np.meshgrid(X, Y)
colors = []
colors.append((0.6, 0.6, 1.0, 1.0))
colors.append((1.0, 1.0, 1.0, 1.0))
colors.append((1.0, 0.6, 0.6, 1.0))
cmap = LinearSegmentedColormap.from_list('pafe', colors, 3)
ax.pcolorfast(X, Y, paint, cmap=cmap)
ax.scatter(data0[:,0], data0[:,1], marker='^', facecolor=[0 if c < 1 else c for c in colors[0][:3]], edgecolor='black', s=20, lw=1)
ax.scatter(data1[:,0], data1[:,1], marker='o', facecolor=[0 if c < 1 else c for c in colors[1][:3]], edgecolor='black', s=20, lw=1)
ax.scatter(data2[:,0], data2[:,1], marker='v', facecolor=[0 if c < 1 else c for c in colors[2][:3]], edgecolor='black', s=20, lw=1)
plt.xlim(X[0], X[-1])
plt.ylim(Y[0], Y[-1])
plt.xlabel("")
plt.ylabel("")
plt.grid(ls='dotted')
plt.savefig('id3.{}.svg'.format(ID), bbox_inches='tight', pad_inches=0.1)
plt.savefig('id3.{}.eps'.format(ID), bbox_inches='tight', pad_inches=0.1)
os.remove('class.dat')
os.remove('data0.dat')
os.remove('data1.dat')
os.remove('data2.dat')
webbrowser.open('file://{}'.format(os.path.realpath('id3.{}.svg'.format(ID))))
