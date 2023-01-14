import os,sys
import webbrowser
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pylab as plt

plt.rcParams['font.family'] = 'monospace'
fig = plt.figure(figsize=(12,3))
ax = fig.add_subplot(111)
X,Y,T = np.loadtxt('wave.dat', delimiter=',').T
ax.plot(np.arange(0, len(X)), X, 'bv', lw=1, label='$x$')
ax.plot(np.arange(0, len(X)), T, 'k-', lw=2, label='$y$')
ax.plot(np.arange(0, len(X)), Y, 'r^', lw=1, label='$f$')
plt.grid(ls='dotted')
plt.xlim(0, len(X)-1)
plt.ylim(0, 1)
plt.xlabel("")
plt.ylabel("")
plt.legend(loc='upper right')
plt.savefig('rnn.phase.svg', bbox_inches='tight', pad_inches=0.1)
plt.savefig('rnn.phase.eps', bbox_inches='tight', pad_inches=0.1)
os.remove('wave.dat')
webbrowser.open('file://{}'.format(os.path.realpath('rnn.phase.svg')))
