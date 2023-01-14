import os,sys
import webbrowser
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.cm as cm
import matplotlib.pylab as plt

plt.rcParams['font.family'] = 'monospace'
fig = plt.figure()
def colors(cents):
	colors = ['blue', 'red', 'green'][:len(cents)]
	pos = cents.index(min(cents, key=lambda c:((c[0]-1)**2) + ((c[1]-1)**2)))
	neg = cents.index(min(cents, key=lambda c:((c[0]+1)**2) + ((c[1]+1)**2)))
	rem = list(set(range(len(cents))) - set([pos,neg]))
	return [c for n,c in sorted(zip([pos,neg] + rem, colors))]

if sys.argv[1] == 'truth':
	K = int(sys.argv[3])
	train = np.loadtxt('train.dat', delimiter=',')
	dense = np.loadtxt('dense.dat', delimiter=',')
	ax = fig.add_subplot(111, aspect='equal')
	plt.xlim(-3.0, 3.0)
	plt.ylim(-3.0, 3.0)
	plt.xlabel("")
	plt.ylabel("")
	plt.grid(ls='dotted')
	X = np.arange(-3.0, 3.05, 0.05)
	Y = np.arange(-3.0, 3.05, 0.05)
	Xt, Yt = np.hsplit(train, 2)
	Xm, Ym = np.meshgrid(X, Y)
	levels = 8
	ax.contourf(Xm, Ym, dense, levels, alpha=1, cmap=cm.jet)
	CR = plt.contour(Xm, Ym, dense, levels, colors='black')
	CR.clabel(fontsize=12, fmt='%.2f')
	ax.scatter(Xt, Yt, marker='.', color='white', s=10)
	plt.savefig('gmm.{}.svg'.format(sys.argv[2]), bbox_inches='tight', pad_inches=0.1)
	plt.savefig('gmm.{}.eps'.format(sys.argv[2]), bbox_inches='tight', pad_inches=0.1)
	os.remove('train.dat')
	os.remove('dense.dat')
	webbrowser.open('file://{}'.format(os.path.realpath('gmm.{}.svg'.format(sys.argv[2]))))

elif sys.argv[1] == 'KM':
	K = int(sys.argv[3])
	cents = np.loadtxt('cents.dat', delimiter=',').tolist()
	mixts = [np.loadtxt('mixt{}.dat'.format(k), delimiter=',') for k in range(K)]
	ax = fig.add_subplot(111, aspect='equal')
	plt.xlim(-3.0, 3.0)
	plt.ylim(-3.0, 3.0)
	plt.xlabel("")
	plt.ylabel("")
	plt.grid(ls='dotted')
	for c,mixt,cent in zip(colors(cents),mixts,cents):
		if len(mixt):
			ax.scatter(mixt[:,0], mixt[:,1], marker='.', color=c, s=10)
	for c,mixt,cent in zip(colors(cents),mixts,cents):
		if len(mixt):
			ax.scatter([cent[0]], [cent[1]], marker='*', facecolor=c, edgecolor='black', lw=1, s=150)
	plt.savefig('gmm.{}.svg'.format(sys.argv[2]), bbox_inches='tight', pad_inches=0.1)
	plt.savefig('gmm.{}.eps'.format(sys.argv[2]), bbox_inches='tight', pad_inches=0.1)
	os.remove('cents.dat')
	for k in range(K): os.remove('mixt{}.dat'.format(k))
	webbrowser.open('file://{}'.format(os.path.realpath('gmm.{}.svg'.format(sys.argv[2]))))

else:
	K = int(sys.argv[3])
	cents = np.loadtxt('cents.dat', delimiter=',').tolist()
	dense = np.loadtxt('dense.dat', delimiter=',')
	mixts = [np.loadtxt('mixt{}.dat'.format(k), delimiter=',') for k in range(K)]
	ax = fig.add_subplot(111, aspect='equal')
	plt.xlim(-3.0, 3.0)
	plt.ylim(-3.0, 3.0)
	plt.xlabel("")
	plt.ylabel("")
	plt.grid(ls='dotted')
	X = np.arange(-3.0, 3.05, 0.05)
	Y = np.arange(-3.0, 3.05, 0.05)
	Xm, Ym = np.meshgrid(X, Y)
	levels = 8
	CR = plt.contour(Xm, Ym, dense, levels, colors='black')
	for c,mixt,cent in zip(colors(cents),mixts,cents):
		if len(mixt):
			ax.scatter(mixt[:,0], mixt[:,1], marker='.', color=c, s=10)
	for c,mixt,cent in zip(colors(cents),mixts,cents):
		if len(mixt):
			ax.scatter([cent[0]], [cent[1]], marker='*', facecolor=c, edgecolor='black', lw=1, s=150)
	plt.savefig('gmm.{}.svg'.format(sys.argv[2]), bbox_inches='tight', pad_inches=0.1)
	plt.savefig('gmm.{}.eps'.format(sys.argv[2]), bbox_inches='tight', pad_inches=0.1)
	os.remove('cents.dat')
	os.remove('dense.dat')
	for k in range(K): os.remove('mixt{}.dat'.format(k))
	webbrowser.open('file://{}'.format(os.path.realpath('gmm.{}.svg'.format(sys.argv[2]))))
