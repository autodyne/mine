import os,sys
import unidecode
import webbrowser
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pylab as plt
import cartopy.crs as ccrs
import cartopy.io.shapereader as shp

def degree(d, m, s):
	return d + m / 60.0 + s / 3600.0

def truncate(text):
	return str(unidecode.unidecode(text.replace('\x00', '')))

small = True
prefs = {truncate(k):v for k,v in np.loadtxt('pref.dat', delimiter=',', dtype='unicode')}
if small:
	nlim = degree( 45, 0, 0)
	slim = degree( 25, 0, 0)
	elim = degree(150, 0, 0)
	wlim = degree(125, 0, 0)
else:
	nlim = degree( 50, 55, 30)
	slim = degree( 20, 25, 31)
	elim = degree(156, 19,  0)
	wlim = degree(122, 55, 57)
plt.rcParams['font.family'] = 'monospace'
fig = plt.figure(figsize=(8, 8))
ax = fig.add_subplot(111, aspect='equal', projection=ccrs.Mercator())
ax.outline_patch.set_visible(False)
ax.set_extent((wlim, elim, slim, nlim))
country = shp.Reader(shp.natural_earth(resolution='10m', category='cultural', name='admin_1_states_provinces'))
invaded = shp.Reader(shp.natural_earth(resolution='10m', category='cultural', name='admin_0_disputed_areas'))
for pref in [p for p in country.records() if truncate(p.attributes['admin']) == 'Japan']:
	color = prefs[truncate(pref.attributes['name'])]
	ax.add_geometries([pref.geometry], crs=ccrs.PlateCarree(), facecolor=color, edgecolor='black', lw=1)
for pref in [p for p in invaded.records() if 'Claimed by Japan' in p.attributes['NOTE_BRK']]:
	color = prefs['Hokkaido'] if truncate(pref.attributes['ADMIN']) == 'Russia' else prefs['Shimane']
	ax.add_geometries([pref.geometry], crs=ccrs.PlateCarree(), facecolor=color, edgecolor='black', lw=1)
plt.savefig('nbc.jmap{}.svg'.format(sys.argv[1]), bbox_inches='tight', pad_inches=0)
plt.savefig('nbc.jmap{}.eps'.format(sys.argv[1]), bbox_inches='tight', pad_inches=0)
os.remove('pref.dat')
webbrowser.open('file://{}'.format(os.path.realpath('nbc.jmap{}.svg'.format(sys.argv[1]))))
