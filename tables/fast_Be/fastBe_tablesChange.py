from origin_tools.Origin_Tools import *
import argparse
import sys

parser = argparse.ArgumentParser()
parser.add_argument('StarName',help='Star name.',type=str)
parser.add_argument('-i','--start_line',help='Line to be used as reference.',type=int,default=0)

args = parser.parse_args()

starname = args.StarName
minidx = args.start_line
zz = 'Z0'+starname[starname.find('Z')+1:starname.find('Z')+3]

OOcSurfV = np.loadtxt('/Users/ekstrom/OBS/Programs/UtilsEvol/AllOmegaDataLarge.dat')
OOc_interp = OOcSurfV[:,0]
Surf_interp = OOcSurfV[:,1]
VVc1_interp = OOcSurfV[:,13]

table_dir = '/Users/ekstrom/OBS/Grids2010/Be/tables/fastBe/'+zz+'/'
loadE(table_dir+starname+'.dat',forced=True,quiet=True)

print 'file loaded, computing new variables...'

ooc = Get_Var('OOc',1)
ttauH = Get_Var('t_tauH',1)
if minidx==0:
    minidx_ooc = np.argmin(ooc[0:85])
    minidx_MS75 = np.where(ttauH<0.075)[0][-1]+1
    minidx_MS10 = np.where(ttauH<0.1)[0][-1]+1
    if minidx_ooc <= minidx_MS10:
        minidx = minidx_ooc
    else:
        minidx = minidx_MS75
    print 'idx min OOc:',minidx_ooc,' - idx 10% MS:',minidx_MS10,' - idx chosen:',minidx
ooc_start = ooc[minidx]
print 'New OOc ini:',ooc_start
lum = Get_Var('L',1)
teff = Get_Var('Teff',1)

oblat = Get_Var('oblat',1)
alpro6 = Get_Var('rot_corr',1)
omsurf = Get_Var('Omega_surf',1)
omcen = Get_Var('Omega_cen',1)
mass = Get_Var('M',1)

new_ooc = np.zeros((minidx))
new_teff = np.zeros((minidx))
new_rpol = np.zeros((minidx))
new_oblat = np.zeros((minidx))
new_veq = np.zeros((minidx))
new_vcrit = np.zeros((minidx))
new_vvcrit = np.zeros((minidx))
new_ocrit = np.zeros((minidx))
new_osurf = np.zeros((minidx))
new_ocen = np.zeros((minidx))
new_alpro6 = np.zeros((minidx))
vcrit2 = 0.

new_surf = np.interp(ooc_start,OOc_interp,Surf_interp)

for i in range(minidx):
    new_ooc[i] = ooc_start
    new_oblat[i] = oblat[minidx]
    new_alpro6[i] = alpro6[minidx]
    new_ocen[i] = omcen[minidx]
    old_surf = np.interp(ooc[i],OOc_interp,Surf_interp)
    new_teff[i] = np.log10(10.**teff[i] * old_surf / new_surf)
    new_rpol[i] = np.sqrt(10.**lum[i]*apc.CGS.Lsol/(apc.CGS.sigma*10.**(4.*new_teff[i])*new_surf))
    new_ocrit[i] = np.sqrt(apc.CGS.G*mass[i]*apc.CGS.Msol/(1.5*new_rpol[i])**3.)
    new_osurf[i] = new_ooc[i]*new_ocrit[i]
    new_veq[i] = new_rpol[i]*new_osurf[i]/new_oblat[i]/1.e5
    new_vvcrit[i] = np.interp(ooc_start,OOc_interp,VVc1_interp)
    new_vcrit[i] = new_veq[i]/new_vvcrit[i]

np.savetxt(table_dir+starname+'_tableNewLine.dat',\
           np.transpose([new_teff,new_osurf,new_ocen,new_oblat,new_alpro6,new_vcrit,new_veq,new_ooc]),\
           header='    Teff  Omega_surf  Omega_cen     Rp/Req   Md/Md(0)   v_crit1   v_crit2    v_equa  Om/Om_cr',\
           fmt='  %8.6f   %9.3E  %9.3E  %9.3E  %9.3E  %8.2E  0.00E+00  %8.2E  %8.6F')

