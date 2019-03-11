import sys
import os
import datetime

try:
    Prog = os.environ['SYCLIST_PROG']
    working_dir = os.environ['CLUSTER_DIR']
except KeyError:
    Prog = raw_input('You did not yet set the needed environment variables.\nEnter the path to the SYCLIST program(full path): ')
    working_dir = raw_input('Enter the path to the clusters computation folder: ')
    os.environ['SYCLIST_PROG'] = Prog
    os.environ['CLUSTER_DIR'] = working_dir
    bp = open(os.path.expanduser('~/.bash_profile'),'a')
    bp.write('####################\n# SYCLIST variables \n####################\n')
    bp.write('SYCLIST_PROG="'+Prog+'"\nexport SYCLIST_PROG\n')
    bp.write('CLUSTER_DIR="'+working_dir+'"\nexport CLUSTER_DIR\n')
    bp.close()

start_dir = os.getcwd()
if start_dir != working_dir:
    os.chdir(working_dir)
ClusterFile = open('ClusterRequest.txt','r')

first = ClusterFile.readline()
if first[0] != '-':
  ClusterFile.readline()
user_name = ClusterFile.readline().lstrip().rstrip()
user_name = user_name[0:user_name.rfind('@')].replace('Cluster request from ','').lstrip()
print 'Cluster request from ',user_name
ClusterFile.readline()
grids = ClusterFile.readline().lstrip().rstrip()
grids = grids.replace('Grids:','').lstrip()
print 'Grids: ',grids
metallicity = ClusterFile.readline().lstrip().rstrip()
metallicity = float(metallicity.replace('Metallicity:','').lstrip())
print 'Metallicity: ',str(metallicity)
age = ClusterFile.readline().lstrip().rstrip()
age = float(age.replace('Cluster age:','').lstrip())
print 'Age: ',str(age)
star_number = ClusterFile.readline().lstrip().rstrip()
star_number = int(star_number.replace('Star number:','').lstrip())
print 'Star number: ',str(star_number)
ClusterFile.readline()
rotation_distrib = ClusterFile.readline().lstrip().rstrip()
rotation_distrib = rotation_distrib.replace('Rotation distribution:','').lstrip()
print 'Rotation distribution: ',rotation_distrib
if rotation_distrib == 'Dirac':
    rotation_rate = ClusterFile.readline().lstrip().rstrip()
    rotation_rate = float(rotation_rate.replace('Dirac rotation value:','').lstrip())
    rotation_rate = float(rotation_rate)
    print 'Dirac rotation value: ',str(rotation_rate)
else:
    rotation_rate = 0.0
angle_distrib = ClusterFile.readline().lstrip().rstrip()
angle_distrib = angle_distrib.replace('Angle distribution:','').lstrip()
print 'Angle distribution: ',angle_distrib
if angle_distrib == 'Dirac':
    angle = ClusterFile.readline().lstrip().rstrip()
    angle = angle.replace('Dirac angle value:','').lstrip()
    angle = float(angle)
    print 'Dirac angle value: ',str(angle)
else:
    angle = 22.0
grav_dark = ClusterFile.readline().lstrip().rstrip()
grav_dark = grav_dark.replace('Gravity darkening:','').lstrip()
print 'Gravity darkening: ',grav_dark
limb_dark = ClusterFile.readline().lstrip().rstrip()
limb_dark = limb_dark.replace('Limb darkening:','').lstrip()
print 'Limb darkening: ',limb_dark
ClusterFile.readline()
binary = ClusterFile.readline().lstrip().rstrip()
binary = float(binary.replace('Binary fraction:','').lstrip())
print 'Binary fraction: ',binary
noise = ClusterFile.readline().lstrip().rstrip()
noise = noise.replace('Noise:','').lstrip()
print 'Noise: ',noise
if noise == 'True':
    nMV = ClusterFile.readline().lstrip().rstrip()
    nMV = float(nMV.replace('Noise in M_V=','').lstrip())
    print 'Noise in M_V: ',nMV
    nBV = ClusterFile.readline().lstrip().rstrip()
    nBV = float(nBV.replace('Noise in B-V=','').lstrip())
    print 'Noise in B-V: ',nBV
else:
    nMV = 0.0
    nBV = 0.0
colours = ClusterFile.readline().lstrip().rstrip()
colours = colours.replace('Colours calibration:','').lstrip()
print 'Colours calibration: ',colours
ClusterFile.readline()
ClusterFile.readline()
request_date = ClusterFile.readline().lstrip().rstrip()
request_date = datetime.datetime.strptime(request_date[0:19], "%Y-%m-%d %H:%M:%S")

ClusterFile.close()

if rotation_distrib == 'ext' or angle_distrib == 4:
    with open('ClusterRequest.txt','a') as ClusterFile:
        ClusterFile.write('\n\n')
        if rotation_distrib == 'ext':
            OOc_file = raw_input('Enter the OOc distribution file (with path):\n')
            ClusterFile.write('OOc distrib. file: '+OOc_file+'\n')
            if OOc_file[0] not in ["'",'"']:
                OOc_file = "'"+OOc_file+"'"
        if angle_distrib == 'ext':
            angle_file = raw_input('Enter the angle distribution file (with path):\n')
            ClusterFile.write('angle distrib. file: '+angle_file+'\n')
            if angle_file[0] not in ["'",'"']:
                angle_file = "'"+angle_file+"'"

date_suffix = request_date.strftime('%y%m%d_%H%M%S')
if not os.path.exists(user_name):
    os.makedirs(user_name)
os.system('cp ClusterRequest.txt '+user_name+'/ClusterRequest'+date_suffix+'.txt')
os.chdir(user_name)
os.system('pwd')

if grids == 'BeGrids':
    Mmin = 1.7
    Mmax = 15.
else:
    Mmin = 0.8
    Mmax = 120.

rotation_distrib = {'flat':'0','H10':'1','HG06':'2','Dirac':'3','ext':'4'}.get(rotation_distrib)
angle_distrib = {'None':'0','Vsini':'2','Dirac':'3','ext':'4'}.get(angle_distrib)
grav_dark = {'vZ24':'1','ELR11':'2'}.get(grav_dark)
noise = {'False':'0','True':'1'}.get(noise)
limb_dark = {'False':'0','True':'1'}.get(limb_dark)
colours = {'old':'1','WL11':'2'}.get(colours)

ConfigFile = open('.Config_SYCLIST','w')
ConfigFile.write('*******************************\n')
ConfigFile.write('Configuration file for SYCLIST\n')
ConfigFile.write('Do not edit unless knowing what you are doing !\n')
ConfigFile.write('*******************************\n')
ConfigFile.write('Grid:   '+grids+'\n')
ConfigFile.write('Star Number:   %9d'%star_number+'\n')
ConfigFile.write('Metallicity distribution:   0\n')
ConfigFile.write('Metallicity:   '+str(metallicity)+'\n')
ConfigFile.write('IMF_type:   1\n')
ConfigFile.write('Minimal mass:   %9.4f'%Mmin+'\n')
ConfigFile.write('Maximal mass:   %9.4f'%Mmax+'\n')
ConfigFile.write('Velocity distribution:   '+rotation_distrib+'\n')
ConfigFile.write('Velocity value (dirac):   %6.4f'%rotation_rate+'\n')
ConfigFile.write('Angle of view distribution:   '+angle_distrib+'\n')
ConfigFile.write('Angle of view (dirac):   %7.2f'%angle+'\n')
ConfigFile.write('Account for the noise:   '+noise+'\n')
ConfigFile.write('Variance in M_V:   %7.4f'%nMV+'\n')
ConfigFile.write('Variance in B-V:   %7.4f'%nBV+'\n')
ConfigFile.write('Binary probability:   %7.2f'%binary+'\n')
ConfigFile.write('Number of beam in mass (population):    1000\n')
ConfigFile.write('Number of beam in velocity (population):     100\n')
ConfigFile.write('Colour - Teff calibration:   %5s'%colours+'\n')
ConfigFile.write('Gravity Darkening:   %4s'%grav_dark+'\n')
ConfigFile.write('Limb Darkening:   %4s'%limb_dark+'\n')
ConfigFile.close()

TempFile = open('TempFile.txt','w')
TempFile.write('n\n1\n'+str(age)+'\n')
if rotation_distrib == '4':
    TempFile.write(OOc_file+'\n')
if angle_distrib == '4':
    TempFile.write(angle_file+'\n')
TempFile.write('y\n')
os.system('more TempFile.txt')
TempFile.close()

ProgLaunch = Prog+' < TempFile.txt'
SaveFile = 'Cluster_z%06.4f'%metallicity+'_t%06.3f'%age
MoveFile = 'mv '+SaveFile+'.dat '+SaveFile+'_'+date_suffix+'.dat'
os.system(ProgLaunch)
os.system(MoveFile)
os.system('rm TempFile.txt')

os.chdir(start_dir)
print '***** Cluster for '+user_name+' computed *****'
print '-----------------------------------------------------------------'
print 'Dear SYCLIST user,\n'
print 'Please find below the cluster you requested on ',request_date.strftime('%a %d %b %Y at %H:%M:%S'),' (CET).\n'
print 'The file format can be found here: https://obswww.unige.ch/Recherche/nevoldb/index/Formats/#cluster'
print '-----------------------------------------------------------------'
