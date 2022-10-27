import numpy as np
import os
from datetime import datetime as dt

input_file = '/Users/ekstrom/OBS/Groupe/SYCLIST_Clusters/ClusterRequest.txt'
prog_launch = 'python3 /Users/ekstrom/OBS/Programs/GitHub/SYCLIST/ClusterRequest.py'

def LaunchCluster_ageList(ageList):
  for age in ageList:
    with open(input_file,'r+') as crwrite:
      file_content = crwrite.readlines()
      crwrite.seek(0)
      file_content[-1] = dt.now().strftime("%Y-%m-%d %H:%M:%S.%f")+'\n'
      for line in file_content:
        if line[:12] == 'Cluster age:':
          line = 'Cluster age: {0}\n'.format(age)
        crwrite.write(line)
    os.system(prog_launch)

def LaunchCluster_manySimilar(number):
  for i in range(number):
    if i!=0:
      with open(input_file,'r+') as crwrite:
        file_content = crwrite.readlines()
        file_content[-1] = dt.now().strftime("%Y-%m-%d %H:%M:%S.%f")+'\n'
        crwrite.seek(0)
        for line in file_content:
          crwrite.write(line)
    os.system(prog_launch)
