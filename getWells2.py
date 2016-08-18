# -*- coding: utf-8 -*-
"""
Created on Tue Mar  1 11:56:30 2016

@author: bbutler
"""

import os, fnmatch

myDirname = os.path.normpath("K:/")

wells1 = "*fargo*"
wells2 = "*wf *"


def find_wf(pattern1, path):
    with open("K:\\Sandbox\\pythonWF.txt", "a") as wells:
        count = 0        
        for root, dirs, files in os.walk(path):
            for name in files:
                if fnmatch.fnmatch(name,pattern1):
                    count+=1
                    f = os.path.join(root,name)
                    wells.write(str(count) + '\t' + str(f) + os.linesep)

#This section is used to create a combined file
#                if fnmatch.fnmatch(name,pattern2):
#                    count+=1
#                    f = os.path.join(root,name)
#                    wells.write(str(count) + '\t' + str(f) + os.linesep)

                    
wellsFargo = find_wf(wells2, myDirname)
 