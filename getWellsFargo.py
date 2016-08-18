# -*- coding: utf-8 -*-
"""
Created on Mon Feb 29 17:03:36 2016

@author: bbutler
"""

import os, fnmatch

myDirname = os.path.normpath("K:/")

def find(pattern, path):
    result = []
    for root, dirs, files in os.walk(path):
        for name in files:
            if fnmatch.fnmatch(name,pattern):
                result.append(os.path.join(root,name))
    return result

wellsFargo = find("*fargo*", myDirname)

with open("K:\\Sandbox\\pythonWells.txt",'w') as file:
    for item in wellsFargo:
        file.write("%s\n" % item) 
        
        
        
        
        