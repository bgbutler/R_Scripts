# -*- coding: utf-8 -*-
"""
Created on Fri Jul 31 13:50:40 2015

@author: bbutler
"""

import sys
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
print (sys.version)



import pyodbc as pyo
conn = pyo.connect('DSN=RSQLData')
cursor = conn.cursor()

cursor.execute("""
select EnterpriseGUID, ClientName, AdminConsoleName
from CORE_APP_APPR_BUDGET.dbo.clients""")
row = cursor.fetchmany(10)

print (row)

df = pd.DataFrame(row)

print (df)