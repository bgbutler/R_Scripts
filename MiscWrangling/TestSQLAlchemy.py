# -*- coding: utf-8 -*-

#Created on Fri Jul 31 13:50:40 2015

#@author: bbutler


import sys
import sqlalchemy as sa

print (sys.version)


#from sqlalchemy import create_engine
engine = sa.create_engine("mssql://dsn=RSQLData")

for row in engine.execute("""
select EnterpriseGUID, ClientName, AdminConsoleName
from CORE_APP_APPR_BUDGET.dbo.client"""):
    print (row.ClientName, row.AdminConsoleName)




