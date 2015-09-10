#script to test the SQL Connectivity

library(RODBC)

channel <- odbcConnect("RDupe")
db <-"CORE_APP_APPR_BUDGET.dbo"
query <- c("Select * FROM ", db, ".clients")

TestData = sqlQuery(channel = channel, 
                    paste("Select * from ", db, ".clients", sep = ""))