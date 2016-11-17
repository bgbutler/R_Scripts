#script to analyze cross sell opportunities

.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

#test the odbc connectivity of R

library(RODBC)

cn <- odbcDriverConnect(connection="Driver={SQL Server};Server=WYORETAVO1;Database=SAS_Persona_Cor_Anon;")

SQLCommand <- "SELECT name FROM Master.dbo.sysdatabases Where name NOT IN ('master', 'tempdb', 'model', 'msdb')"


database <- "SAS_Persona_Cor_Anon"

SQLCommand3 <- sprintf("SELECT SCHEMA_NAME(schema_id)+'.'+name AS SchemaTable FROM sys.tables ORDER by SchemaTable",
                       database)


SQLCommand2 <- "SELECt * FROM CAlC.HIERARCHY"

#rtest the speed of a moderately large dataset
#startTime <- Sys.time()
cn <- odbcDriverConnect(connection="Driver={SQL Server};Server=WYORETAVO1;Database=SAS_Persona_Cor_Anon;")
tables <- sqlQuery(cn, SQLCommand3)
tables

databases <- sqlQuery(cn, SQLCommand)

hierarchy <- sqlQuery(cn, SQLCommand2)
heirarchy
#endTime1 <- Sys.time()
odbcClose(cn)

#timeRun <- difftime(endTime1, startTime, units = "secs")
#print(timeRun)


dataBases

head(investments)