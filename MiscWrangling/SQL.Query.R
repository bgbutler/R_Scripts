
#test SQL query

library(RODBC)

ch <- odbcConnect("RSQLData")
db <- "CORE_APP_APPR_BUDGET.dbo."
q = paste("SELECT * FROM ", db, "clients", sep = "")


TestData <- sqlQuery(ch, q)


odbcClose(ch)

head(TestData)