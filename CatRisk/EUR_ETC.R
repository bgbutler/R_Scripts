library(XML)
library(ggplot2)

#load in the CSV data
# generated a list of three tables
#dataURL <- "http://www.europeanwindstorms.org/cgi-bin/storms/storms.cgi?sort=date&opt="
#tbl <- readHTMLTable(dataURL)


dataURL <- "http://www.europeanwindstorms.org/cgi-bin/storms/storms.cgi?sort=date&opt="
tbl <- readHTMLTable(dataURL, header = TRUE, which = 3, skip = 1)


headers <- c("Storm", "DateFactor","InsuredLoss", "Countries", "Umax", 
             "LowestMSLP", "MaxVorticity", "Sft")

#clean up headers
colnames(tbl) <- headers
tbl$DateClean <- as.character(tbl$Date)

#clean up dates

tbl$DateClean <- format(tbl$DateClean, format = "%Y-%m-%d")

tbl$DateClean <- as.Date(tbl$DateClean, format = "%d/%M/%Y")

#write to .CSV
write.csv(tbl, file = "~/Documents/RFiles/R_Scripts/ETC.csv", 
          row.names = FALSE)

