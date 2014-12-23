# This is code to extract enso data
# It first downloads the files using a .raw indicator
# Then it writes a .csv of the data only using a .clean indicator


# Get the ENSO data and store it as a .CSV file there are two files
# The ranks and the actual normalized index
library(httr)
require(httr)
#The Ranks data is at the URL below
fileUrl = "http://www.esrl.noaa.gov/psd/enso/mei.ext/rank.ext.html"

#The actual index data is at the URL below
#fileUrl = "http://www.esrl.noaa.gov/psd/enso/mei.ext/table.ext.html"

download.file(fileUrl, destfile="/Users/Bryan/Documents/RFiles/enso.ranks.raw.csv", method="curl")
fileLoc <- "~/Documents/RFiles/enso.ranks.raw.csv"




# Read the ENSO index data vs the ranks
ENSOdata <-read.table(file=fileLoc, header=TRUE, sep="", skip=12, nrows=135)
head(ENSOdata)

# Read the ENSO ranks data
ENSOdata <-read.table(file=fileLoc, header=TRUE, sep="", skip=24, nrows=135)
head(ENSOdata)

# Write the data to a clean .CSV
write.csv(ENSOdata, file="~/Documents/RFiles/enso.clean.csv", row.names=FALSE)

# Write the ranks data to a clean .CSV
write.csv(ENSOdata, file="~/Documents/RFiles/enso.ranks.clean.csv", row.names=FALSE)


# Get post 1950 ENSO data
Url1950 = "http://www.esrl.noaa.gov/psd/enso/mei/table.html"
download.file(Url1950, destfile="/Users/Bryan/Documents/RFiles/enso1950.raw.csv", method="curl")
fileLoc <- "~/Documents/RFiles/enso1950.raw.csv"

# Read the ENSO 1950 data index data vs the ranks
ENSO1950data <-read.table(file=fileLoc, header=TRUE, sep="", skip=13, nrows=64)
head(ENSO1950data)

# Write the ENSO 1950 data to a clean .CSV
write.csv(ENSO1950data, file="~/Documents/RFiles/enso1950.clean.csv", row.names=FALSE)