

# Get the AMO data and store it as a .CSV file
# This is the raw format
fileUrl = "http://www.esrl.noaa.gov/psd/data/correlation/amon.us.long.data"
fileLoc <- "~/Documents/RFiles/AMO.long.raw.csv"
download.file(fileUrl, destfile=fileLoc, method="curl")

#Read only the data there are no column names in the data
AMOdata <-read.table(file=fileLoc, header=FALSE, sep="", skip=1, nrows=157)
head(AMOdata)

# Apply appropriate column names
colnames(AMOdata) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                       "Aug", "Sep", "Oct", "Nov", "Dec")
head(AMOdata)

# Write the data to clean .CSV file
write.csv(AMOdata, file="~/Documents/RFiles/AMO.long.clean.csv", row.names=FALSE)