#load in the Excel files
library(readxl)

#get a list of the files
url <- "K:/Sandbox/Bryan Projects/Activity Reports/"

files <- list.files(url, full.names = TRUE)
count <- length(files)

#loop through all files and combine
allReports <- data.frame()

for (i in 1:count){
  allReports <- rbind(allReports, 
                      read_excel(files[i], sheet = 2, col_names = TRUE,
                                 col_types = NULL, na = "", skip = 1))
}