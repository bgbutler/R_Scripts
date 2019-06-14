#script to analyze survey data


.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))



getwd()

setwd(url)

url <- "S:/TEMP/NetInsights"
txtNew <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)


textFiles = list.files(url, pattern = "\\.txt", full.names = T)

l = length(textFiles)


allText3 <- data.frame()
colnames(allText1) <- c("FNumber", "NNumber", "Date")


for (i in 105:357) {
    allText3 <- rbind(allText3,
                     read_data(textFiles[i], header = FALSE, sep=";"))
}




write.csv(allText2, file = "S:/TEMP/NetInsights/alltextNext5.csv", row.names = FALSE)
write.csv(allText3, file = "S:/TEMP/NetInsights/allfinal.csv", row.names = FALSE)





