#This gets the TPC data and converts it to a single file after conversion to CSV

#test the data pulling mechanism
dataURL <- "~/Desktop/SII Data Work/HU_Data_Decades/HU_Data_DecadesClean/2010.data.csv"
huData2010 <- read.csv(dataURL, header = F, as.is = TRUE)

#set the working directory
setwd("~/Desktop/SII Data Work/HU_Data_Decades/HU_Data_DecadesClean")


#set up the input and output paths to first clean the data before combining
inPath = "~/Desktop/SII Data Work/HU_Data_Decades/HU_Data_DecadesClean"
out.file <- ""
file.names <- dir(inPath, pattern =".csv")

#loop through and open the files to clean up the NOT NAMED issue
for(i in 1:length(file.names)){
  file <- read.csv(file.names[i],header=F, stringsAsFactors=FALSE, sep = ",")
  out.file <- rbind(out.file, file)
}

#set the proper working directory for the out file
setwd("~/Desktop/SII Data Work/HU_Data_Decades/HU_Data_DecadesClean")

#create headers for the data
headers <- c("year", "month", "day", "time", "count", "name", "name2", "lat", "long", "ws", "cp")
colnames(out.file) <- headers

#rename file, check data, clear the blank row in line 1
huAllYears <- out.file[2:46990,]
head(huAllYears)


#use paste to combined the not named values in the columns
huAllYears$name <- paste(huDataAllYears$name, huAllYears$name2, sep ="")



#write the final file as a .txt file
write.csv(huAllYearsClean, file = "HU_Data_AllDecades.csv", 
            row.names = FALSE, qmethod = "double")

#drop the column that was used to hold the not named value column name2
huAllYearsClean <- huAllYears[,c("year", "month", "day", "time", "count", 
                                 "name", "lat", "long", "ws", "cp")]

#write the data file to a CSV
write.csv(huAllYearsClean, file = "HU_Data_AllDecades.csv", 
          row.names = FALSE, qmethod = "double")



#write the final file as a .txt file
write.table(out.file, file = "HU_Data_AllDecades.txt", 
            row.names = FALSE, qmethod = "double",fileEncoding="windows-1252")