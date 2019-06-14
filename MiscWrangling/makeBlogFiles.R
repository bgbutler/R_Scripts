#r file to get all of the blog roles, combine them and write


#loop through all files and combine
#start with the .xlsx files then the .xls files
blogs <- data.frame()


#read the .xlsx files
url <- "K:/COmmunispace Blog Rolls/For Bryan to Compile/"
files <- list.files(url, full.names = TRUE)
orig <- list.files(url, full.names = FALSE)
origClean <- substr(orig, 1, nchar(orig)-4)

count <- length(files)

for (i in 1:count){}
  blogs <- rbind(blogs, 
               read.csv(files[i], header = TRUE, sep=",", as.is = T))
  size[i] <- nrow(blogs)
  print(files[i])
}

colnames(blogs) <- c("FirstName", "LastName", "Email", "Blog", "Traffic", "Source")

# get the size of each appended file
fileSize <- size[1]
for (i in 2:length(size)){
fileSize[i] <- size[i] - size[i-1]
}

#create a character vector with the clean names of each file according
#to the length of each file
Sources <- character()
for (i in 1:length(size)){
  Source <- rep(origClean[i], fileSize[i])
  Sources <- append(Sources,Source, after = length(Sources))
}

#bind it to the blogs dataframe
blogs$Source <- Sources


#write to .CSV
write.csv(blogs, file = "K:/Communispace Blog Rolls/MasterList/MasterBlogList.csv", 
          row.names = FALSE)