fileLoc <- "~/Desktop/Burning Glass/ProgramDirectors.csv"
PM <-read.csv(file=fileLoc, header=TRUE, sep=",", as.is = TRUE)

for (i in 2:ncol(cleanPM)) {
  PM[[i]] <- as.logical(as.character(PM[[i]]))
}

remove(PM)

for (i in 2:ncol(cleanPM)) {
  cleanPM[[i]] <- as.logical(as.character(cleanPM[[i]]))
}

for (i in 2:ncol(cleanPM)) {
  cleanPM[[i]] <- as.numeric(as.logical(cleanPM[[i]]))
}