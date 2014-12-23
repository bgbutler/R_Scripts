# Get the AMO data and store it as a .CSV file
# This is the raw format
fileLoc <- "~/Desktop/Burning Glass/ITSampleClean.csv"

#Get Supply Demand Files
StateLoc <- "~/Downloads/StateCSV.csv"
CountyLoc <- "~/Desktop/Burning Glass/CountyCSV.csv"




#Read only the data there are no column names in the data
ITData <-read.csv(file=fileLoc, header=TRUE, sep=",", as.is = TRUE)
head(ITData)

StateData <-read.csv(file=StateLoc, header=TRUE, sep=",")
head(StateData)

CountyData <-read.csv(file=CountyLoc, header=TRUE, sep=",")
head(CountyData)


StateSub <- subset(ITData, select = c(TTF,State, MSAName, Skills))
cleanStateSub <- na.omit(StateSub)

plot(CountyData$CountOfTTF, CountyData$AvgOfTTF, xlab = "Count of Postings", ylab = "Avg TTF", main = "Supply of Jobs and Avg Time to Fill (TTF)")



ITSkills <- ITData[,c(22:221)]
head(ITSkills)

SkillsNum <- ifelse(ITSkillsNum =="TRUE",1,0 )

test <-as.data.frame(ifelse(ITSkillsNum =="TRUE",1,0 ))

ITSkillsNum <- cleanITSkills[,c(3:200)]

cleanNewData$Count <- as.numeric(as.character(cleanNewData$Count))


testClean <-test

for (i in 1:ncol(testClean)) {
  testClean[[i]] <- as.numeric(as.character(testClean[[i]]))
}

skillsModel2 <- lm(TTF ~ ., data = testClean)