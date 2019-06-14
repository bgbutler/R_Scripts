

library(ggplot2)
library(dplyr)
library(reshape2)



#get data
url <- "K:/Sandbox/R/Data IO/R_allDataRollupOct.csv"
dataRaw <- read.csv(url, header = TRUE, na.strings = c('', "NA"), sep=",", as.is = FALSE)



newNames <- c("SolCode", "Client", "Industry", "Project", "Position", "Role", 
              "Sr_Mgr", "AppBudget", "ContractSize","Consulting",
              "CR","Launch","LearnDes", "Ops","StoryTelling")

colnames(dataRaw) <- newNames

#Create Additional Columns
dataRaw$Role <- as.character(dataRaw$Role)

#create function to clean up the NAs
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

for (i in 10:15){
  dataRaw[,i] <- na.zero(dataRaw[,i])
}

na.zero(dataRaw$Role)


dataRaw$Data <- ifelse(dataRaw$Role %in% c("DA", "DM"),dataRaw$CR + dataRaw$Launch, 0)
dataRaw$CR <- ifelse(!(dataRaw$Role %in% c("DA", "DM")),dataRaw$CR, 0)
dataRaw$Launch <- ifelse(!(dataRaw$Role %in% c("DA", "DM")),dataRaw$Launch, 0)        


dataRaw$Role <- as.factor(dataRaw$Role)

data1 <- select(dataRaw, Client, Project, Position, AppBudget, ContractSize, Industry,
                Sr_Mgr, Consulting, CR, Launch, Ops, LearnDes,StoryTelling, Data, Role)


dataMelt <- melt(data1, 
                   id = c("Client", "Project", "Position",
                          "Sr_Mgr", "AppBudget", "ContractSize", "Industry", "Role"),
                 variable.name = "Task",
                 value.name = "Hours")

#create a dataframe containg just the clients and total hours
hoursClient <- group_by(dataMelt, Client)
hoursOnly <- summarise(hoursClient,
                       totHours = sum(Hours, na.rm = T))
byHours <- arrange(hoursOnly, desc(totHours))

######################################
by_ClientTask <- group_by(dataMelt, Client, Task, Industry)
ClientHrs <- summarise(by_ClientTask,
                       totHours = sum(Hours, na.rm = T))
###################################



arrange(ClientHrs, desc(totHours))


#################################
#old way of working with Citi and Walmart for scaling

#big2 <- subset(ClientHrs, (Client %in% c("Citibank", "Wal-Mart")))

#allOther <- subset(ClientHrs, !(Client %in% c("Citibank", "Wal-Mart")))

######################################



#merger allOther with the byHours dataframe
merged <- merge(ClientHrs, byHours, by.x = "Client", by.y = "Client", all.x = TRUE)

head(merged)

merged <- arrange(merged, desc(totHours.y))

merged$quintile <- with(merged, factor(
  findInterval(totHours.y,
               c(-Inf,quantile(totHours.y, probs=c(0.2, .4, .6, .8)), Inf)), 
  labels=c("Q1","Q2","Q3","Q4", "Q5")))



df <- list()
df[[1]] <- filter(merged, quintile =="Q1")
df[[2]] <- filter(merged, quintile =="Q2")
df[[3]] <- filter(merged, quintile =="Q3")
df[[4]] <- filter(merged, quintile =="Q4")
df[[5]] <- filter(merged, quintile =="Q5")
names(df) <- c("Q1", "Q2", "Q3", "Q4", "Q5")


#aggregate by role
by_IndustryRole <- group_by(dataMelt, Task, Position)
ClientHrs <- summarise(by_IndustryRole,
                       totHours = sum(Hours, na.rm = T))

#plot each of the quintiles
PP <- list()

for (i in 1:5){
  
  arrange(df[[i]], desc(totHours.y))
  
PP[[i]] <- ggplot(data = df[[i]], aes(x = reorder(Client, totHours.x), y = totHours.x, fill = Task)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  ggtitle(sprintf("Client Hours For Quintile %s Clients", i)) + 
  xlab("") +
  ylab("Hours") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
print(PP[[i]])
}

#plot by Task
P <- ggplot(ClientHrs, aes(x = reorder(Task, totHours), y = totHours, fill = Task)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  ggtitle("Total Client Hours by Task") + 
  xlab("") +
  ylab("Total Hours") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
P



#plot by Industry
P <- ggplot(merged, aes(x = reorder(Task, totHours.y), y = totHours.x, fill = Task)) + 
  geom_bar(stat = "identity") + coord_flip() + facet_wrap(~Industry, ncol = 4) +
  ggtitle("Total Task Hours by Industry") + 
  xlab("") +
  ylab("Total Hours") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
P


#aggregate by role
by_IndustryRole <- group_by(dataMelt, Task, Position)
ClientHrs <- summarise(by_IndustryRole,
                       totHours = sum(Hours, na.rm = T))

P <- ggplot(ClientHrs, aes(x = reorder(Position, totHours), y = totHours, fill = Task)) + 
  geom_bar(stat = "identity") + coord_flip() + facet_wrap(~Task) +
  ggtitle("Total Task Hours by Role") + 
  xlab("") +
  ylab("Total Hours") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
P





#aggregate by industry
by_IndustryRole <- group_by(dataMelt, Task, Industry)
industryHrs <- summarise(by_IndustryRole,
                       totHours = sum(Hours, na.rm = T))


P <- ggplot(data = industryHrs, aes(x = reorder(Industry, totHours), y = totHours, fill = Task)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  ggtitle("Client Hours by Industry Includes Walmart and Citi") + 
  xlab("") +
  ylab("Total Hours") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
print(P)


##############################################################
#subset dataRaw for Datateam
dataTeam <- subset(dataRaw, (Role %in% c("DA", "DM")))

data2 <- select(dataTeamDates, Industry, SolCode, Project, DA, DM, Status)

#aggregate by industry
dataMelt2 <- melt(data2, 
                 id = c("Industry", "SolCode", "Project", "Status"),
                 variable.name = "Role",
                 value.name = "Hours")



by_Role <- group_by(dataTeam, SolCode, Role)
dataHrs <- summarise(by_Role,
                         totHours = sum(Data, na.rm = T))




#plot the Da and DM distributions
P <- ggplot(dataMelt2, aes(x = Hours, fill = Role)) + 
  geom_density(position = "identity", alpha = .5) + facet_wrap(~Industry, ncol = 4) + xlim(0,150) +
  ggtitle("Distribution of DA and DM Hours") + 
  ylab("Frequency") +
  xlab("Total Hours") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
P


by_Role <- group_by(dataHrs, Role)
avgHrs <- summarise(by_Role,
                     avg = mean(totHours, na.rm = T),
                     median = median(totHours, na.rm = T),
                     stdev = sd(totHours, na.rm = T))
avgHrs

##########################################
#new plots by client and role and industry and role/level

data2 <- select(dataRaw, Client, Project, Position, Industry, Consulting, 
                CR, Launch, Ops, LearnDes,StoryTelling)

head(data2)

data2$CCSHrs <- data2$CR + data2$Launch + data2$Consulting + 
  data2$Ops + data2$LearnDes + data2$StoryTelling


dataMelt <- melt(data2, 
                 id = c("Client", "Project", "Position", "Industry"),
                 variable.name = "Task",
                 value.name = "Hours")

head(dataMelt)

#remove some of the extraneous positions
dataMelt <- dataMelt[!dataMelt$Position %in% c("ADMN", "DEV", "INN1", "SALS", "OP1","OP2","OP3","AC",
                                                                 "INN2", "INN3", "JRDR"),]


#create a dataframe containg just the clients and total hours
hoursClient <- group_by(dataMelt, Client)
hoursOnly <- summarise(hoursClient,
                       totHours = sum(Hours, na.rm = T))
byHours <- arrange(hoursOnly, desc(totHours))

head(byHours)

by_ClientPosition <- group_by(dataMelt, Client, Position, Industry)
positionHrs <- summarise(by_ClientPosition,
                       totHours = sum(Hours, na.rm = T))


arrange(positionHrs, desc(totHours))


#merger position with the byHours dataframe
positionMerged <- merge(positionHrs, byHours, by.x = "Client", by.y = "Client", all.x = TRUE)

head(positionMerged,10)

positionMerged <- arrange(positionMerged, desc(totHours.y))
head(positionMerged,30)


positionMerged$quintile <- with(positionMerged, factor(
  findInterval(totHours.y,
               c(-Inf,quantile(totHours.y, probs=c(0.2, .4, .6, .8)), Inf)), 
  labels=c("Q1","Q2","Q3","Q4", "Q5")))



df <- list()
df[[1]] <- filter(positionMerged, quintile =="Q1")
df[[2]] <- filter(positionMerged, quintile =="Q2")
df[[3]] <- filter(positionMerged, quintile =="Q3")
df[[4]] <- filter(positionMerged, quintile =="Q4")
df[[5]] <- filter(positionMerged, quintile =="Q5")
names(df) <- c("Q1", "Q2", "Q3", "Q4", "Q5")


#plot each of the quintiles

PP <- list()

for (i in 1:5){
  
  arrange(df[[i]], desc(totHours.y))
  
  PP[[i]] <- ggplot(data = df[[i]], aes(x = reorder(Client, totHours.y), y = totHours.x, fill = Position)) + 
    geom_bar(stat = "identity") + coord_flip() + scale_fill_brewer(palette = "Spectral") +
    ggtitle(sprintf("Client Position Hours For Quintile %s Clients", i)) + 
    xlab("") +
    ylab("Hours") + 
    theme(plot.title = element_text(face = "italic", color = "blue"),
          axis.title.x=element_text(color = "blue"),
          axis.title.y=element_text(color = "blue"),
          axis.text.x=element_text(color = "red"),
          axis.text.y=element_text(color = "red"))
  print(PP[[i]])
}


#aggregate by industry
by_IndustryPosition <- group_by(dataMelt, Position, Industry)
industryHrs <- summarise(by_IndustryPosition,
                         totHours = sum(Hours, na.rm = T))


P <- ggplot(data = industryHrs, aes(x = reorder(Industry, totHours), y = totHours, fill = Position)) + 
  geom_bar(stat = "identity") + coord_flip() + scale_fill_brewer(palette = "Spectral") +
  ggtitle("Industry Hours by Position") + 
  xlab("") +
  ylab("Total Hours") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
print(P)


##############################
#aggregate by level 1, 2, 3, etc.

#create the level field for aggregation
positionMerged$Position <- as.character(positionMerged$Position)

positionMerged$Level <- substr(positionMerged$Position,
                               start = nchar(positionMerged$Position),
                               stop = nchar(positionMerged$Position))

head(positionMerged)




#resplit the data and make the lists
positionMerged$Position <- ordered(positionMerged$Position)
positionMerged$Level <- ordered(positionMerged$Level)

#remove the position
positionMerged$Position <- NULL


#aggregate by industry
by_IndustryLevel <- group_by(positionMerged, Client, Industry, Level, totHours.y, quintile)
levelHrs <- summarise(by_IndustryLevel,
                         totHours = sum(totHours.x, na.rm = T))
head(levelHrs)

levelHrs <- arrange(levelHrs, desc(totHours.y))


df <- list()
df[[1]] <- filter(levelHrs, quintile =="Q1")
df[[2]] <- filter(levelHrs, quintile =="Q2")
df[[3]] <- filter(levelHrs, quintile =="Q3")
df[[4]] <- filter(levelHrs, quintile =="Q4")
df[[5]] <- filter(levelHrs, quintile =="Q5")
names(df) <- c("Q1", "Q2", "Q3", "Q4", "Q5")



#plot each of the quintiles

PP <- list()

for (i in 1:5){
  
  arrange(df[[i]], desc(totHours.y))
  
  PP[[i]] <- ggplot(data = df[[i]], aes(x = reorder(Client, totHours.y), y = totHours, fill = Level)) + 
    geom_bar(stat = "identity") + coord_flip() + scale_fill_brewer(palette = "Spectral") +
    ggtitle(sprintf("Client Hours by Level for Quintile %s Clients", i)) + 
    xlab("") +
    ylab("Hours") + 
    theme(plot.title = element_text(face = "italic", color = "blue"),
          axis.title.x=element_text(color = "blue"),
          axis.title.y=element_text(color = "blue"),
          axis.text.x=element_text(color = "red"),
          axis.text.y=element_text(color = "red"))
  print(PP[[i]])
}

#aggregate by industry
by_IndustryLevel <- group_by(levelHrs, Level, Industry)
industryHrs <- summarise(by_IndustryLevel,
                         totHours = sum(totHours, na.rm = T))
head(industryHrs)

P <- ggplot(data = industryHrs, aes(x = reorder(Industry, totHours), y = totHours, fill = Level)) + 
  geom_bar(stat = "identity") + coord_flip() + scale_fill_brewer(palette = "Spectral") +
  ggtitle("Industry Hours by Level") + 
  xlab("") +
  ylab("Total Hours") + 
  theme(plot.title = element_text(face = "italic", color = "blue"),
        axis.title.x=element_text(color = "blue"),
        axis.title.y=element_text(color = "blue"),
        axis.text.x=element_text(color = "red"),
        axis.text.y=element_text(color = "red"))
print(P)

