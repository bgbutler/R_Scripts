

#load in the Excel files
library(readxl)
library(openxlsx)
library(ggplot2)
library(gridExtra)
library(plyr)

#loop through all files and combine
#start with the .xlsx files then the .xls files
Act <- data.frame()

#read the .xlsx files
url <- "K:/Sandbox/Bryan Projects/Activity Reports/"
files <- list.files(url, full.names = TRUE)
count <- length(files)

for (i in 1:count){
  Act <- rbind(Act, 
               read.xlsx(files[i], sheet = 2, startRow = 2, colNames = T))
  print(files[i])
}

#get a list of the .xls files
url <- "K:/Sandbox/Bryan Projects/Abbvie/"
files <- list.files(url, full.names = TRUE)
count <- length(files)

Act2 <- data.frame()

#read the .xls files in
for (i in 1:count){
  Act2 <- rbind(Act2, 
                      read_excel(files[i], sheet = 2, col_names = T,
                                 col_types = NULL, na = "", skip = 1))
  print(files[i])
}



#change column names to be cleaner for processing
newNames <- c("Level", "Title", "InvitedGroups", "StartDate", "EndDate", "AuthFirstName",
              "AuthLastName", "AuthRole", "ActivityType", "FirstName", "LastName", 
              "ContributorRole", "Contributions", "TotContributors")

#change the names of the columns
colnames(Act) <- newNames
colnames(Act2) <- newNames

Act <- rbind(Act, Act2)

#remove Act2
rm(Act2)

#set up factor variables
Act$Level <- as.factor(Act$Level)
Act$Title <- as.factor(Act$Title)
Act$InvitedGroups <- as.factor(Act$InvitedGroups)
Act$ActivityType <- as.factor(Act$ActivityType)
Act$AuthRole <- as.factor(Act$AuthRole)
Act$ContributorRole <- as.factor(Act$ContributorRole)

#set up dates and create duration
Act$StartDate <- as.Date(Act$StartDate, format="%m/%d/%Y")
Act$EndDate <- as.Date(Act$EndDate, format="%m/%d/%Y")
Act$Duration <- Act$EndDate - Act$StartDate
Act$Duration <- as.numeric(Act$Duration)


#separate the summary from the data and remove it
actSum <- subset(Act, TotContributors != 'NA')
Act <- Act[Act$Level != 'Activity',]



#plot the data
actSumP <- ggplot(actSum, aes(x =Contributions, fill = AuthRole))
actSumP + geom_density() + facet_wrap(~ActivityType, ncol = 3) +
  ggtitle("Distribution of Contributions by Activity Type") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

actSumP <- ggplot(actSum, aes(x =Contributions, fill = AuthRole))
actSumP + geom_histogram(binwidth = 1) + facet_wrap(~ActivityType, ncol = 3) +
  ggtitle("Distribution of Contributions by Activity Type") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

actSumP <- ggplot(actSum, aes(x =Duration, fill = AuthRole))
actSumP + geom_histogram(binwidth = 1) + facet_wrap(~ActivityType, ncol = 3) + xlim(1,35) + 
  ggtitle("Distribution of Duration by Activity Type") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

actSumP <- ggplot(actSum, aes(x =Contributions, fill = ActivityType))
actSumP + geom_histogram(binwidth = 1) + facet_wrap(~InvitedGroups, ncol = 3) +
  ggtitle("Distribution of Contributions by Invited Groups") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

actSumP <- ggplot(actSum, aes(x =Contributions))
actSumP + geom_histogram(binwidth = 1, fill = "red") + facet_wrap(~ActivityType, ncol = 3) +
  ggtitle("Distribution of Contributions by Activity Type") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))



#alternate way to plot across multiple pages
P <- ggplot(actSum, aes(x =Contributions, color = ActivityType)) + 
  geom_histogram(binwidth = 1) + 
  ggtitle("Distribution of Contributions by Invited Groups") + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))

plots = dlply(actSum, "InvitedGroups", `%+%`, e1 = P)
ml <- do.call(marrangeGrob, c(plots.list, list(nrow=3, ncol = 3)))
plot("multipage.pdf", ml)
  

pdf('test3.pdf', width=21, height=27)
i = 1
plot = list() 
for (n in unique(Act$InvitedGroup)){
  ### process data for plotting here ####
  plot[[i]] = ggplot(actSum, aes(x =Contributions, color = ActivityType)) + 
    geom_histogram(binwidth = 1) + facet_wrap(~InvitedGroups, nrow = 2, ncol = 26) + 
    ggtitle("Distribution of Contributions by Invited Groups") + 
    theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))
  if (i %% 8 ==0) {
    print(do.call(grid.arrange, plot))
    plot = list()
    i = 0
  }
  i = i + 1
}
if (length(plot) !=0) {
  print(do.call(grid.arrange,plot))
}
dev.off()




#alternative method
noPlots <- 4
allVars <- unique(actSum$InvitedGroups)
noVars <- length(allVars)

# indices for plotting variables
plotSequence <- c(seq(0, noVars-1, by = noPlots), noVars)

pdf("test4.pdf") 
# loop over the variables to plot
for(ii in 2:length(plotSequence)){
  # select start and end of variables to plot
  start <- plotSequence[ii-1] + 1
  end <- plotSequence[ii]
    
  tmp <- subset(actSum, InvitedGroups %in% allVars[start:end])
  cat(unique(tmp$InvitedGroups), "\n")
  
  P <- ggplot(actSum, aes(x =Contributions, color = ActivityType)) + 
    geom_histogram(binwidth = 1) + facet_wrap(~InvitedGroups, ncol =2, nrow = 26) + 
    ggtitle("Distribution of Contributions by Invited Groups") + 
    theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue"))
  print(P)
}
dev.off()
  