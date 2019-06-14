#this is for an all community analysis of ramp-up
#load up a bunch of libraries

library(ggplot2)
library(plyr)
library(RODBC)
library(RColorBrewer)

#get the list of industry names
#set up the SQL connection
ch <- odbcConnect("ApprBudget")
industryNames <- sqlQuery(ch, "SELECT DiSTINCT Industry from BenchmarkingHistory
                          WHERE Solomon_Office like '%Boston%'")
odbcClose(ch)

industryNames$Industry <- as.character(industryNames$Industry)
count <- length(industryNames$Industry)

params <- industryNames$Industry
df <- list()

#set up the SQL connection
ch <- odbcConnect("ApprBudget")

for (i in 1:count){
  df[[i]] <- sqlQuery(ch, sprintf("SELECT CommunityGUID, CommunityName, Industry, avg(ParticipationPercentageActual) as Participation,
                        MonthsActive, TotalUniqueContributors as Contributors, ClientName,
                        TotalContributions as Vibrancy,
                        Coalesce(CommunityName,'') + '-' + Coalesce(ClientName,'') as CommClient
                        FROM BenchmarkingHistory
                        WHERE Industry = '%s'
                        AND Solomon_Office = 'Boston' 
                        GROUP BY CommunityGUID, CommunityName, MonthsActive, Industry, ClientName, TotalContributions,
                        TotalUniqueContributors, ClientName
                        ORDER BY MonthsActive Asc", params[i]))

}    

odbcClose(ch)
                      
 #change the names of the list items
  names(df) <- params
 
  
PDFpath <- "Q:/DataScience/ParticipationPlots/"
#clean up industry names
params <- sub("/", " & ", params)

#for (z in 1:19){
#myPath <- file.path("Q:","DataScience", "ParticipationPlots", paste(params[z], ".pdf", sep=""))   
#print(myPath)
#}



 gg <- list()
 #make the plots, facet by client on each page - works well
  for (p in 1:length(df)){
    
    myPath <- file.path("Q:","Analytics", "ParticipationPlots", paste(params[p], ".pdf", sep="")) 
    
    pdf(file = myPath, onefile = F, paper = "USr", width = 11, height = 8.5)
    
    gg[[p]] <- ggplot(data = df[[p]],  aes(x = MonthsActive, y = Participation, color = CommClient)) + 
      ylim(0,1) + geom_line(size = 0.8) + 
      scale_x_continuous(limits = c(1,13)) + 
      facet_wrap(~ClientName, scales="fixed") + 
      scale_color_hue(l = 45) + 
      ggtitle(sprintf("Participation Rate for %s for First Year",params[p]))
    print(gg[[p]])
    dev.off()
    }


 
 
 
 
 
 
 
 
 dev.off()
 
 myPath <- file.path("Q:","DataScience", "ParticipationPlots", paste(params[p], ".pdf", sep="")) 
 
 pdf(file = myPath, onefile = F, paper = "USr", width = 11, height = 8.5)
 
 dev.off() 

for (x in 1:length(df)){
pdf(paste(params[p], ".pdf", sep=""), 
    onefile = F, paper = "USr", width = 11, height = 8.5)
  }
 dev.off() 