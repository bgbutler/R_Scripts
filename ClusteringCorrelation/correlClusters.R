library(ggplot2)
library(plyr)
library(RODBC)
library(reshape2)
library(gdata)



params <- list("DidNotLogin", "UniqueLogins", "TotalLogins",
            "TotalContributions","TotalUniqueContributors", "Active", "Moderate", "Low", "Lurkers")
df <- list()
count <- length(params)

#set up the SQL connection
ch <- odbcConnect("ApprBudget")

for (i in 1:count){

  df[[i]]<- sqlQuery(ch, sprintf("SELECT Period,
                    [Education] as Education,
                    [Apparel & Footwear] as ApparelFootwear,
                    [Banking & Financial Services] as BankingFinSvc,
                    [Insurance] as Insurance,
                    [Transportation] as Transportation,
                    [Healthcare/Pharmaceuticals] as HealthcarePharma,
                    [Consumer Electronics] as ConsumerElectronics,
                    [Media] as Media,
                    [Publishing] as Publishing,
                    [Telecommunications] as Telecommunications,
                    [Food & Beverage] as FoodBeverage,
                    [Automotive] as Automotive,
                    [Home Improvement] as HomeImprovement,
                    [Other] as Other,
                    [Retail] as Retail,
                    [Consumer Packaged Goods] as CPG,
                    [Beauty] as Beauty,
                    [Hospitality & Travel] as HospitalityTravel,
                    [Technology] as Technology
                    FROM
                    (SELECT Period, Industry, %s
                    FROM BenchmarkingHistory ) bh
                    PIVOT
                    (
                    Avg(%s)
                    FOR Industry IN
                    ([Education],
                    [Apparel & Footwear],
                    [Banking & Financial Services],
                    [Insurance],
                    [Transportation],
                    [Healthcare/Pharmaceuticals],
                    [Consumer Electronics],
                    [Media],
                    [Publishing],
                    [Telecommunications],
                    [Food & Beverage],
                    [Automotive],
                    [Home Improvement],
                    [Other],
                    [Retail],
                    [Consumer Packaged Goods],
                    [Beauty],
                    [Hospitality & Travel],
                    [Technology])
                    )
                    AS pvt
                    ",params[i], params[i]))
  
#create dates from period
df[[i]]$Period <- as.character(df[[i]]$Period)
df[[i]]$Period <- as.Date(paste(df[[i]]$Period,"-01", sep ="")) 
}

odbcClose(ch)

#change the names of the list items
names(df) <- params


distance <- list()
for (j in 1:count) {
  distance[[j]] <- as.dist(1 - 
                             cov2cor(cov(df[[j]][,c(2:19)], 
                            method = "pearson", use = "pairwise.complete.obs")))}

#change the names of the list items
names(distance) <- params

#make the industry plots
for (p in 1:count){
  plot(hclust(distance[[p]]), hang = -1, main = params[p], xlab = "Industry")
}



### This is the end of the code


#this code plots a single cluster  
DidNotLoginCov <- cov(df$DidNotLogin[,c(2:19)], method = "pearson", use = "pairwise.complete.obs")
DidNotLoginCorr <- cov2cor(DidNotLoginCov)
disDNL <- 1 - cor(DidNotLoginCorr)
distance <- as.dist(disDNL)