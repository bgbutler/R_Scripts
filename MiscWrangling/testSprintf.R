library(ggplot2)
library(plyr)
library(RODBC)
library(reshape2)
library(gdata)



params <- list("DidNotLogin", "UniqueLogin", "TotalLogins",
               "TotalContributions","TotalUniqueContributors")
df <- list()
count <- length(params)

for (i in 1:count){
df[[i]] <- sprintf("SELECT Period,
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
        ",params[i], params[i])

 df[[i]]
 }