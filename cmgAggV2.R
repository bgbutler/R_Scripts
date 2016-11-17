#script to analyze CMG portfolio data

.libPaths(c("C:\\Users\\n846490\\Documents\\R", .libPaths()))

library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)
library(scales)
library(leaflet)
library(RColorBrewer)
library(htmltools)
library(jsonlite)





#get the new data file it is much larger
url <- "//STATESFPSOP1/apps/RASHD/Projects/RAnalyses/CMGAnalysisData.csv"
cmgAll <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)



#convert to the dates to ordered factor by month
cmgAll$Month2016 <- ifelse(cmgAll$Month == '31JAN2016', "Jan",
                                ifelse(cmgAll$Month == '29FEB2016', "Feb",
                                ifelse(cmgAll$Month == '31MAR2016', "Mar",
                                ifelse(cmgAll$Month == '30APR2016', "Apr",
                                ifelse(cmgAll$Month == '31MAY2016', "May",
                                ifelse(cmgAll$Month == '30JUN2016', "Jun", "Jul"))))))

cmgAll$Month2016 <- ordered(cmgAll$Month2016, levels = c("Jan", "Feb", "Mar", 
                                                          "Apr", "May", "Jun", "Jul"))

table(cmgAll$Month2016)

#create customer duration
#first make factor a date
sum(cmgAll$FirstRelDate == "")
sum(cmgAll$CleanDate == "")


cmgAll$FirstRelDate <- as.character(cmgAll$FirstRelDate)
cmgAll$CleanDate <- ifelse(cmgAll$FirstRelDate == "", "31May2012", cmgAll$FirstRelDate)
cmgAll$CleanDate <- as.Date(cmgAll$CleanDate, format = "%d%b%Y")

cmgAll$Today <- rep("2016-07-31", length(cmgAll$CleanDate))
cmgAll$Today <- as.Date(cmgAll$Today, format = "%Y-%m-%d")

cmgAll$DurationYrs <- (as.numeric(cmgAll$Today - cmgAll$CleanDate))/365


#reset the tenure range
cmgAll$Tenure <- ifelse(cmgAll$Duration < 1.5001, "<1.5 Yrs", "> 1.5 Yrs")
cmgAll$Tenure <-  as.factor(cmgAll$Tenure)


######################################################
#create a count of products
#subset by products
cmgProducts <- cmgAll %>% 
    filter(Month2016 == "Jan") %>%
    select(Portfolio, 
           Own_CCrd,
           Own_CD,
           Own_Chk,
           Own_DDep30,
           Own_HE,
           Own_LN,
           Own_Loc,
           Own_Mma,
           Own_Mort,
           Own_Sav,
           Own_Inv,
           bald.i,
           Month2016)

#use the magritter function to simplify this for counting account penetration
#get the sum value for each column by HHID

cmg <- length(which(cmgProducts$Portfolio == "CMG"))
premier <- length(which(cmgProducts$Portfolio == "Premier"))
non <- length(which(cmgProducts$Portfolio == "NonPortfolio"))
select <- length(which(cmgProducts$Portfolio == "Select"))


allProducts <- cmgProducts %>% group_by(Portfolio) %>% 
                         summarise(
                         CC = sum(Own_CCrd),
                         CD = sum(Own_CD),
                         Chk = sum(Own_Chk),
                         DDep = sum(Own_DDep30),
                         HE = sum(Own_HE),
                         LN = sum(Own_LN),
                         Loc = sum(Own_Loc),
                         MM = sum(Own_Mma),
                         Mort = sum(Own_Mort),
                         Sav = sum(Own_Sav),
                         Inv = sum(Own_Inv),
                         Bal = max(bald.i))


#sum all products to do a scatter plot
cmgProducts$ProdCounts <- cmgProducts$Own_CCrd +
        cmgProducts$Own_CD +
        cmgProducts$Own_Chk +
        cmgProducts$Own_DDep30 + 
        cmgProducts$Own_HE + 
        cmgProducts$Own_LN + 
        cmgProducts$Own_Loc + 
        cmgProducts$Own_Mma + 
        cmgProducts$Own_Mort + 
        cmgProducts$Own_Sav + 
        cmgProducts$Own_Inv 


allProductsRollup <- cmgProducts %>% group_by(Portfolio) %>% 
    summarise(
        CC = sum(Own_CCrd),
        CD = sum(Own_CD),




#make a quadrant scatter plot

g <- ggplot(cmgProducts, aes(x = ProdCounts, y = bald.i, color = Portfolio)) + 
    geom_point(alpha = .5) + scale_y_continuous(labels = comma, limits = c(0,750000)) + 
    scale_x_discrete(limits = c(1,2,3,4,5,6,7,8,9,10)) + 
    facet_wrap(~Portfolio) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g

xlim(0,60) 

geom_smooth(size = 2) +

################################################################################3



#calculate percentage
allProducts$Totals <- ifelse(allProducts$Portfolio == "CMG", cmg, 
                            ifelse(allProducts$Portfolio == "Select", 
                                   select, ifelse(allProducts$Portfolio == "Premier", premier,
                                                  non)))

#melt products
meltProducts <- melt(allProducts, id = c("Portfolio", "Totals"),
                    variable.name = "Account",
                    value.name = "Count")


meltProducts$Percent <- meltProducts$Count/meltProducts$Totals



#this is the bar plot of product ownership
g <- ggplot(meltProducts, aes(x = reorder(Account, Percent), y = Percent, fill = Account)) + 
    geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) + 
    facet_wrap(~Portfolio) + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g





#replace Products with new sum
#sum across to see how mnay months each HHID is in the portfolio
allProducts$Products <- allProducts$CC + allProducts$CD + allProducts$Chk + allProducts$DDep +
    allProducts$HE + allProducts$LN + allProducts$Loc + allProducts$MM + 
    allProducts$Mort + allProducts$Sav + allProducts$Inv


allProducts$Products <- as.numeric(allProducts$Products)
allProducts$HHID <- as.character(allProducts$HHID)

rm(by_HHID)

#this is the histogram of product ownership
g <- ggplot(allProducts, aes(x = Products, fill = Portfolio)) + facet_wrap(~Portfolio) +
    geom_histogram(binwidth = 1) + scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10,11)) +
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


#this is the density of product ownership
g <- ggplot(prodTotal, aes(x = Portfolio, y = Products, fill = Portfolio)) + 
    geom_boxplot(notch=TRUE) + scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) + 
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


prodTotal <- data.frame(allProducts$Portfolio, allProducts$Products)
colnames(prodTotal) <- c("Portfolio", "Products")

#try melting and plotting
JanMelt$Product <- ifelse(JanMelt$Balance > 0, 1, 0)


acctTotals <- JanMelt %>%
    group_by(Portfolio, Account) %>%
    summarise(Value = sum(Product))



g <- ggplot(acctTotals, aes(x = reorder(Account, Value), y = Value, fill = Account)) + 
    geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) + 
    facet_wrap(~Portfolio) + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g




#make more plots using ecd function
g1 <- ggplot(allProducts, aes(x=Products, color = Portfolio)) + 
    stat_ecdf(size = 2) + xlim(0,7) + ylab("Cum Probability") + 
    theme(legend.position = c(0.8, 0.2))
g1 + ggtitle("Empirical Cumulative Distribution of Product Ownership") + 
    theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "red"))
g1

#remove  the extra data CcmgProducts
cmgProducts <- NULL


#index the data for sampling
rndid <- with(allProducts, ave(HHID, Portfolio, FUN = function(x) {sample.int(length(x))}))
ProductsRed <- allProducts[rndid<=80000,]
table(ProductsRed$Portfolio)


meltProducts <- melt(ProductsRed, id= c("HHID", "Portfolio"),
                      variable.name = "Account",
                      value.name = "Value")
                     
meltProducts <- filter(meltProducts, Account != "Products" & Account != "LN" & Account != "Loc")


g <- ggplot(meltProducts, aes(x = Value, color = Portfolio)) + stat_ecdf(size = 2) + 
    facet_wrap(~Account) + xlim(0,7) + ylab("Cum Probability") + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    ggtitle("Distribution of Products Owned By Portfolio") + 
    theme(plot.title = element_text(color = "red"))
g


g <- ggplot(meltProducts, aes(x = Value, fill = Portfolio)) + geom_histogram(binwidth = 1) + 
    facet_wrap(~Account) + xlim(0,2) + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    ggtitle("Distribution of Products Owned By Portfolio") + 
    theme(plot.title = element_text(color = "red"))
g


#################################################################################

#get the max value for each column by HHID
by_HHID <- group_by(cmgProducts, Portfolio, HHID)


#make a bar plot to summrize the products
allProducts <- cmgProducts %>% 
                    summarise(by_HHID,
                    CC = max(Own_CCrd),
                    CD = max(Own_CD),
                    Chk = max(Own_Chk),
                    DDep = max(Own_DDep30),
                    HE = max(Own_HE),
                    LN = max(Own_LN),
                    Loc = max(Own_Loc),
                    MM = max(Own_Mma),
                    Mort = max(Own_Mort),
                    Sav = max(Own_Sav),
                    Inv = max(Own_Inv))


cmgBal <- cmgAll %>% 
    filter(Month2016 == "Jan") %>%
    select(Portfolio, 
           HHID, 
           balchk, 
           balsav, 
           balmma, 
           balcd, 
           balmort, 
           balhe, 
           balloc, 
           balinv,
           DurationYrs,
           TenureRange,
           Month2016)





#complete with product analysis
#############################################################
#remove unneeded dfs
allProducts <- NULL
melt <- NULL
meltProducts <- NULL
ProductsRed <- NULL


#remove many extraneous columns from previous analysis
cmgAll <- cmgAll[, -c(27:36)]

#perform additional cleanup
cmgAll <- cmgAll[, -c(32, 33, 34,36,37,41)]
######################################################
#get the month data and make a table
months <- select(cmgCombined, HHID, CustType, Month)

#widen the data
months$Jan <- ifelse(months$Month == '31JAN2016', 1, 0)
months$Feb <- ifelse(months$Month == '29FEB2016', 1, 0)
months$Mar <- ifelse(months$Month == '31MAR2016', 1, 0)
months$Apr <- ifelse(months$Month == '30APR2016', 1, 0)
months$May <- ifelse(months$Month == '31MAY2016', 1, 0)
months$Jun <- ifelse(months$Month == '30JUN2016', 1, 0)
months$Jul <- ifelse(months$Month == '31JUL2016', 1, 0)

monthsAll <- select(months, HHID, CustType, Jan, Feb, Mar, Apr, May, Jun, Jul)
monthsAll$HHID <- as.factor(as.character(monthsAll$HHID))


cmgCombined$BalSant <- cmgCombined$baldeposits + cmgCombined$balinv

#get the max value for each column by HHID
monthsAllSum <- aggregate(. ~ HHID, data = monthsAll, FUN = max)

#sum across to see how mnay months each HHID is in the portfolio
monthsAllSum$MonthsIn <- apply(monthsAllSum[,c(3:9)], 1, sum)

# Write the data to clean .CSV file
write.csv(monthsAllSum, file="//STATESFPSOP1/apps/RASHD/Projects/RAnalyses/HHIDbyMonth.csv", row.names=FALSE)

#get only the HHIDs that last all 7 months
HHIDAllMonths <- filter(monthsAllSum, MonthsIn == 7)


# Write the data to clean .CSV file
write.csv(HHIDAllMonths, file="//STATESFPSOP1/apps/RASHD/Projects/RAnalyses/HHIDallMonths.csv", row.names=FALSE)


# Write the combined set
write.csv(cmgCombined, file="//STATESFPSOP1/apps/RASHD/Projects/RAnalyses/cmgCombinedv3.csv", row.names=FALSE)

###########################################################
#begin analysis of the balances


#subset the balance data as above
cmgBal <- cmgAll %>% 
            filter(Month2016 == "Jan") %>%
                    select(Portfolio, 
                           HHID, 
                           balchk, 
                           balsav, 
                           balmma, 
                           balcd, 
                           balmort, 
                           balhe, 
                           balloc, 
                           balinv,
                           DurationYrs,
                           TenureRange,
                           Month2016)


#get the max value for each column by HHID
#data has been cut by month so no need to max
#melt the data to plot
JanMelt <- melt(cmgBal, id = c("Portfolio", "HHID",
                               "Month2016", "TenureRange", "DurationYrs"),
                 variable.name = "Account",
                 value.name = "Balance")

JanMelt$HHID <- as.character(JanMelt$HHID)

#get a distribution of the months

g <- ggplot(JanMelt, aes(x = DurationYrs, fill = Portfolio)) + facet_wrap(~Portfolio, ncol = 2) +
    geom_histogram(bins = 50) + xlim(0,50) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


g <- ggplot(JanMelt, aes(x = DurationYrs, fill = Portfolio)) + facet_wrap(~Portfolio, ncol = 2) +
    geom_density() + xlim(0,50) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


#filter out mort and loc
JanMelt <- filter(JanMelt, Account != "balmort" & Account != "balloc")


deposits <- filter(JanMelt, Account %in% c("balchk", "balsav", "balmma"))
investments <- filter(JanMelt, Account %in% c("balcd", "balhe", "balinv"))


g <- ggplot(deposits, aes(x = Balance, fill = Portfolio)) + facet_wrap(~Account) +
    geom_histogram(binwidth = 1000, alpha = .8) + xlim(0,50000) + ylim(0,5000) + 
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


g <- ggplot(investments, aes(x = Balance, fill = Portfolio)) + facet_wrap(~Account) +
    geom_histogram(binwidth = 1000, alpha = .8) + xlim(0,50000) + ylim(0,1000) + 
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g



####################################################
#####Repeat for July


#subset the balance data as above
cmgBal <- cmgAll %>% 
    filter(Month2016 == "Jul") %>%
    select(Portfolio, 
           HHID, 
           balchk, 
           balsav, 
           balmma, 
           balcd, 
           balmort, 
           balhe, 
           balloc, 
           balinv,
           DurationYrs,
           TenureRange,
           Month2016)


#get the max value for each column by HHID
#data has been cut by month so no need to max
#melt the data to plot


JulMelt <- melt(cmgBal, id = c("Portfolio", "HHID",
                               "Month2016", "TenureRange", "DurationYrs"),
                variable.name = "Account",
                value.name = "Balance")

JulMelt$HHID <- as.character(JulMelt$HHID)


#get a distribution of the months

g <- ggplot(JulMelt, aes(x = DurationYrs, fill = Portfolio)) + facet_wrap(~Portfolio, ncol = 2) +
    geom_histogram(bins = 50) + xlim(0,50) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


g <- ggplot(JulMelt, aes(x = DurationYrs, fill = Portfolio)) + facet_wrap(~Portfolio, ncol = 2) +
    geom_density() + xlim(0,50) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


#filter out mort and loc
JulMelt <- filter(JulMelt, Account != "balmort" & Account != "balloc")


deposits <- filter(JulMelt, Account %in% c("balchk", "balsav", "balmma"))
investments <- filter(JulMelt, Account %in% c("balcd", "balhe", "balinv"))


g <- ggplot(deposits, aes(x = Balance, fill = Portfolio)) + facet_wrap(~Account) +
    geom_histogram(binwidth = 1000, alpha = .8) + xlim(0,50000) + ylim(0,5000) + 
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


g <- ggplot(investments, aes(x = Balance, fill = Portfolio)) + facet_wrap(~Account) +
    geom_histogram(binwidth = 1000, alpha = .8) + xlim(0,50000) + ylim(0,1000) + 
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


################################################################
#scatter plots & other interesting plots


#boxplot
g <- ggplot(JulMelt, aes(x = Portfolio, y = DurationYrs, fill = Portfolio)) +
    geom_boxplot(notch=TRUE) + ylim(0,60) + 
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
g


#boxplot
#checking balance only for july
checking <- filter(deposits, Account == "balchk")


g <- ggplot(checking, aes(x = Portfolio, y = Balance, fill = Portfolio)) + 
    geom_boxplot(notch=TRUE) + ylim(0,15000) + 
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
g



#check for january start
#later we create a dataset for jul to make some comparisons
janChecking <- filter(JanMelt, Account %in% c("balchk"))

g <- ggplot(janChecking, aes(x = Portfolio, y = Balance, fill = Portfolio)) + 
    geom_boxplot(notch=TRUE) + ylim(0,15000) +
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
    legend.position = "bottom",
    axis.text.y = element_text(color = "red"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank())
g


pal <- c("blue", "red", "yellow")

#scatter plots
g <- ggplot(deposits, aes(x = DurationYrs, y = Balance, color = Portfolio)) + 
    geom_point(alpha = .25) + facet_wrap(~Account) + xlim(0,60) + scale_y_continuous(labels = comma, limits = c(0,750000)) +
    geom_smooth(size = 2) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


g <- ggplot(investments, aes(x = DurationYrs, y = Balance, color = Portfolio)) + 
    geom_point(alpha = .4) + facet_wrap(~Account) + xlim(0,60) + scale_y_continuous(labels = comma, limits = c(0,750000)) +
    geom_smooth() +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g



g <- ggplot(cmgBal, aes(x = balhe, y = balinv, color = Portfolio)) + 
    geom_point(alpha = 0.2) + facet_wrap(~Portfolio) + 
    scale_x_continuous(labels = comma, limits = c(0,250000))  + scale_y_continuous(labels = comma, limits = c(0,1000000)) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red")) + 
    geom_smooth()
g


###############################################################################
#last group of analysis
#compare the construction of the portfolios
#are they random or deliberate
#subset the balance data as above
#get the columns and filter it for the month of jan
##############################################################################
BalCompare <- cmgAll %>%
                filter(Month2016 == "Jan") %>%
                select(Portfolio,
                       baldeposits, 
                       balinv,
                       Month2016)

#check the correlations with scatter plots
g <- ggplot(BalCompare, aes(x = baldeposits, y = balinv, color = Portfolio)) + 
    geom_point(alpha = .4) + facet_wrap(~Portfolio) + scale_x_continuous(labels = comma, limits = c(0,1000000)) + 
    scale_y_continuous(labels = comma, limits = c(0,1000000)) + 
    geom_smooth(color = "darkblue", size=1.25) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 10, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g


cmgBal <- cmgAll %>% 
    filter(Month2016 == "Jul") %>%
    select(Portfolio, 
           balsav, 
           balmma, 
           balcd, 
           balmort, 
           balhe, 
           balinv,
           Month2016)

ggpairs(cmgBal[-7], color = "Portfolio", alpha = 0.4)


pairs(cmgBal[2:7])

############################################################################
#attrition analysis
#check on those that left the bank
#############################################
leftBank <- filter(cmgAll, LeftBank == 1)

g3 <- ggplot(leftBank, aes(x = balchk, fill = Portfolio)) + facet_wrap(~Month2016) +
    geom_histogram(bins = 100, alpha = 0.8, na.rm = T) + scale_x_continuous(labels = comma, limits = c(0,300000)) +
    scale_y_continuous(limits = c(0,10)) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 14, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g3


g4 <- ggplot(leftBank, aes(x = balchk, fill = Month2016)) + facet_wrap(~Portfolio) +
    geom_histogram(bins = 100, alpha = 0.8, na.rm = T) + ylim(0,400) + scale_x_continuous(labels = comma, limits = c(0,30000)) +
    theme(axis.text.x = element_text(size = 8, color = "red", angle = 45),
          legend.position = "right",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 14, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g4


g5 <- ggplot(leftBank, aes(x = DurationYrs, fill = Portfolio)) + 
    geom_density(alpha = 0.8, na.rm = T) + 
    facet_wrap(~Portfolio) + 
    scale_x_continuous(limits = c(0,50)) +
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 14, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g5



g5 <- ggplot(leftBank, aes(x = Portfolio, y = DurationYrs, fill = Portfolio)) + 
    geom_boxplot(notch = TRUE) + 
    scale_y_continuous(limits = c(0,50)) +
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 14, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g5


rm(leftBank)



#plot bald.i by portfolio
di <- filter(cmgAll, Month2016 == "Jul")


di <- select(cmgAll, Month2016, Portfolio, bald.i)


cmgAll$Portfolio <- ordered(cmgAll$Portfolio, levels = c("CMG", "Premier", "Select", "NonPortfolio"))


g5 <- ggplot(di, aes(x = bald.i, fill = Portfolio)) + 
    geom_density(alpha = 0.8, na.rm = T) + 
    facet_wrap(~Portfolio) + 
    scale_x_continuous(labels = comma, limits = c(0,1000000)) +
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.text.x = element_text(size = 14, face = "bold"),
          strip.background = element_rect(fill = "red")) +
    theme(plot.title = element_text(color = "red"))
g5





#boxplot of d&I over the time period
g <- ggplot(di, aes(x = Month2016, y = bald.i, fill = Month2016)) + 
    geom_boxplot(notch=TRUE) + 
    stat_summary(fun.y="mean", geom="point", shape = 23, size = 3, fill = "white") + 
    facet_wrap(~Portfolio) + scale_y_continuous(labels = comma, limits = c(0,200000)) + 
    theme(axis.text.x = element_text(size = 8, color = "red"),
          legend.position = "bottom",
          axis.text.y = element_text(color = "red"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
g



#############################################
#map locations of the portfolios
#load the branch location data
############################################

#get the files  with location
url <- "C:/Users/n846490/Documents/DataScience/CSVs/branches.csv"
branchLocs <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#get the files  with june perf
url <- "C:/Users/n846490/Documents/DataScience/CSVs/juneBranchPerf.csv"
branchPerf <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)


#get key data from branchlocs
branchLocs <- select(branchLocs, Branch.Name, Cost.Center, Latitude, Longitude)


#clean the names
names(branchLocs) <- c("BranchName", "BranchNumber", "Lat", "Lon")

#merge the perf with the location
branchLocs <- left_join(branchLocs, branchPerf, by = "BranchNumber")


#get a subset of the CMG data
portLocs <- cmgAll %>% 
    filter(Month2016 == "Jul", Portfolio == "CMG" | Portfolio == "Premier") %>%
    select(Portfolio, 
           HH_Patronage_Branch, 
           BranchVisits, 
           Month2016)

colnames(portLocs) <- c("Portfolio", "BranchNumber", "Visits",
                        "Month2016")

portRollup <- portLocs %>% group_by(Portfolio, BranchNumber) %>%
            summarize(Visits = sum(Visits)) %>%
            select(Portfolio, BranchNumber, Visits)



#merge the branchLocs and branchPL on branch number
branchData <- inner_join(portRollup, branchLocs, by = "BranchNumber")

branchData <- na.omit(branchData)
#two observations removed from the na.omit()

mapData <- arrange(branchData, Visits)


#######################################
#mapping
######################################

mapData$PerfLevel <- as.ordered(mapData$PerfLevel)


#bucket visits
mapData$visitLvl <- cut(mapData$Visits,
                            c(0,100,200,400,600,10000), include.lowest = T,
                            labels = c("<100", "100-200", "200-400", "400-600", "600+"))


#set the color scale
binColors <- colorFactor(rainbow(2), mapData$Portfolio)
binColorsVisLvl <- colorFactor(palette = "Greens", mapData$visitLvl)
binColorsPerfLvl <- colorFactor(rainbow(5), mapData$PerfLevel)


#create a full popup
mapData$popUp <- paste("<strong>",mapData$BranchName, "</strong><br>",
                              "Port = ", mapData$Portfolio, "<br>",
                              "Visits = ", mapData$Visits, "<br>", 
                              "Perf Lvl = ", mapData$PerfLevel, sep = "")


#ensure that the map data is sorted by visits
mapData <- arrange(mapData, Visits)

m <- leaflet() %>%
    addTiles("Stamen.Toner") %>%
    addProviderTiles("Stamen.Toner", group = "Toner") %>%
    addProviderTiles("CartoDB.Positron", group = "CartoDB") %>%
    setView(lng = -71.0572145, lat = 42.3586396, zoom = 8) %>%
    
    
    #visits
    addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon,
                     color = ~binColorsVisLvl(visitLvl), popup = mapData$popUp,
                     radius = ~Visits/15, group = "Visits") %>%
    #porfolio
    addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColors(Portfolio), popup = mapData$popUp,
                     group = "Portfolio") %>%
    
    #performance
    addCircleMarkers(data = mapData, lat = ~Lat, lng = ~Lon, 
                     color = ~binColorsPerfLvl(PerfLevel), popup = mapData$popUp,
                     group = "Performance") %>%
    
   

    #layer control
    addLayersControl(
        baseGroups = c("Toner", "CartoDb"),
        overlayGroups = c("Visits",
                          "Portfolio",
                          "Performance"
        ),
        options = layersControlOptions(collapsed = F)
    ) %>%
    
    
    #portfolio        
    addLegend("bottomright", pal = binColors, values = mapData$Portfolio,
              title = "Portfolio",
              opacity = 1) %>%
    #visits    
    addLegend("bottomright", pal = binColorsVisLvl, values = mapData$visitLvl,
              title = "Total Visits<br>In Jan",
              opacity = 1) %>%

    #performance
    addLegend("bottomleft", pal = binColorsPerfLvl, values = mapData$PerfLevel,
          title = "Perf Level<br>In Jan",
          opacity = 1)
    
m  # Print the map


################################################
#make a table from the map
table(mapData$PerfLevel, mapData$Portfolio)






#################################################################
#export columns from report


HH <- select(cmgAll, Portfolio, HH_Patronage_Branch)


HH <- cmgAll %>% 
    filter(Month2016 == "Jan") %>%
    select(HH_Patronage_Branch, 
           Porfolio,
           Month2016)


write.csv(HH, file="C:/Users/n846490/Documents/DataScience/CSVs/PortfolioHH.csv", row.names=FALSE)

#######################################################################
