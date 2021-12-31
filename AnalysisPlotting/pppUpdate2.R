




library(RODBC)
library(ggplot2)
library(reshape2)
library(scales)
library(stringi)            # For processing strings
library(tidyr)              
library(knitr)
library(gridExtra)
library(plotly)
library(readr)
library(DT)
library(RColorBrewer)
library(dplyr)


setwd('C:/Users/bbutler/Documents/DataScience/PPP_project/SQLQueries')


# set up the query function
AP2_QUERY <- function(sql){
  ptm <- proc.time()
  cred <- "DSN=AP2"
  ch <- odbcDriverConnect(cred)
  res <- sqlQuery(ch,sql,as.is = TRUE)
  odbcClose(ch)
  timeit <- ((proc.time()-ptm)[3])/60
  print(round(timeit,2))
  return(res)
}

#######   GET ALL THE DATA
#####    prePPP
sql = read_file('prePPP.sql')
prePPP = AP2_QUERY(sql = sql)
head(prePPP)
write_csv(prePPP, 'C:/Users/bbutler/Documents/DataScience/PPP_project/CSVFiles/prePPP.csv')


#####    newAccounts
sql = read_file('newAccts.sql')
newAccts = AP2_QUERY(sql = sql)
head(newAccts)



###### prePPPNumAcct
sql = read_file('prePPPNumAccts.sql')
prePPPNumAcct = AP2_QUERY(sql = sql)
head(prePPPNumAcct)


####  transactCat
sql = read_file('transactCat.sql')
transactCat = AP2_QUERY(sql = sql)
head(transactCat)
write_csv(transactCat, 'C:/Users/bbutler/Documents/DataScience/PPP_project/CSVFiles/transactCat.csv')




####  transAcct
sql = read_file('transAcct.sql')
transAcct = AP2_QUERY(sql = sql)
head(transAcct)


####  pppBalances
sql = read_file('pppBalances.sql')
pppBalances = AP2_QUERY(sql = sql)
head(pppBalances)
write_csv(pppBalances, 'C:/Users/bbutler/Documents/DataScience/PPP_project/CSVFiles/pppBalances.csv')

## revised closed with detail
sql = read_file('ClosedAcctsRev.sql')
ClosedAcctsRev = AP2_QUERY(sql = sql)
head(ClosedAcctsRev)

write_csv(ClosedAcctsRev, 'C:/Users/bbutler/Documents/DataScience/PPP_project/CSVFiles/ClosedAcctsRev.csv')


ClosedAcctsRev$Acct_Ledger_Balance_YTD_Avg = as.numeric(ClosedAcctsRev$Acct_Ledger_Balance_YTD_Avg)
ClosedAcctsRev$Acct_Ledger_Balance_MTD_Avg = as.numeric(ClosedAcctsRev$Acct_Ledger_Balance_MTD_Avg)

length(unique(ClosedAcctsRev$Customer_CIS_Number))
mean(ClosedAcctsRev$Acct_Ledger_Balance_YTD_Avg)

# get branch data
branch = read_csv('C:/Users/bbutler/Documents/DataScience/BranchInfo/branch numbers and cost centers only.csv')
head(branch)


##################  START THE CLEANING AND ANALYSIS

easternPal = c('midnightblue', 'sienna1', 'darkgray', 'skyblue', 'darkorange4', 'deepskyblue')

# use this theme
theme_bryan <- function () { 
  theme(axis.text.x = element_text(size = 8, color = 'blue', angle = 0),
        legend.position = '',
        axis.text.y = element_text(color = 'blue'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(color = 'darkblue', size = 10, face = 'bold'),
        strip.background = element_rect(fill = 'light blue'))
}



# join branch on branch close
# mapData <- merge(helocAdd, zipcode, by.x = 'propertyZip', by.y = 'zip')
# branchClosedwName = dplyr::left_join(branchClosed, branch, by = c('Account_Branch_Number' = 'BranchNum'))

# do the same with leadlist
# leadListwName = dplyr::left_join(leadList, branch, by = c('Account_Branch_Number' = 'BranchNum'))
length(unique(prePPP$Cust_CIS_Nbr))


# sort customers and prospects
prePPP$CustValue = ifelse(prePPP$Customer == 'Customer',1,0)


custRollup = prePPP %>% group_by(Customer, BusType) %>% 
  summarise(custCounts = n_distinct(Cust_CIS_Nbr))

# get the subset of existing customers who had consumer accts
consumers = prePPP %>% filter(Consumer_Acct == 'Consumer') %>% 
group_by(Cust_CIS_Nbr, Consumer_Acct)
length(unique(consumers$Cust_CIS_Nbr))



custAccounts = prePPP %>% group_by(Cust_CIS_Nbr, BusType) %>% 
  summarise(AccntCnts = n_distinct(Account_Type_Descr))


#### get mapping of cust CIS to type and prospect
mapping = prePPP %>% group_by(n_distinct(Cust_CIS_Nbr)) %>% 
  select(Cust_CIS_Nbr, Customer, BusType)


mapping = distinct(prePPP, Cust_CIS_Nbr, .keep_all = T) %>% 
  select(Cust_CIS_Nbr, Customer, BusType)


profiles = prePPP %>% select(Cust_CIS_Nbr, BusType, Customer) %>% 
  group_by(Cust_CIS_Nbr, BusType, Customer) %>% 
  count(Cust_CIS_Nbr)

write_csv(profiles, 'C:/Users/bbutler/Documents/DataScience/PPP_project/CSVFiles/profiles.csv')

# plot histogram
g = ggplot(custAccounts, aes(x = AccntCnts, fill = BusType)) + 
  geom_histogram(binwidth = 1, color = 'white') + 
  stat_bin(binwidth=1, geom='text', color='midnightblue', aes(label=..count..),
           position=position_stack(vjust = 1.75)) + 
  scale_fill_manual(values = easternPal) + 
  theme_bryan() + 
  theme(axis.title.x = element_text(color = 'blue', size = 8)) + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) + 
  facet_wrap(~BusType) +
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Distribution of Number of Accounts")
g



## get ppp loan production
pppLoanIssue = prePPP %>% select(Cust_CIS_Nbr, Month_Post_Issued, Customer, BusType) %>% 
  group_by(Month_Post_Issued,  Customer, BusType) %>% 
  summarise(Counts = n_distinct(Cust_CIS_Nbr))



pppLoanIssue = pppLoanIssue %>% dplyr::mutate(Month = dplyr::case_when(Month_Issued  == "1" ~ "Jan",
                                                                     Month_Issued  == "2" ~ "Feb",
                                                                     Month_Issued  == "3" ~ "Mar",
                                                                     Month_Issued  == "4" ~ "Apr",
                                                                     Month_Issued  == "5" ~ "May",
                                                                     Month_Issued  == "6" ~ "Jun",
                                                                     Month_Issued  == "7" ~ "Jul",
                                                                     Month_Issued  == "8" ~ "Aug",
                                                                     Month_Issued  == "9" ~ "Sep",
                                                                     Month_Issued  == "10" ~ "Oct",
                                                                     Month_Issued  == "11" ~ "Nov",
                                                                     Month_Issued  == "12" ~ "Dec"))


pppLoanIssue$Month = ordered(pppLoanIssue$Month, levels = c("Jan",
                                                               "Feb",
                                                               "Mar",
                                                               "Apr",
                                                               "May",
                                                               "Jun",
                                                               "Jul",
                                                               "Aug",
                                                               "Sep",
                                                               "Oct",
                                                               "Nov",
                                                               "Dec"))



# bar plot by Month_Post
# make a plot
plotLoans =  pppLoanIssue %>% select(Month, Counts) %>% 
  group_by(Month) %>% 
  summarise(TotLoans = sum(Counts))
g1 = ggplot(plotLoans, aes(x = Month, y = TotLoans, label = TotLoans)) +
  geom_bar(stat = 'identity', fill = 'midnightblue') + 
  geom_text(size = 4, position = position_stack(vjust = 0.5), color = 'white') + 
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("PPP Loan Originations") +
  theme(legend.position = 'bottom')
g1


############  LOOK AT NEW ACCOUNTS

# rename cis in mapping
# colnames(trSamp)[2] <- "newname2"

colnames(mapping)[1] = 'Customer_CIS_Number'


newAcctsMap = left_join(newAccts, mapping, by = c('Customer_CIS_Number'))
sum(is.na(newAcctsMap$Customer))



# group by account_type desc
sum(is.na(prePPP$Account_Type_Descr))

# Accounts for Existing
acctType = prePPP %>% select(BusType, Account_Type_Descr, Cust_CIS_Nbr) %>% 
  group_by(Account_Type_Descr,BusType) %>% 
  filter(!is.na(Account_Type_Descr)) %>% 
  summarise(Counts = n_distinct(Cust_CIS_Nbr)) %>% 
  arrange(desc(Counts))
  
  
plotAcctType = head(acctType,10)  

# top 10
# plot histogram
g1 = ggplot(plotAcctType, aes(x = Counts, y = reorder(Account_Type_Descr, Counts),  fill = BusType)) +
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Top Ten Types of Accounts Held By Existing Customers") + 
  theme(legend.position = 'bottom')
g1


####### NEW ACCOUNTS BY BUSINESS AND cOMMERCIAL
newAcctsPlot = newAcctsMap %>% select(BusType, Account_Type_Descr, Customer_CIS_Number) %>% 
  group_by(Account_Type_Descr,BusType) %>% 
  filter(!is.na(BusType)) %>% 
  summarise(Counts = n_distinct(Customer_CIS_Number)) %>% 
  arrange(desc(Counts))



g1 = ggplot(head(newAcctsPlot,10), aes(x = Counts, y = reorder(Account_Type_Descr, Counts),  fill = BusType)) +
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Top Ten Types of New Accounts Opened by PPP Loanholders") + 
  theme(legend.position = 'bottom')
g1


#### NEW ACCOUNTS BY CUSTOMER PROSPECT
newAcctsPlot = newAcctsMap %>% select(Customer_CIS_Number, Account_Type_Descr, Customer) %>% 
  group_by(Account_Type_Descr,Customer) %>% 
  filter(!is.na(Customer)) %>% 
  summarise(Counts = n_distinct(Customer_CIS_Number)) %>% 
  arrange(desc(Counts))



g1 = ggplot(head(newAcctsPlot,10), aes(x = Counts, y = reorder(Account_Type_Descr, Counts),  fill = Customer)) +
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Top Ten Types of New Accounts Opened by PPP Loanholders") + 
  theme(legend.position = 'bottom')
g1




######### ROLL UP TRANSACTIONS
transactInfo = transactCat

# get num accounts
numAccts = transactCat %>% group_by(Cust_CIS_Nbr) %>% 
  summarize(Accts = n_distinct(Account_Number))
head(numAccts)

# join to info

transactInfo = left_join(transactInfo, numAccts, by = 'Cust_CIS_Nbr')
head(transactInfo, 10)

transactInfo$Month_Post = ordered(transactInfo$Month_Post, levels = c("Jan",
                                                                "Feb",
                                                                "Mar",
                                                                "Apr",
                                                                "May",
                                                                "Jun",
                                                                "Jul",
                                                                "Aug",
                                                                "Sep",
                                                                "Oct",
                                                                "Nov",
                                                                "Dec"))


transactInfo$TransPerAcct = transactInfo$Num_Trans/transactInfo$Accts

# join customer info on transMonth
transMonthInfo = left_join(transactInfo, mapping, by = c('Cust_CIS_Nbr' = 'Customer_CIS_Number'))

head(transMonthInfo)

transMonth = transMonthInfo %>% select(Month_Post, Cust_CIS_Nbr, Customer, BusType, TransPerAcct) %>% 
  group_by(Month_Post, Cust_CIS_Nbr, Customer, BusType) %>% 
  summarise(AvgTrans = mean(TransPerAcct, na.rm = T))
head(transMonth)

# remove the na
transMonth = transMonth %>% filter(!is.na(Customer))

min(transMonth$AvgTrans)

# remove everything over 50 for scale
plotTrans = transMonth %>% filter(AvgTrans < 50)

# try a boxplot
b <- ggplot(plotTrans, aes(x = Month_Post, y = AvgTrans, fill = BusType)) + 
  geom_boxplot(notch=FALSE) + 
  scale_fill_manual(values = easternPal) +
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~Customer, scales = 'free') + 
  theme(legend.position = 'bottom') +
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Average Transactions per Account by Month")
b


##### look at transaction type

transTypes = transMonthInfo %>% select(Month_Post, Transaction_Descr, Num_Trans, Customer) %>% 
  group_by(Month_Post, Transaction_Descr, Customer) %>% 
  summarise(TotTrans = sum(Num_Trans))


transTypes$Customer = ifelse(is.na(transTypes$Customer), "Prospect", transTypes$Customer)


# final rollup
transTypesTotal = transTypes %>% group_by(Transaction_Descr, Customer) %>% 
  summarise(Tot = sum(TotTrans)) %>% 
  arrange(desc(Tot)) %>% 
  top_n(30)

g1 = ggplot(head(transTypesTotal,30), aes(x = Tot, y = reorder(Transaction_Descr, Tot),  fill = Customer)) +
  geom_bar(stat = 'identity') + 
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  scale_x_continuous(labels = comma) + 
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Top Transactions Types") + 
  theme(legend.position = 'bottom')
g1


min(ClosedAcctsRev$Account_Open_Date)





#### CHECK BALANCES
# extract Month_Post for transactions
pppBalances = read_csv('C:/Users/bbutler/Documents/DataScience/PPP_project/CSVFiles/pppBalances.csv')
head(pppBalances)


pppBalances$Month_Num = stringr::str_sub(pppBalances$Year_Month_Number,-2,-1)


# convert to Jan, Feb, etc.
pppBalances = pppBalances %>% dplyr::mutate(Month = dplyr::case_when(Month_Num  == "01" ~ "Jan",
                                                                    Month_Num  == "02" ~ "Feb",
                                                                    Month_Num  == "03" ~ "Mar",
                                                                    Month_Num  == "04" ~ "Apr",
                                                                    Month_Num  == "05" ~ "May",
                                                                    Month_Num  == "06" ~ "Jun",
                                                                    Month_Num  == "07" ~ "Jul",
                                                                    Month_Num  == "08" ~ "Aug",
                                                                    Month_Num  == "09" ~ "Sep",
                                                                    Month_Num  == "10" ~ "Oct",
                                                                    Month_Num  == "11" ~ "Nov",
                                                                    Month_Num  == "12" ~ "Dec"))


pppBalances$Month = ordered(pppBalances$Month, levels = c("Jan",
                                                               "Feb",
                                                               "Mar",
                                                               "Apr",
                                                               "May",
                                                               "Jun",
                                                               "Jul",
                                                               "Aug",
                                                               "Sep",
                                                               "Oct",
                                                               "Nov",
                                                               "Dec"))





# join the mapping data
head(pppBalances)

# just need customer
mappingCust = mapping[,c(1,2)]


# convert cis to numeric
mappingCust$Customer_CIS_Number = as.numeric(mappingCust$Customer_CIS_Numbe)

pppBalancesAll = left_join(pppBalances, mappingCust, by = c('Cust_CIS_Nbr' = 'Customer_CIS_Number'))
head(pppBalancesAll)


unique(pppBalancesAll$Month_Issued)

# make monthissued a name
# convert to Jan, Feb, etc.
pppBalancesAll = pppBalancesAll %>% dplyr::mutate(Month_Cohort = dplyr::case_when(Month_Issued  == 4 ~ "Apr",
                                                                     Month_Issued  == 5 ~ "May",
                                                                     Month_Issued  == 6 ~ "Jun",
                                                                     Month_Issued  == 7 ~ "Jul",
                                                                     Month_Issued  == 8 ~ "Aug"))

pppBalancesAll$Month_Cohort = ordered(pppBalancesAll$Month_Cohort, levels = c('Apr',
                                                                              'May',
                                                                              'Jun',
                                                                              'Jul',
                                                                              'Aug'))


                                                                     
                                                                     

# roll it up remove NA customers
groupBalance = pppBalancesAll %>% select(Month, Acct_Ledger_Balance_MTD_Avg, Account_Type_Descr,
                                      Acct_Ledger_Balance_YTD_Avg, Customer, Customer_CIS_Number,
                                      BusType, Consumer_Acct, Month_Cohort) %>% 
  filter(!is.na(Customer)) %>% 
  group_by(Month, Customer_CIS_Number, Customer, BusType, Account_Type_Descr, Consumer_Acct, Month_Cohort) %>% 
  summarise(AvgMTD = mean(Acct_Ledger_Balance_MTD_Avg, na.rm = T),
            AvgYTD = mean(Acct_Ledger_Balance_YTD_Avg, na.rm = T))

# remove white spaces
groupBalance$Account_Type_Descr = trimws(groupBalance$Account_Type_Descr)
unique(groupBalance$Account_Type_Descr)




# plot of all balances
# filter it to less than 500K
plotBal = groupBalance %>% filter(AvgMTD < 300000 & BusType == 'Business')

b <- ggplot(plotBal, aes(x = Month, y = AvgMTD, fill = Customer)) + 
  geom_boxplot(notch=FALSE) + 
  scale_fill_manual(values = easternPal) +
  scale_y_continuous(labels = comma, limits = c(0,250000)) + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + 
  theme(axis.text.x = element_text(size = 8, color = 'blue', angle = 45, hjust = 1)) + 
  theme(legend.position = 'right') + 
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Average MTD Balance for Business Accts")
b



plotBal = groupBalance %>% filter(AvgMTD < 500000 & BusType == 'Business')
# another roll up
plotBal = plotBal %>% group_by(Month, Month_Cohort) %>% 
  summarise(AvgBal = mean(AvgMTD))

plotBal$Month_Cohort = as.factor(plotBal$Month_Cohort)

b <- ggplot(plotBal, aes(x = Month, y = AvgBal, group = Month_Cohort, color = Month_Cohort)) + 
  geom_line(size = 1.2) + 
  scale_color_manual(values = easternPal) +
  scale_y_continuous(labels = comma, limits = c(0,75000)) + 
  theme_bryan() + 
  theme(axis.text.x = element_text(size = 8, color = 'blue', angle = 45, hjust = 1)) + 
  theme(legend.position = 'right') + 
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Average MTD Balance for Business Accts By Loan Month Cohort")
b




#### Commercial

plotBal = groupBalance %>% filter(AvgMTD < 500000 & BusType == 'Commercial')

b <- ggplot(plotBal, aes(x = Month, y = AvgMTD, fill = Customer)) + 
  geom_boxplot(notch=FALSE) + 
  scale_fill_manual(values = easternPal) +
  scale_y_continuous(labels = comma, limits = c(0,100000)) + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + 
  theme(axis.text.x = element_text(size = 8, color = 'blue', angle = 45, hjust = 1)) + 
  theme(legend.position = 'right') + 
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Average MTD Balance for Commercial Accts")
b



plotBal = groupBalance %>% filter(AvgMTD < 1000000 & BusType == 'Commercial')
# another roll up
plotBal = plotBal %>% group_by(Month, Month_Cohort) %>% 
  summarise(AvgBal = mean(AvgMTD))

plotBal$Month_Cohort = as.factor(plotBal$Month_Cohort)

b <- ggplot(plotBal, aes(x = Month, y = AvgBal, group = Month_Cohort, color = Month_Cohort)) + 
  geom_line(size = 1.2) + 
  scale_color_manual(values = easternPal) +
  scale_y_continuous(labels = comma, limits = c(0,250000)) + 
  theme_bryan() + 
  theme(axis.text.x = element_text(size = 8, color = 'blue', angle = 45, hjust = 1)) + 
  theme(legend.position = 'right') + 
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Average MTD Balance for Commercial Accts By Loan Month Cohort")
b







# plot balances by Month_Post year
plotBal = groupBalance %>% filter(Account_Type_Descr %in% c('BUS SELECT CKG II',
                                                            'FREE CHECKING',
                                                            'FREE BUSINESS CKG',
                                                            'BUSINESS CHECKING',
                                                            'BUS SELECT CKG I',
                                                            'BUS STATEMENT SVGS',
                                                            'BUSI SELECT MON MKT'))


# filter it to less than 500K
plotBal = plotBal %>% filter(AvgMTD < 500000)


plotBal2 = plotBal %>% filter(Account_Type_Descr == 'BUSINESS CHECKING')
b <- ggplot(plotBal2, aes(x = Month, y = AvgMTD, fill = Customer)) + 
  geom_boxplot(notch=FALSE) + 
  scale_fill_manual(values = easternPal) +
  scale_y_continuous(labels = comma) + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BusType, scales = 'free_y') + 
  theme(axis.text.x = element_text(size = 8, color = 'blue', angle = 45, hjust = 1)) + 
  theme(legend.position = 'bottom') + 
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Average MTD Balance for Business Checking")
b

plotBal3 = plotBal %>% filter(Account_Type_Descr == 'FREE CHECKING')
b <- ggplot(plotBal3, aes(x = Month, y = AvgMTD, fill = Customer)) + 
  geom_boxplot(notch=FALSE) + 
  scale_fill_manual(values = easternPal) +
  scale_y_continuous(labels = comma) + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BusType, scales = 'free_y') + 
  theme(axis.text.x = element_text(size = 8, color = 'blue', angle = 45, hjust = 1)) + 
  theme(legend.position = 'bottom') + 
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Average MTD Balance for Free Checking")
b


plotBal4 = plotBal %>% filter(Account_Type_Descr == 'FREE BUSINESS CKG')
b <- ggplot(plotBal4, aes(x = Month, y = AvgMTD, fill = Customer)) + 
  geom_boxplot(notch=FALSE) + 
  scale_fill_manual(values = easternPal) +
  scale_y_continuous(labels = comma) + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BusType, scales = 'free_y') + 
  theme(axis.text.x = element_text(size = 8, color = 'blue', angle = 45, hjust = 1)) + 
  theme(legend.position = 'bottom') + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Average MTD Balance for Free Business CKG")
b


plotBal5 = plotBal %>% filter(Account_Type_Descr == 'BUS SELECT CKG I' | Account_Type_Descr == 'BUS SELECT CKG II')
b <- ggplot(plotBal5, aes(x = Month, y = AvgMTD, fill = Customer)) + 
  geom_boxplot(notch=FALSE) + 
  scale_fill_manual(values = easternPal) +
  scale_y_continuous(labels = comma) + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BusType, scales = 'free_y') + 
  theme(axis.text.x = element_text(size = 8, color = 'blue', angle = 45, hjust = 1)) + 
  theme(legend.position = 'bottom') + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Average MTD Balance for Bus Select CKG I & II")
b


plotBal6 = plotBal %>% filter(Account_Type_Descr == 'BUS STATEMENT SVGS' | Account_Type_Descr == 'BUSI SELECT MON MKT')
b <- ggplot(plotBal6, aes(x = Month, y = AvgMTD, fill = Customer)) + 
  geom_boxplot(notch=FALSE) + 
  scale_fill_manual(values = easternPal) +
  scale_y_continuous(labels = comma) + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + facet_wrap(~BusType, scales = 'free_y') + 
  theme(axis.text.x = element_text(size = 8, color = 'blue', angle = 45, hjust = 1)) + 
  theme(legend.position = 'right') + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Average MTD Balance for Bus Svgs & Money Mkt")
b









# branch closures
# join with mapping
pppClosedInfo = left_join(pppClosed, mapping, by = c('Customer_CIS_Number' = 'Cust_CIS_Nbr'))
head(pppClosedInfo)

# make closed date a date
pppClosedInfo$Account_Close_Date = as.Date(pppClosedInfo$Account_Close_Date, format = '%Y-%m-%d')
head(pppClosedInfo)


# get Month_Post year
pppClosedInfo$Month_Post = format(pppClosedInfo$Account_Close_Date, format = "%b")
pppClosedInfo$Month_Post = as.factor(pppClosedInfo$Month_Post)
pppClosedInfo$Month_Post = ordered(pppClosedInfo$Month_Post, levels = c("Apr", "May", "Jun", "Jul",
                                                              "Aug","Sep", "Oct", "Nov", "Dec"))


# map branch name
# mapData <- merge(helocAdd, zipcode, by.x = 'propertyZip', by.y = 'zip')
pppClosedBranchInfo = dplyr::left_join(pppClosedInfo, branch, by = c('Account_Branch_Number' = 'BranchNum'))


pppClosedGroup =pppClosedBranchInfo %>% select(Month_Post, Customer, BusType, BranchName, Account_Number) %>% 
  group_by(Month_Post, Customer, BusType, BranchName) %>% 
  summarize(ClosedAccts = n_distinct(Account_Number))
head(pppClosedGroup)


pppClosedGroup2 = pppClosedGroup %>% select(Month_Post, Customer, BusType, BranchName, ClosedAccts) %>% 
  filter(!is.na(Customer)) %>% 
  group_by(Month_Post, Customer, BusType) %>% 
  summarise(TotClosed = sum(ClosedAccts))

# bar plot by Month_Post
# make a plot
plotClosed = pppClosedGroup2 %>% select(Month_Post, BusType, TotClosed) %>% 
  group_by(Month_Post, BusType) %>% 
  summarise(TotClosed = sum(TotClosed))
g1 = ggplot(plotClosed, aes(x = Month_Post, y = TotClosed,  fill = BusType, label = TotClosed)) +
  geom_bar(stat = 'identity') + 
  geom_text(size = 4, position = position_stack(vjust = 0.5), color = 'white') + 
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Account Closures") + 
  theme(legend.position = 'right')
g1



# prospects
plotClosed = pppClosedGroup2 %>% select(Month_Post, Customer, TotClosed) %>% 
  group_by(Month_Post, Customer) %>% 
  summarise(TotClosed = sum(TotClosed))
g1 = ggplot(plotClosed, aes(x = Month_Post, y = TotClosed,  fill = Customer, label = TotClosed)) +
  geom_bar(stat = 'identity') + 
  geom_text(size = 4, position = position_stack(vjust = 0.5), color = 'white') + 
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Account Closures") + 
  theme(legend.position = 'right')
g1


# by branch
plotClosed = pppClosedGroup %>% select(BranchName, Customer, ClosedAccts) %>% 
  group_by(BranchName, Customer) %>% 
  summarise(TotClosed = sum(ClosedAccts))


# change NA branch name to CSC

plotClosed$BranchName = ifelse(is.na(plotClosed$BranchName), 'CSC', plotClosed$BranchName)

grandTotal = plotClosed %>% select(BranchName, TotClosed) %>% 
  group_by(BranchName) %>% 
  summarise(GrandTotal = sum(TotClosed))

plotClosed2 = left_join(plotClosed,grandTotal, by = c('BranchName'))

plotClosed2 = plotClosed2 %>% select(BranchName, Customer, TotClosed, GrandTotal) %>% 
  group_by(BranchName, Customer, TotClosed, GrandTotal) %>% 
  arrange(desc(GrandTotal))

plotClosed3 = head(plotClosed2, 30)


# filter out na cust
plotClosed3 = plotClosed3 %>% filter(!is.na(Customer))


g1 = ggplot(plotClosed3, aes(x = reorder(BranchName, GrandTotal), y = TotClosed,
                            fill = Customer, label = TotClosed)) +
  geom_bar(stat = 'identity') + 
  geom_text(size = 3, position = position_stack(vjust = 0.5), color = 'white') + 
  scale_fill_manual(values = easternPal) + 
  coord_flip() + 
  scale_x_discrete() + 
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Account Closures") + 
  theme(legend.position = 'bottom')
g1





##### Set Method of Closures
# closures by account type
# closures by month
pppClosed = ClosedAcctsRev %>% dplyr::mutate(Month = dplyr::case_when(Month_Closed  == 1 ~ "Jan",
                                                                     Month_Closed  == 2 ~ "Feb",
                                                                     Month_Closed  == 3 ~ "Mar",
                                                                     Month_Closed  == 4 ~ "Apr",
                                                                     Month_Closed  == 5 ~ "May",
                                                                     Month_Closed  == 6 ~ "Jun",
                                                                     Month_Closed  == 7 ~ "Jul",
                                                                     Month_Closed  == 8 ~ "Aug",
                                                                     Month_Closed  == 9 ~ "Sep",
                                                                     Month_Closed  == 10 ~ "Oct",
                                                                     Month_Closed  == 11 ~ "Nov",
                                                                     Month_Closed  == 12 ~ "Dec"))


pppClosed$Month = ordered(pppClosed$Month, levels = c("Jan",
                                                          "Feb",
                                                          "Mar",
                                                          "Apr",
                                                          "May",
                                                          "Jun",
                                                          "Jul",
                                                          "Aug",
                                                          "Sep",
                                                          "Oct",
                                                          "Nov",
                                                          "Dec"))







pppClosedType = pppClosed %>% select(Month, Customer_Type, Account_Type_Descr, Customer_CIS_Number) %>% 
  group_by(Month, Customer_Type, Account_Type_Descr) %>% 
  summarize(ClosedAccts = n_distinct(Customer_CIS_Number))
head(pppClosedType)


plotClosedType2 = pppClosed %>% select(Month, Customer_Type, Customer_CIS_Number) %>% 
  group_by(Month, Customer_Type) %>% 
  summarize(Count = n_distinct(Customer_CIS_Number))



aggClosed = pppClosed %>% select(Customer_Type, Customer_CIS_Number) %>% 
  group_by(Customer_Type) %>% 
  summarize(Count = n_distinct(Customer_CIS_Number))


plotClosed =  pppClosedType %>% select(Month, Customer_Type, ClosedAccts) %>% 
  group_by(Month, Customer_Type) %>% 
  summarise(TotClosed = sum(ClosedAccts))


g1 = ggplot(plotClosed, aes(x = Month, y = TotClosed, fill = Customer_Type, label = TotClosed)) +
  geom_bar(stat = 'identity') + 
  geom_text(size = 5, position = position_stack(vjust = 0.5), color = 'white') + 
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Accounts Closed by PPP Loan Holders") +
  theme(legend.position = 'right')
g1











pppClosedType = pppClosedType %>% filter(!is.na(Customer))




pppClosedType = pppClosedType %>% arrange(desc(ClosedAccts)) %>% 
  top_n(20)


plotClosedType = head(pppClosedType, 20)

# top 10
# plot histogram
g1 = ggplot(plotClosedType, aes(x = ClosedAccts, y = reorder(Account_Type_Descr, ClosedAccts),
                                fill = Customer, label = ClosedAccts)) +
  geom_bar(stat = 'identity') + 
  geom_text(size = 3, position = position_stack(vjust = 0.5), color = 'white') + 
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1), face = "bold.italic", color = "blue")) +
  ggtitle("Types of Accounts") + 
  theme(legend.position = 'bottom')
g1


# filter prospects

prospectsClosed = pppClosedBranchInfo %>% select(Customer_CIS_Number, Customer) %>% 
  filter(Customer == 'Prospect') %>% 
  group_by(Customer_CIS_Number, Customer)


commercialClosed = pppClosedBranchInfo %>% select(Customer_CIS_Number, BusType) %>% 
  filter(BusType == 'Commercial') %>% 
  group_by(Customer_CIS_Number, BusType)

prospect_set = unique(prospectsClosed$Customer_CIS_Number)


commercial_set = unique(commercialClosed$Customer_CIS_Number)


set.intersection <- function(a, b) {
  intersect <- vector()
  
  for (i in 1:length(a)) {
    if (a[i] %in% b) {
      intersect <- append(intersect, a[i])
    }
  }
  return(intersect)
}


overlap = set.intersection(prospect_set, commercial_set)

