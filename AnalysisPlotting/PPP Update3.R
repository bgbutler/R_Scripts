


# library(RODBC)
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


setwd('C:/Users/bbutler/Documents/DataScience/PPP_project')


# get the data
prePPP = read_csv('prePPPCust.csv')
head(prePPP)


# get the closed accounts
closed = read_csv('pppClosedClean.csv')


# get transactions
trans = read_csv('pppTransactions.csv')

# get grouped transactions
transGrouped = read_csv('transGrouped.csv')

# map cis to bustype
custMapClosed = prePPP %>% select(Cust_CIS_Nbr, UniqueCust, Cust_Status) %>% filter(UniqueCust == 1)

custMap = prePPP %>% select(Cust_CIS_Nbr, BusType, UniqueCust, Cust_Status) %>% filter(UniqueCust == 1)


##################  START THE CLEANING AND ANALYSIS

easternPal = c('midnightblue', 'sienna1', 'darkgray','skyblue', 'darkorange4', 'deepskyblue')

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


# set daysClosed
daysOpen = 45

# get all closed within 30 days
closed30 = closed %>% filter(Days_Open < daysOpen)


closed30Map = left_join(closed30, custMapClosed, by = c('Customer_CIS_Number' = 'Cust_CIS_Nbr'))
closed30Map$Cust_Status = ifelse(is.na(closed30Map$Cust_Status), 'Loan Cust', closed30Map$Cust_Status)

# get loan data for closed
closed30gp = closed30Map %>% select(Customer_CIS_Number, Customer_Short_Name,
                                 Days_Open, Loan_Balance, CheckLoanNum, BusType, Account_Branch_Number, Cust_Status) %>% 
  group_by(Customer_CIS_Number, Customer_Short_Name, Days_Open, BusType, Account_Branch_Number, Cust_Status) %>% 
  summarise(TotLoan = sum(Loan_Balance * CheckLoanNum)) %>% 
  arrange(Days_Open, Customer_CIS_Number)

head(closed30gp)

plot30 = closed30gp %>% group_by(Days_Open, BusType) %>% 
  summarise(Tot_Loans = sum(TotLoan),
            Count = n())


# first plot
g1 = ggplot(plot30, aes(x = Days_Open, y = Count, fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
               size = 4, color = 'blue',
               aes(y=Count+3, label=Count, hjust=.5)) +
  
  
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle(paste("Number of Accounts Closed within ",daysOpen, " Days")) +
  theme(legend.position = 'right') + 
  theme(axis.title.x = element_text(color = 'blue')) + 
  xlab("Days Open")
g1


# get the value
TotalLoans = sum(plot30$Tot_Loans)
TotalLoans = 76473180

# zoom in
plot10 = plot30 %>% filter(Days_Open <11)
g1 = ggplot(plot10, aes(x = Days_Open, y = Count, fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(y=Count+3, label=Count, hjust=.5)) +
  
  
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Number of Accounts Closed within 10 Days") +
  theme(legend.position = 'bottom') + 
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) + 
  theme(axis.title.x = element_text(color = 'blue')) + 
  xlab("Days Open")
g1





################## cut by customer type
plot30Cust = closed30gp %>% group_by(Days_Open, Cust_Status) %>% 
  summarise(Tot_Loans = sum(TotLoan),
            Count = n())



# cust type plot
g1 = ggplot(plot30Cust, aes(x = Days_Open, y = Count, fill = Cust_Status)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(y=Count+3, label=Count, hjust=.5)) +
  
  
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Number of Accounts Closed within 30 Days") +
  theme(legend.position = 'right') + 
  theme(axis.title.x = element_text(color = 'blue')) + 
  xlab("Days Open")
g1









# group by acct type
acct30 = closed30 %>% group_by(BusType, Account_Type_Descr) %>% 
  summarise(Tot_Accts = n()) %>% 
  arrange(desc(Tot_Accts))

g1 = ggplot(acct30, aes(x = Tot_Accts, y = reorder(Account_Type_Descr, Tot_Accts), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=Tot_Accts+3, label=Tot_Accts, hjust=0)) + 
  
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Accounts Closed within 30 Days By Type") +
  theme(legend.position = 'right')
g1








#### look at prePPP
customers = prePPP %>% select(BusType, PPP_Balance, Cust_Status, UniqueCust) %>% 
  filter(UniqueCust == 1)

# group for plotting
custgp = customers %>% group_by(BusType, Cust_Status) %>% 
  summarise(LoanTot = sum(PPP_Balance),
            Count = n())

# customer plot
g1 = ggplot(custgp, aes(x = Count, y = reorder(Cust_Status, Count), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=Count+3, label=format(Count, big.mark = ','), hjust=0)) + 
  
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Customer Profile of PPP Customers") +
  theme(legend.position = 'bottom')
g1


# customer plot
g1 = ggplot(custgp, aes(x = Count, y = reorder(Cust_Status, Count), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=Count+3, label=format(LoanTot, big.mark = ','), hjust=0)) + 
  
  scale_fill_manual(values = easternPal) +
  
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Customer Profile of PPP Customers with Loan Totals") +
  theme(legend.position = 'bottom') + 
  scale_x_continuous(labels = comma, limits = c(0, 6000))
g1



###### transactions  #######


c = length(unique(trans$Cust_CIS_Nbr))
c


# map to trans
transMap = left_join(trans, custMap, by = c('Cust_CIS_Nbr'))
head(transMap)

# order the months
inactive$Month_Post = ordered(inactive$Month_Post, levels = c("Jan",
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




# get a rollup
transgp = transMap %>% select(Transaction_Descr, Month_Post, Trans_per_Account, Trans_Bins, BusType) %>% 
  group_by(Month_Post, Trans_Bins, Transaction_Descr, BusType) %>% 
  summarise(TransAcct = sum(Trans_per_Account))

plotTrans = transgp %>% group_by(Transaction_Descr, BusType) %>% 
  summarise(TotalTrans = sum(TransAcct)) %>% 
  filter(!is.na(BusType)) %>% 
  arrange(desc(TotalTrans))

  
  



# plot by bins
# customer plot
g1 = ggplot(head(plotTrans, 40), aes(x = TotalTrans, y = reorder(Transaction_Descr, TotalTrans), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=TotalTrans+3, label=format(TotalTrans, big.mark = ','), hjust=0)) + 
  
  scale_fill_manual(values = easternPal) +
  facet_wrap(~BusType, scales = 'free_y') +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Top Transaction Counts by Business Type in 2020") +
  theme(legend.position = 'bottom') + 
  scale_x_continuous(labels = comma, limits = c(0, 5000000))
g1


unique(transMap$Trans_Bins)

# get the inactives
inactive = transMap %>% select(Cust_CIS_Nbr, Cust_Status, Transaction_Descr, Month_Post, Trans_per_Account, Trans_Bins, BusType, Sum_Trans) %>% 
  group_by(Cust_CIS_Nbr, Cust_Status, , Trans_Bins, Month_Post, Transaction_Descr, BusType) %>% 
  filter(Trans_Bins == '(0.0, 5.0]' & !is.na(BusType)) %>% 
  summarise(TotAmt = sum(Sum_Trans),
            TransperAcct = max(Trans_per_Account))

# look at transaction nums
inactiveTrans = inactive %>% group_by(Transaction_Descr, BusType) %>% 
  summarise(TotalTrans = n(),
            TotAmt = sum(TotAmt)) %>% 
  arrange(desc(TotalTrans))



############################################################

# use gridarrange
# get transaction counts over 2020
busInact <- inactiveTrans %>% filter(BusType == 'Business') %>% 
  head(20)

# do for commercial
commInact <- inactiveTrans %>% filter(BusType == 'Commercial') %>% 
  head(20)



### this does the counts
#   arrange(desc(TotalTrans))
g1 = ggplot(busInact, aes(x = TotalTrans, y = reorder(Transaction_Descr, TotalTrans), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=TotalTrans+3, label=format(TotalTrans, big.mark = ','), hjust=0)) + 
  
  scale_fill_manual(values = easternPal) +
  
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Business") +
  

  # theme(axis.title.y = element_text(color = 'blue')) + 
  scale_x_continuous(labels = comma, limits = c(0, 5000))


#   arrange(desc(TotalTrans))
g2 = ggplot(commInact, aes(x = TotalTrans, y = reorder(Transaction_Descr, TotalTrans), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=TotalTrans+3, label=format(TotalTrans, big.mark = ','), hjust=0)) + 
  
  scale_fill_manual(values = 'sienna1') +
  
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Commercial") +
  # theme(legend.position = 'bottom') + 
  
  # theme(axis.title.y = element_text(color = 'blue')) + 
  scale_x_continuous(labels = comma, limits = c(0, 300))

grid.arrange(g1, g2, ncol=2)


########    LAYER IN THE TRANSACTION AMTS

### this does the counts
#   arrange(desc(TotalTrans))
g1 = ggplot(busInact, aes(x = TotalTrans, y = reorder(Transaction_Descr, TotalTrans), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=TotalTrans+3, label=format(TotAmt, big.mark = ',', scientific = F, digits = 0), hjust=0)) + 
  
  scale_fill_manual(values = easternPal) +
  
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Business") +
  
  
  # theme(axis.title.y = element_text(color = 'blue')) + 
  scale_x_continuous(labels = comma, limits = c(0, 6000))


#   arrange(desc(TotalTrans))
g2 = ggplot(commInact, aes(x = TotalTrans, y = reorder(Transaction_Descr, TotalTrans), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=TotalTrans+3, label=format(TotAmt, big.mark = ',', scientific = F, digits = 0), hjust=0)) + 
  
  scale_fill_manual(values = 'sienna1') +
  
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Commercial") +
  # theme(legend.position = 'bottom') + 
  
  # theme(axis.title.y = element_text(color = 'blue')) + 
  scale_x_continuous(labels = comma, limits = c(0, 300))

grid.arrange(g1, g2, ncol=2)


############################################

# set inactive to Q4
q4 = inactive %>% filter(Month_Post %in% c('Oct', 'Nov', 'Dec'))

inactRollup = inactive %>% select(Cust_CIS_Nbr, BusType, TotAmt, TransperAcct) %>% 
  group_by(Cust_CIS_Nbr, BusType) %>% 
  summarise(Total = sum(TotAmt),
            TPC = max(TransperAcct))

# describe transactions
inactRollup$Direction = ifelse(inactRollup$Total <= 0, 'Negative', 'Positive')


# group for plotting
plotInact = inactRollup %>% group_by(BusType, Direction, TPC) %>% 
  summarise(TotalTrans = sum(Total),
            Count = n())



# customer plot
g1 = ggplot(plotInact, aes(x = Count, y = reorder(TPC, Count), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=Count+3, label=format(Count, big.mark = ','), hjust=0)) + 
  
  scale_fill_manual(values = easternPal) +
  
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Deposit Profiles of 2,724 Least Active Customers") +
  theme(legend.position = 'bottom') + 
  facet_wrap(~Direction) + 
  theme(axis.title.y = element_text(color = 'blue')) + 
  ylab("Max Trans per Acct") + 
  xlim(0,600)
g1


#  By amount
# customer plot
g1 = ggplot(plotInact, aes(x = Count, y = reorder(TPC, Count), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=Count+3, label=format(round(TotalTrans, 0), big.mark = ','), hjust=0)) + 
  
  scale_fill_manual(values = easternPal) +
  
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Deposit Profiles of 2,724 Least Active Customers") +
  theme(legend.position = 'bottom') + 
  facet_wrap(~Direction) + 
  theme(axis.title.y = element_text(color = 'blue')) + 
  ylab("Max Trans per Acct") +
  xlim(0,800)
g1


######################  look at Q4 



# set inactive to Q4
q4 = inactive %>% filter(Month_Post %in% c('Oct', 'Nov', 'Dec'))


inactRollup = q4 %>% select(Cust_CIS_Nbr, BusType, TotAmt, TransperAcct) %>% 
  group_by(Cust_CIS_Nbr, BusType) %>% 
  summarise(Total = sum(TotAmt),
            TPC = max(TransperAcct))

# describe transactions
inactRollup$Direction = ifelse(inactRollup$Total <= 0, 'Negative', 'Positive')


# group for plotting
plotInact = inactRollup %>% group_by(BusType, Direction, TPC) %>% 
  summarise(TotalTrans = sum(Total),
            Count = n())

plotInact$TPC = ordered(plotInact$TPC, levels = c(1,2,3,4,5))


# customer plot
g1 = ggplot(plotInact, aes(x = Count, y = TPC, fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=Count+3, label=format(Count, big.mark = ','), hjust=0)) + 
  
  scale_fill_manual(values = easternPal) +
  
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Q4 Deposit Profiles of 1,362 Least Active Customers") +
  theme(legend.position = 'bottom') + 
  facet_wrap(~Direction) + 
  theme(axis.title.y = element_text(color = 'blue')) + 
  ylab("Max Trans per Acct") + 
  xlim(0,600)
g1


#  By amount
# customer plot
g1 = ggplot(plotInact, aes(x = Count, y = TPC, fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=Count+3, label=format(round(TotalTrans, 0), big.mark = ','), hjust=0)) + 
  
  scale_fill_manual(values = easternPal) +
  
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Q4 Deposit Profiles of 1,362 Least Active Customers") +
  theme(legend.position = 'bottom') + 
  facet_wrap(~Direction) + 
  theme(axis.title.y = element_text(color = 'blue')) + 
  ylab("Max Trans per Acct") +
  xlim(0,800)
g1


########################## Do the Q4 Transactions

# look at transaction nums
q4Trans = inactive %>% group_by(Transaction_Descr, BusType) %>% 
  filter(Month_Post %in% c('Oct', 'Nov', 'Dec')) %>% 
  summarise(TotalTrans = n(),
            TotAmt = sum(TotAmt)) %>% 
  arrange(desc(TotalTrans))
  
  
# get transaction counts over 2020
busInact <- q4Trans %>% filter(BusType == 'Business') %>% head(20)

# do for commercial
commInact <- q4Trans %>% filter(BusType == 'Commercial') %>% 
  head(20)  



### this does the counts
#   arrange(desc(TotalTrans))
g1 = ggplot(busInact, aes(x = TotalTrans, y = reorder(Transaction_Descr, TotalTrans), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=TotalTrans+3, label=format(TotalTrans, big.mark = ','), hjust=0)) + 
  
  scale_fill_manual(values = easternPal) +
  
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Business") +
  
  
  # theme(axis.title.y = element_text(color = 'blue')) + 
  scale_x_continuous(labels = comma, limits = c(0, 1500))


#   arrange(desc(TotalTrans))
g2 = ggplot(commInact, aes(x = TotalTrans, y = reorder(Transaction_Descr, TotalTrans), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=TotalTrans+3, label=format(TotalTrans, big.mark = ','), hjust=0)) + 
  
  scale_fill_manual(values = 'sienna1') +
  
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Commercial") +
  # theme(legend.position = 'bottom') + 
  
  # theme(axis.title.y = element_text(color = 'blue')) + 
  scale_x_continuous(labels = comma, limits = c(0, 100))

grid.arrange(g1, g2, ncol=2)


########    LAYER IN THE TRANSACTION AMTS

### this does the counts
#   arrange(desc(TotalTrans))
g1 = ggplot(busInact, aes(x = TotalTrans, y = reorder(Transaction_Descr, TotalTrans), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=TotalTrans+3, label=format(TotAmt, big.mark = ',', scientific = F, digits = 0), hjust=0)) + 
  
  scale_fill_manual(values = easternPal) +
  
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Business") +
  
  
  # theme(axis.title.y = element_text(color = 'blue')) + 
  scale_x_continuous(labels = comma, limits = c(0, 3000))


#   arrange(desc(TotalTrans))
g2 = ggplot(commInact, aes(x = TotalTrans, y = reorder(Transaction_Descr, TotalTrans), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=TotalTrans+3, label=format(TotAmt, big.mark = ',', scientific = F, digits = 0), hjust=0)) + 
  
  scale_fill_manual(values = 'sienna1') +
  
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Commercial") +
  # theme(legend.position = 'bottom') + 
  
  # theme(axis.title.y = element_text(color = 'blue')) + 
  scale_x_continuous(labels = comma, limits = c(0, 200))

grid.arrange(g1, g2, ncol=2)



