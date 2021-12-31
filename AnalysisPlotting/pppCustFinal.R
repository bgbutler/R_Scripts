



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
prePPP = read_csv('pppWave1CustProspectCheck.csv')
head(prePPP)


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


# change Paid_Status
# names(df)[names(df) == 'old.var.name'] <- 'new.var.name'
prePPP$Paid_Status = ifelse(prePPP$Paid_Status == 'Paid', 'Closed', prePPP$Paid_Status)


prePPP$Paid_Status = ordered(prePPP$Paid_Status, levels = c('Open', 'Closed'))


# group for plotting
custgp = prePPP%>% group_by(BusType, Cust_Status, Paid_Status) %>% 
  summarise(Count = n())
  
# make a percent column
tot = prePPP %>% group_by(Paid_Status) %>% 
  summarise(Tot = n())

custgp$Percent = ifelse(custgp$Paid_Status == "Open", 100*custgp$Count/tot$Tot[1], 100*custgp$Count/tot$Tot[2])


# customer 
g1 = ggplot(custgp, aes(x = Count, y = reorder(Cust_Status, Count), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=Count+3, label=format(Count, big.mark = ','), hjust=0)) + 
  
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Customer Profile of PPP Customers (7,466 Open, 1,422 Closed)") +
  facet_wrap(~Paid_Status) + 
  scale_x_continuous(labels = comma, limits = c(0, 3200)) + 
  theme(legend.position = 'right')
g1


# customer 
g1 = ggplot(custgp, aes(x = Percent, y = reorder(Cust_Status, Percent), fill = BusType)) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  
  geom_text(position = position_dodge(width= 1),
            size = 4, color = 'blue',
            aes(x=Percent+3, label=paste0(format(Percent, digits = 0),"%"), hjust=0)) + 
  
  scale_fill_manual(values = easternPal) +
  theme_bryan() + 
  theme(plot.title = element_text(size = rel(1.5), face = "bold.italic", color = "blue")) +
  ggtitle("Customer Profile of PPP Customers as Percentage of Open/Closed") +
  facet_wrap(~Paid_Status) + 
  scale_x_continuous(labels = comma, limits = c(0, 60)) + 
  theme(legend.position = 'right')
g1


