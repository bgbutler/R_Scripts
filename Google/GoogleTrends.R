
# google trends

library(tidyverse)
library(DT)
library(reshape2)
library(RColorBrewer)
library(ggplot2)
library(knitr)
library(plotly)

library(scales)
library(htmltools)
library(plotly)
library(dplyr)

# set wd
# os.chdir('C:\\Users\\bbutler\\Documents\\DigitalAnalytics')
setwd('C:/Users/bbutler/Documents/DigitalANalytics/')

# get the file
trends <- readr::read_csv('heloc_trends.csv')

# check the structure
str(trends)

# rename columns
colnames(trends) <- c('date', 'home_equity_credit_line', 'heloc', 'home_equity_rate',
                     'heloc_loan', 'equity_loan', 'home_equity', 'DCU_home_equity_loan',
                     'Citizens_home_equity_loan', 'Citizens_bank_HELOC')
head(trends)

base <-  trends %>% select(date, heloc, home_equity_credit_line, home_equity_rate, heloc_loan, equity_loan, home_equity)

expensive <-  trends %>% select(date,home_equity_credit_line, heloc, home_equity_rate)

competitors <- trends %>% select(date,home_equity_credit_line, Citizens_bank_HELOC, Citizens_home_equity_loan,
                                 DCU_home_equity_loan)

# melt the data for plotting
trendsMelt <- melt(trends, id = c('date'),
                   variable.name = "Keyword",
                   value.name = "RelImp")



# use this theme
theme_bryan <- function () { 
  theme(axis.text.x = element_text(size = 8, color = 'blue', angle = 0),
        legend.position = 'bottom',
        axis.text.y = element_text(color = 'blue'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(color = 'darkblue', size = 10, face = 'bold'),
        strip.background = element_rect(fill = 'light blue'))
}



# make a plot
#this is the density of product ownership
# basic boxplot
b <- ggplot(trendsMelt, aes(x = Keyword, y = RelImp, fill = Keyword)) + 
  geom_boxplot(notch=TRUE) + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan() + 
  theme(axis.text.x = element_text(size = 12, color = 'blue', angle = 45, hjust = 1))

b

# interactive one
g <- ggplot(trendsMelt, aes(x = Keyword, y = RelImp, fill = Keyword)) + 
  geom_boxplot() + 
  stat_summary(fun="mean", geom="point", shape = 23, size = 3, fill = "white") + 
  theme_bryan()

g

ggplotly(g, tooltip = c("x", "y"))



# get line plot
g <- ggplot(trendsMelt, aes(x = date, y = RelImp, color = Keyword)) + 
  geom_line() + facet_wrap(~Keyword, ncol = 2, scales = "free") +
  theme_bryan() + 
  scale_x_date(date_breaks = "month" , date_labels = "%m-%y") + 
  theme(legend.position = ' ') + 
  theme(axis.text.x = element_text(angle = 45)) + 
  theme(panel.margin.x = unit(4, "lines"))

# get the plotting parameters
params <- ggplot_build(g)$layout$panel_params[[1]][c("x.range","y.range")]
params <- unlist(params)
y_pos <- as.numeric(params[3])
y_pos <- y_pos

ggplotly(g, tooltip = c("x", "y")) %>% layout(showlegend = FALSE)







# perform seasonal decomp on baseline
heloc <- base %>% select(date, heloc)
helocTs<- ts(heloc, frequency=1, start=c(2019,1,6))
helocComp <- decompose(helocTs, "additive")
plot(as.ts(helocComp$seasonal))
plot(as.ts(helocComp$trend))
plot(as.ts(helocComp$random))
plot(helocComp)

plot(helocComp)










