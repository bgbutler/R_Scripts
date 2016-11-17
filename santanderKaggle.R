

# this is for the kaggle competition 

library(dplyr)
library(data.table)
library(ggplot2)
library(reshape2)
library(scales)
library(markovchain)
library(scales)

#get the data use fread and data table to control input
url <- "~/Desktop/RDataFiles/CSVs/train_ver2.csv"
#data <- read.csv(url, header = TRUE, sep=",", na.strings = 0,as.is = FALSE)

set.seed(1000)

df <- fread("~/Desktop/RDataFiles/CSVs/train_ver2.csv", nrows = -1)

#clean the date field
df$fecha_dato <- as.Date(df$fecha_dato, format = "%Y-%m-%d")

#create a month field
df$Month <- as.character(df$fecha_dato, format = "%b-%Y")
df$Month <- ordered(df$Month, 
                    levels = c("Jan-2015", "Feb-2015", "Mar-2015", "Apr-2015", "May-2015", 
                             "Jun-2015", "Jul-2015", "Aug-2015", "Sep-2015", "Oct-2015",
                             "Nov-2015", "Dec-2015", "Jan-2016", "Feb-2016", "Mar-2016", 
                             "Apr-2016", "May-2016"))




#check for missing columns
sapply(df, function(x)any(is.na(x)))

#find the number of unique users
#there are 956645 unique users
length(unique(df$ncodpers))


#check the format of the data
test <- filter(df, ncodpers == 1050611)

#set up some common plotting themes
my_theme <- theme_bw() +
  theme(axis.title=element_text(size=24),
        plot.title=element_text(size=36),
        axis.text =element_text(size=16))

my_theme_dark <- theme_dark() +
  theme(axis.title=element_text(size=24),
        plot.title=element_text(size=36),
        axis.text =element_text(size=16))

#plot the age distribution
#bimodal distribution heavy with university students
ggplot(data=df,aes(x=age)) + 
  geom_bar(alpha=0.75,fill="tomato",color="black") +
  scale_y_continuous(labels = scales::comma) + 
  ggtitle("Age Distribution") + 
  my_theme

#create a subset dataframe to work with only the products
df$segmento <- as.factor(df$segmento)

#turn indrel into a factor
df$indrel <- as.character(df$indrel)

#get a reduced data set to do some testing
dfRed <- select(df,
                Month,
                fecha_dato,
                ncodpers,
                segmento,
                ind_ahor_fin_ult1,
                ind_aval_fin_ult1,
                ind_cco_fin_ult1,
                ind_cder_fin_ult1,
                ind_cno_fin_ult1,
                ind_ctju_fin_ult1,
                ind_ctma_fin_ult1,
                ind_ctop_fin_ult1,
                ind_ctpp_fin_ult1,
                ind_deco_fin_ult1,
                ind_deme_fin_ult1,
                ind_dela_fin_ult1,
                ind_ecue_fin_ult1,
                ind_fond_fin_ult1,
                ind_hip_fin_ult1,
                ind_plan_fin_ult1,
                ind_pres_fin_ult1,
                ind_reca_fin_ult1,
                ind_tjcr_fin_ult1,
                ind_valo_fin_ult1,
                ind_viv_fin_ult1,
                ind_nomina_ult1,
                ind_nom_pens_ult1,
                ind_recibo_ult1)


class(dfRed$segmento)
table(dfRed$segmento)

#remove the original to free up space
rm(df)


#find the max number of products each held by month
allProducts <- dfRed %>% group_by(ncodpers, segmento, Month) %>% 
  summarise(
        ind_ahor_fin_ult1 = max(ind_ahor_fin_ult1),
        ind_aval_fin_ult1 = max(ind_aval_fin_ult1),
        ind_cco_fin_ult1 = max(ind_cco_fin_ult1),
        ind_cder_fin_ult1 = max(ind_cder_fin_ult1),
        ind_cno_fin_ult1 = max(ind_cno_fin_ult1),
        ind_ctju_fin_ult1 = max(ind_ctju_fin_ult1),
        ind_ctma_fin_ult1 = max(ind_ctma_fin_ult1),
        ind_ctop_fin_ult1 = max(ind_ctop_fin_ult1),
        ind_ctpp_fin_ult1 = max(ind_ctpp_fin_ult1),
        ind_deco_fin_ult1 = max(ind_deco_fin_ult1),
        ind_deme_fin_ult1 = max(ind_deme_fin_ult1),
        ind_dela_fin_ult1 = max(ind_dela_fin_ult1),
        ind_ecue_fin_ult1 = max(ind_ecue_fin_ult1),
        ind_fond_fin_ult1 = max(ind_fond_fin_ult1),
        ind_hip_fin_ult1 = max(ind_hip_fin_ult1),
        ind_plan_fin_ult1 = max(ind_plan_fin_ult1),
        ind_pres_fin_ult1 = max(ind_pres_fin_ult1),
        ind_reca_fin_ult1 = max(ind_reca_fin_ult1),
        ind_tjcr_fin_ult1 = max(ind_tjcr_fin_ult1),
        ind_valo_fin_ult1 = max(ind_valo_fin_ult1),
        ind_viv_fin_ult1 = max(ind_viv_fin_ult1),
        ind_nomina_ult1 = max(ind_nomina_ult1),
        ind_nom_pens_ult1 = max(ind_nom_pens_ult1),
        ind_recibo_ult1 = max(ind_recibo_ult1))

#sum across the products to get an idea of max holdings
allProducts$Total <- rowSums(allProducts[,c(4:27)])


#this is the bar plot of product ownership
g <- ggplot(allProducts, aes(x = Total, fill = segmento)) + 
  geom_histogram(binwidth = 1) + scale_y_continuous(labels = comma) + 
  scale_x_continuous(limits = c(0,10), breaks = c(0,1,2,3,4,5,6,7,8,9,10)) + 
  facet_wrap(~Month) + 
  theme(axis.text.x = element_text(size = 8, color = "red"),
        legend.position = "bottom",
        axis.text.y = element_text(color = "red"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold"),
        strip.background = element_rect(fill = "red")) +
  theme(plot.title = element_text(color = "red"))
g


#melt and reorder
#drop the id
allProducts <- allProducts[-1]

meltProducts <- melt(allProducts, id = c("segmento", "Month"),
                     variable.name = "Account",
                     value.name = "Count")



#plot by product by month
g <- ggplot(meltProducts, aes(x = reorder(Account, Count), y = Count, fill = Account)) + 
  geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) + 
  facet_wrap(~Month) + 
  theme(axis.text.x = element_text(size = 8, color = "red"),
        legend.position = "right",
        axis.text.y = element_text(color = "red"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size = 10, face = "bold"),
        strip.background = element_rect(fill = "red")) +
  theme(plot.title = element_text(color = "red"))
g








