
# this is for 30 to 90 day claims

## @knitr getData
# make sure that the directories are set
getwd()
setwd('N:/Bryan/Markov')

library(RODBC)
library(plyr)
library(dplyr)
library(markovchain)
library(DT)
library(reshape2)
library(knitr)
library(ggplot2)
library(scales)
library(PerformanceAnalytics)
library(readxl)






# get what we need for markov analysis
# also includes other data
# claim number and snapshot and value
# sort on claim number and then snapshot
data <- ss %>% arrange(
  CLM_NBR, CLM_FTR_NBR, SNAPSHOT_DAY)

# rm(ss)


## @knitr cleanCols
# rename and clean up
# change the column names
colnames(data) <- c('ClmNbr',
                    'FeatureNbr',
                    'SnapshotDay',
                    'IncSnapshotDay',
                    'FinalPaid',
                    'AttyRepresent',
                    'BILimitPerson',
                    'InjuryType',
                    'ClmtAgeAtLoss',
                    'InsScore',
                    'StateRated',
                    'VehYear',
                    'ClmtStResidence',
                    'FaultPercentage',
                    'FaultCode',
                    'AccdntZip',
                    'AccdntState')


# change ClmNbr to character
data$ClmNbr <- as.character(data$ClmNbr)


# Make a generation column
data$Generation  <- cut(data$ClmtAgeAtLoss,
                    c(0,21,38,54,200), include.lowest = F,
                           labels = c('Gen Z', 'Millennials', 'Gen X', 'Boomers+'))


# make the reason codes
data$FaultPercentage <- as.character(data$FaultPercentage)
data$FaultLvl <- ifelse(data$FaultPercentage == '1G', 'Ins at fault > 50%',
                        ifelse(data$FaultPercentage == '2G', 'Ins at fault < 50%',
                        ifelse(data$FaultPercentage == '3G', 'Ins not at fault',
                        ifelse(data$FaultPercentage == '4G', 'Ins at fault 50%','NA'))))



# count of na
datatable(data %>% group_by(SnapshotDay) %>%
            summarise(count = sum(is.na(IncSnapshotDay))))


# clean up attyrepresent
data$AttyRepresent <- as.character(data$AttyRepresent)
data$AttyRepresent <- as.factor(
  ifelse(is.na(data$AttyRepresent), 'Y', data$AttyRepresent))

# change day seven NAs to zero
data$IncSnapshotDay <- ifelse(is.na(data$IncSnapshotDay) & data$SnapshotDay ==7,0, data$IncSnapshotDay)


# cut the claims final paid into bins
data$FinalBins  <- cut(data$FinalPaid,
                         c(0,1,5000,10000,15000,20000,25000, 1000000), include.lowest = T,
                         labels = c('0', '0-05K', '05-10K', '10-15K', '15-20K', '20-25K','25K+'))


# create a MA split
data$MA <- ifelse(as.character(data$StateRated) == 'MA', 'MA', 'xMA')
data$MA <- as.factor(data$MA)

# make key state split
data$keyState <- ifelse(as.character(data$StateRated) == 'MA', 'MA',
                             ifelse(as.character(data$StateRated) == 'NY', 'NY',
                                    ifelse(as.character(data$StateRated) == 'NJ', 'NJ',
                                           'Other')))
data$keyState <- as.factor(data$keyState)

# Clean the insurance score
data$cleanScore <- ifelse(data$InsScore <= -5, NA, 
                               ifelse(data$InsScore %in% c(-4, -3, -2, -1), 0,
                                      data$InsScore))

# cut the claims at snapshot day into bins
data$SnapshotBins  <- cut(data$IncSnapshotDay,
                              c(0,1,5000,10000,15000,20000,25000, 1000000), include.lowest = T,
                              labels = c('0', '0-05K', '05-10K', '10-15K', '15-20K', '20-25K','25K+'))



# Get a view of claims by zip
datatable(data %>% dplyr::count(AccdntZip, sort = T))


# Set up plotting
theme_bryan <- function () { 
  theme(axis.text.x = element_text(size = 12, color = 'Dark Blue'),
        legend.position = 'bottom',
        axis.text.y = element_text(size = 12, color = 'Dark Blue'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size = 12, face = 'bold'),
        strip.background = element_rect(fill = 'light blue'))
}

hanover <- c('green', 'orange', 'light blue', 'grey', 'blue')


# load the reason codes and map the values
codes <- read_excel('reasonCodes.xlsx', sheet = 1, col_names = T)

# map the codes to the AIA 1 and 2
data$ReasonCodes <- plyr::mapvalues(data$FaultCode, codes$Code, codes$Reason)

# Clean up the injury codes
data$InjuryType <- as.character(data$InjuryType)
# clean up data
data$InjuryType <- ifelse(data$InjuryType == 'mentaldisorder', 'psych',data$InjuryType)
data$InjuryType <- ifelse(data$InjuryType == 'mentalstress', 'psych', data$InjuryType)
data$InjuryType <- ifelse(data$InjuryType == 'otherpsy', 'psych', data$InjuryType)
data$InjuryType <- ifelse(data$InjuryType == 'hernia', 'other', data$InjuryType)
data$InjuryType <- ifelse(data$InjuryType == 'stroke', 'other', data$InjuryType)
data$InjuryType <- ifelse(data$InjuryType == 'visionloss', 'other', data$InjuryType)
data$InjuryType <- ifelse(data$InjuryType == 'paralysis', 'other', data$InjuryType)
data$InjuryType <- ifelse(data$InjuryType == 'rupture', 'other', data$InjuryType)
data$InjuryType <- ifelse(data$InjuryType == 'foreignbody', 'other', data$InjuryType)
data$InjuryType <- ifelse(data$InjuryType == 'amputationseverance', 'other', data$InjuryType)
data$InjuryType <- ifelse(data$InjuryType == 'paralysis', 'other', data$InjuryType)
data$InjuryType <- ifelse(data$InjuryType == 'lossofconsciousness', 'other', data$InjuryType)
data$InjuryType <- ifelse(data$InjuryType == 'bruisecontusion', 'bruise', data$InjuryType)
data$InjuryType <- ifelse(data$InjuryType == 'closedheadinjury', 'headinjury', data$InjuryType)
data$InjuryType <- ifelse(data$InjuryType == 'cutlaceration', 'laceration', data$InjuryType)
data$InjuryType <- ifelse(data$InjuryType == 'sprainstrain', 'sprain', data$InjuryType)
data$InjuryType <- as.factor(data$InjuryType)

data$FaultLvl <- ordered(data$FaultLvl, levels = c('Ins not at fault',
                                                   'Ins at fault < 50%',
                                                   'Ins at fault 50%',
                                                   'Ins at fault > 50%'))

##########  FINISH CLEANUP


# use the 90 day snapshot as the basis for plotting
data90 <- data %>% filter(SnapshotDay == 90 & !is.na(FinalPaid))


## @knitr histogram1
g <- ggplot(data90, aes(x = ClmtAgeAtLoss, fill = AttyRepresent)) + 
  geom_histogram(bins=70, alpha = .8) + 
  scale_fill_manual(values = hanover) + 
  scale_x_continuous(labels = comma, limits = c(0,100)) + 
  facet_wrap(~AttyRepresent, ncol = 1, scales = 'free') + 
  theme_bryan()
g


## @knitr density
g <- ggplot(data90, aes(x = ClmtAgeAtLoss, fill = AttyRepresent)) + 
  geom_density(alpha = 0.8) + 
  scale_fill_manual(values = hanover) + 
  scale_x_continuous(labels = comma, limits = c(0,100)) + 
  facet_wrap(~AttyRepresent) + 
  theme_bryan()
g

# boxplot by attny present
# use 100K to get better picture of the average
## @knitr atttnyBoxplot
g <- ggplot(data90, aes(x = AttyRepresent, y = FinalPaid,
                           fill = AttyRepresent)) +  
  scale_fill_manual(values = hanover) + 
  geom_boxplot(notch = T) + 
  scale_y_continuous(labels = comma, limits = c(0,100000)) + 
  coord_flip() +
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') + 
  theme_bryan()

g


# boxplot by injury
## @knitr injuryBoxplot
g <- ggplot(data90, aes(x = reorder(InjuryType, -FinalPaid, mean, order = T), y = FinalPaid,
                            fill = InjuryType)) +  
  geom_boxplot(notch = F) + 
  scale_y_continuous(labels = comma, limits = c(0,100000)) + 
  coord_flip(expand = T) + 
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') +
  facet_wrap(~AttyRepresent) + 
  theme_bryan()

g


## @knitr ageGroups
# check age groups in the bins
g <- ggplot(data90, aes(x = FinalBins, y = ClmtAgeAtLoss,
                            fill = FinalBins)) +  
  geom_boxplot(notch = T) + 
  ylim(20,70) + coord_flip() + 
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') + 
  theme_bryan() + theme(legend.position = 'bottom')
g

## @knitr addGenerations
# Add generation to the mix
data90Age <- data %>% filter(!is.na(FinalBins) & !is.na(Generation) & SnapshotDay == 90)

portion <- data90Age %>% select(FinalBins, FinalPaid, Generation) %>%
                         filter(FinalBins != '0')

grouped <- portion %>% group_by(FinalBins, Generation) %>% 
                              tally(FinalPaid)
colnames(grouped) <- c('FinalBins', 'Generation', 'TotClaims')
grouped$Percent <- grouped$TotClaims/sum(grouped$TotClaims) * 100

g <- ggplot(grouped, aes(x = FinalBins, y = TotClaims, fill = Generation)) +  
  geom_bar(stat='identity', position=position_dodge()) + 
  scale_fill_manual(values = hanover) +
  scale_y_continuous(labels = comma) +
  coord_flip() + 
  theme_bryan() + theme(legend.position = 'bottom')
g



## @knitr generationBox
# Claims by generation
data90Age <- data90 %>% filter(!is.na(Generation))
g <- ggplot(data90Age, aes(x = Generation, y = FinalPaid,
                            fill = Generation)) +  
  scale_fill_manual(values = hanover) + 
  geom_boxplot(notch = T) + 
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') + 
  scale_y_continuous(labels = comma, limits = c(0,100000)) + 
  coord_flip() +
theme_bryan()
g


## @knitr generationAtty
# Take Attny into account
g <- ggplot(data90Age, aes(x = Generation, y = FinalPaid,
                         fill = AttyRepresent)) +  
  scale_fill_manual(values = hanover) + 
  geom_boxplot(notch = T) + 
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') + 
  scale_y_continuous(labels = comma, limits = c(0,100000)) + 
  coord_flip() +
  facet_wrap(~AttyRepresent, ncol = 1)
  theme_bryan()
g
            



## @knitr maClaims
# check the claims for MA and other
maAgg <- data90 %>% select(
                          ClmNbr,
                          FinalPaid,
                          MA) %>%
                    group_by(ClmNbr, MA) %>%
                    summarise(TotClaims = sum(FinalPaid))



g <- ggplot(maAgg, aes(x = MA, y = TotClaims,
                            fill = MA)) +  
  geom_boxplot(notch = T) + 
  scale_fill_manual(values = hanover) +
  scale_y_continuous(labels = comma, limits = c(0,100000)) +
  coord_flip() + 
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') + 
theme_bryan() + theme(legend.position = 'bottom')
g


# key states
stateAgg <- data90 %>% select(
  ClmNbr,
  FinalPaid,
  keyState) %>%
  group_by(ClmNbr, keyState) %>%
  summarise(TotClaims = sum(FinalPaid))


g <- ggplot(stateAgg, aes(x = keyState, y = TotClaims,
                            fill = keyState)) +  
  geom_boxplot(notch = T) + 
  scale_fill_manual(values = hanover) +
  scale_y_continuous(labels = comma, limits = c(0,100000)) +
  coord_flip() + 
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') + 
  theme_bryan() + theme(legend.position = 'bottom')
g


## @knitr state

stateData <- data90 %>% filter(FinalBins != '0')

g <- ggplot(stateData, aes(x = reorder(StateRated, FinalPaid, mean),
                           y = FinalPaid, fill = AttyRepresent)) +  
  scale_fill_manual(values = hanover) +
  geom_boxplot(notch = F) + 
  coord_flip() +
  scale_y_continuous(labels = comma, limits = c(0,100000)) + 
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') +
  theme_bryan() + theme(legend.position = '')
g

## @knitr state2
stateDataClean <- stateData %>% filter(!is.na(Generation))
g <- ggplot(stateDataClean, aes(x = reorder(StateRated, FinalPaid, mean),
                                y = FinalPaid, fill = Generation)) +  
  geom_boxplot(notch = F) + 
  scale_fill_manual(values = hanover) +
  coord_flip() +
  scale_y_continuous(labels = comma, limits = c(0,100000)) + 
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') +
  # facet_wrap(~Generation, ncol = 2) +
  theme_bryan() + theme(legend.position = 'right')
g



# look at the at fault codes
## @knitr faultGen
data90Gen <- data90 %>%  filter(!is.na(Generation))
data90Gen$FinalPaid <- ifelse(data90Gen$FinalPaid <0, 0, data90Gen$FinalPaid)
g <- ggplot(data90Gen, aes(x = FaultLvl, y = FinalPaid,
                        fill = Generation)) +  
  geom_boxplot(notch = T) + 
  coord_flip() + 
  scale_fill_manual(values = hanover) +
  scale_y_continuous(labels = comma, limits = c(0,50000)) + 
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') + 
  # facet_wrap(~Generation) +
  theme_bryan() + theme(legend.position = 'bottom')
g


## @knitr reasonGen
g <- ggplot(data90Gen, aes(x = ReasonCodes, y = FinalPaid,
                           fill = Generation)) +  
  geom_boxplot(notch = F) + 
  coord_flip() + 
  scale_fill_manual(values = hanover) +
  scale_y_continuous(labels = comma, limits = c(0,50000)) + 
  stat_summary(fun.y='mean', geom='point', shape = 23, size = 3, fill = 'white') + 
  # facet_wrap(~Generation) +
  theme_bryan() + theme(legend.position = 'bottom')
g


## @knitr reasonCount
g <- ggplot(data90Gen, aes(x = reorder(ReasonCodes, ReasonCodes, function(x) length(x)), fill = Generation)) +  
  geom_bar(stat = 'count') + 
  coord_flip() + 
  scale_fill_manual(values = hanover) +
  theme_bryan() + theme(legend.position = 'bottom')
g


## @knitr sevenToThirty
# data for MarkovChains
# seven to 30
sevenToThirty <- data %>% filter(SnapshotDay == 7 | SnapshotDay == 30) %>% 
  select(
    ClmNbr,
    FeatureNbr,
    SnapshotDay,
    SnapshotBins)

# widen the data
ClaimStates <- dcast(sevenToThirty, ClmNbr + FeatureNbr ~ SnapshotDay, value.var='SnapshotBins')
head(ClaimStates)


# make the markov chains
# fit the markov chain to get the transition matrix
mcClaims <- markovchainListFit(data = ClaimStates[,3:4], name = "ClaimTransitions")
mcClaims


## @knitr thirtyToNinety
thirtyToNinety <- data %>% filter(SnapshotDay == 30 | SnapshotDay == 90) %>% 
                select(
                ClmNbr,
                FeatureNbr,
                SnapshotDay,
                SnapshotBins)

# widen the data
ClaimStates <- dcast(thirtyToNinety, ClmNbr + FeatureNbr ~ SnapshotDay, value.var='SnapshotBins')
head(ClaimStates)


# make the markov chains
# fit the markov chain to get the transition matrix
mcClaims <- markovchainListFit(data = ClaimStates[,3:4], name = "ClaimTransitions")
mcClaims



## @knitr ninetyToFinal
# go from 90 to final
ninetyToFinal <- data %>% filter(
  SnapshotDay ==90) %>%
  select(
    ClmNbr,
    FeatureNbr,
    SnapshotBins,
    FinalBins
  )


mcClaims90Final <- markovchainListFit(data = ninetyToFinal[,3:4], name = "ClaimTransitions")
mcClaims90Final  

## @knitr sevenToEnd
# from seven to final 
sevenToFinal <- data %>% filter(
  SnapshotDay ==7) %>%
  select(
    ClmNbr,
    FeatureNbr,
    SnapshotBins,
    FinalBins
  )


mcClaims7Final <- markovchainListFit(data = sevenToFinal[,3:4], name = "ClaimTransitions")
mcClaims7Final              
              