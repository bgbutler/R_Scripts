

#libraries for reshaping and plotting data
library(ggplot2)
library(reshape2)
library(dplyr)
library(caret)
library(circular)
library(plotrix)
library(readxl)


#load the file
url <- "K:/Sandbox/Bryan Projects/CQ/CQLevers.xlsx"
cqDataRaw <- read_excel(url, sheet = 2, col_names = TRUE,
                        col_types = NULL, na = "", skip = 0)

#check scaling by converting GoodToToal to full percentage
cqDataRaw$GoodPercent <- cqDataRaw$GoodToTotal*100


model1 <- lm(data = cqDataRaw, CQScore ~ Industry + Dimensions + GoodToTotal-1)
summary(model1)
plot(model1)

model2 <- lm(data = cqDataRaw, CQScore ~ Industry2 + ExcessCQ + GoodToTotal)
summary(model2)
plot(model2)

model3 <- lm(data = cqDataRaw, CQScore ~ IndustryCQ + Dimensions + GoodPercent)
summary(model3)
plot(model3)

model4 <- lm(data = cqDataRaw, CQScore ~ IndustryConstant + Openness + 
               Empathy + Relevance + CX + EmotionalValidation + GoodPercent-1)
summary(model4)


#rescale to account for the fact that the dimensions are aggregates
model5 <- lm(data = cqDataRaw, CQScore ~ IndustryConstant + Openness + 
               I(Empathy/3) + I(Relevance/3) + I(CX/4) + I(EmotionalValidation/4) + GoodPercent-1)
summary(model5)


