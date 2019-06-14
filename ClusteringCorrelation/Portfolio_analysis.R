# code for the use of portfolo analytics package

library(PortfolioAnalytics)

#load the data
dataURL <- "~/RDataFiles/CSVs/Portfolio.csv"
allData <- read.csv(dataURL, header = TRUE, as.is = TRUE, sep = "," )

#create the mix vector and col names
states <- colnames(allData)
states <- states[2:46]

mix <- allData[1,2:26]

#remove mix data from allData
ev <- allData[2:110,]

#convert NA to 0
ev[is.na(ev)] <- 0

#creates and equal weight portfolio

port.ev1 <- portfolio.spec(assets = colnames(ev))


#specify the full data set
port.ev <-portfolio.spec(assets = ev, weight_seq = mix)

port.ev <-portfolio.spec(assets = ev)


port.ev <- add.constraint(port.ev, type="box", min = 0, max = .25)
port.ev <- add.constraint(port.ev, type="position_limit", min_pos= 10)
port.ev <- add.objective(port.ev, type="return", name="mean", target=250)
port.ev <- add.constraint(port.ev, typ="full_investment")
port.ev <- add.constraint(port.ev, type="long_only")

rp <- random_portfolios(port.ev, 10000, "sample")

