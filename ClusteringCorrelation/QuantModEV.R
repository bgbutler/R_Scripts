# load up the RMySQL package for connecting
require(RMySQL)
require(quantmod)
require(xts)
require(zoo)
require(ggplot2)

analyticsDb <- dbConnect(MySQL(), user="readonly",password= "1227boston", db="analytics",
                         host="internal-db-rr0.cpzuqtj4hq4e.us-east-1.rds.amazonaws.com")


dailyEV <- dbGetQuery(analyticsDb, "select future_pay_date,
cast(AVG(case when policy_type in ('Auto', 'Homeowners') then pre_tax_premium else 0 end) as decimal(7,2)) as Premium_open,
cast(MAX(case when policy_type in ('Auto', 'Homeowners') then pre_tax_premium else 0 end) as decimal(7,2)) as Premium_high,
cast(AVG(case when policy_type in ('Auto', 'Homeowners') then pre_tax_premium * 0.5 else 0 end) as decimal(7,2)) as Premium_low,
cast(AVG(case when policy_type in ('Auto', 'Homeowners') then pre_tax_premium else 0 end) as decimal(7,2)) as Premium_close,
COUNT(case when policy_type in ('Auto', 'Homeowners') then policy_number else 0 end) as Premium_volume,
cast(AVG(case when policy_type in ('Auto', 'Homeowners') then pre_tax_premium else 0 end) as decimal(7,2)) as Premium_settle
from daily_financial
where future_pay_date between '2014-01-01' and '2014-12-31'
 and carrier_sold like 'Safeco'
Group by future_pay_date
"); dbDisconnect(analyticsDb);

#take a look  - this shows the first 10 results
head(dailyEV, 10)

#convert date format to use with xts or zoo
dailyEV$future_pay_date <- as.Date(dailyEV$future_pay_date)
dailyEV$future_pay_date <- as.POSIXlt(dailyEV$future_pay_date)


#drop the date to avoid duplication
Premium <- dailyEV[,c(2:7)]


#function to format the data to two decimals
formatEV <- function(x) {
    format(round(x, 2), nsmall = 2)
}

#sapply the function to key fields
sapply(EV$ev_open, formatEV)
sapply(EV$ev_high, formatEV)
sapply(EV$ev_low, formatEV)
sapply(EV$ev_close, formatEV)
sapply(EV$ev_settle, formatEV)


Premium <- xts(Premium, order.by = dailyEV$future_pay_date)

chartSeries(Premium)
addMACD()
addBBands()

setTA()

chartSeries(EV, major.ticks = 'months')
addVo()

candleChart(Premium)

