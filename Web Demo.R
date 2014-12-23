# This batch of code is for extracting teams and scores from the web page
# Requires XML package to be installed

library(XML)

fileUrl <- "http://espn.go.com/nfl/team/_/name/ne/new-england-patriots"
doc <- htmlTreeParse(fileUrl, useInternal=TRUE)
scores <- xpathSApply(doc, "//li[@class='score']", xmlValue)
teams <- xpathSApply(doc,"//li[@class='team-name']", xmlValue)
schedule <- matrix(data=cbind(teams,scores), nrow=18, ncol=2)
schedule