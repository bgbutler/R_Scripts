fileUrl <- "http://www.esrl.noaa.gov/psd/enso/mei.ext/rank.ext.html"
download.file(fileUrl, destfile="/Users/Bryan/Documents/RFiles/enso.txt", method="curl")


data <-read.csv("/Users/Bryan/Documents/RFiles/enso.csv", header=TRUE, sep=",")
head(data)

data <-read.delim("/Users/Bryan/Documents/RFiles/enso.txt", header=TRUE, sep=" ", fill = TRUE, comment.char="")
head(data)


# This code generated simulated losses and then applies a RI structure

loss <- rnorm(100,10, 5) # Generate the losses

#Generate the function to calculate recoveries
recov <- function(loss, limit, cession = 1, attach) {
	max(0,min(limit,cession*(loss - attach)))
}

#Generate the function to remove negatives from the losses
removeneg <- function(x) {
	if (x<0) x=0 
	else  x	= x
}

gross <- sapply(loss, removeneg)  #Remove negative losses

#Calculate recoveries
recovery <- sapply(gross, function(gross) recov(loss=gross, limit=5, attach=10))

net <- gross - recovery # Calculate net losses

#Make the final data set Gross, Ceded, Net
grossnet <- matrix(data=cbind(gross, recovery, net), nrow=100, ncol=3)
colnames(grossnet) <- c("Gross", "Ceded", "Net")
grossnet



# This batch of code is for extracting teams and scores from the web page

fileUrl <- "http://espn.go.com/nfl/team/_/name/ne/new-england-patriots"
	doc <- htmlTreeParse(fileUrl, useInternal=TRUE)
	scores <- xpathSApply(doc, "//li[@class='score']", xmlValue)
	teams <- xpathSApply(doc,"//li[@class='team-name']", xmlValue)
	schedule <- matrix(data=cbind(teams,scores), nrow=18, ncol=2)
schedule