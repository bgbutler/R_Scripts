#Load_Data

filename = "ARL PreFX RO.csv"
#filename = "DataFileRunoff(ARL).csv"
#filename = "DataFileNB(ASEL).csv"
#filename = "DataFileRunoff(ASEL).csv"
#filename = "DataFileNB(Group).csv"
#filename = "DataFileRunoff(Group).csv"
#filename = "ASEL NB - Natural PreFX.csv"

XX1<-read.csv(filename)
samples <- 100000

#Percentiles to use (starting, reduce by 25% each step)
upper <- 1.0
lower <- .75


#format list of titles
nameList = names(XX1)
nameList = sub("-_", "", nameList)
nameList = sub("_", "\n", nameList)
nameList = sub(" ", "\n", nameList)

classes <- length(nameList)

Results = array(0, dim=c(4,classes,classes))


## Construct the data
for (l in 1:4){

    # dependent and independent thresholds
    indep <- (upper-lower)^2
    dep <- (upper-lower)
    
    for( k in 1:classes) {
          for( j in 1:classes) {
            dataA <- XX1[,j]
            dataB <- XX1[,k]
            
            quanAu <- quantile(dataA, upper)
            quanAl <- quantile(dataA, lower)
            quanBu <- quantile(dataB, upper)
            quanBl <- quantile(dataB, lower)
            count <- 0
            countA <- 0
            countB <- 0
            
            for (i in 1:samples) {
                if (dataA[i]<= quanAu && dataA[i]> quanAl && dataB[i]<=quanBu && dataB[i]>quanBl){
                  count = count +1
                }
            }
            Results[l,j,k] = max(0, count/samples - indep)/(dep-indep)
        }
    }
    
    upper = upper - .25
    lower = lower - .25
}

## Plot the results
par(mfrow=c(classes,classes), mar=c(.0, .0, .0, .0), oma = c(5,5,5,5))
xVals <- c(0.875, 0.625, 0.375, 0.125)
for (a in 1:classes){
  for (b in 1:classes){
    x=matrix(0,4)
    y=matrix(0,4)
    for (c in 1:4){
    	y[c]<-Results[c,a,b]
      x[c]<-xVals[c]
  		}
    
    ## diagonal
    if (b == a) {
      plot(x=0,y=0, 
           type="l", col="red",
           xlab = "", ylab = "", ylim=c(0,1), xlim=c(0,1), xaxt = "n", yaxt = "n",
           mar=c(.1, .1, .1, .1)
           )
              text(0.5, .5, nameList[a], cex = 0.8)
      }
    ## Top Row
    else if (a == 1) {
        plot(x=x,y=y, 
           type="l", col="red",
           xlab = "", ylab = "", ylim=c(0,1), xaxt = "n", yaxt = "n",
           mar=c(.1, .1, .1, .1)
           )      
        if ( (b %% 2) == 0){
          axis(3, labels = c("4","3","2","1"), at = xVals, cex.axis = 0.7)
      }
    }
    
    ## Left Column
    else if (b == 1) {
      plot(x=x,y=y, 
           type="l", col="red",
           xlab = "", ylab = "", ylim=c(0,1), xaxt = "n", yaxt = "n",
           mar=c(.1, .1, .1, .1)
           )
      if ( (a %% 2) == 0){
          axis(2)
        }
    }
    
    ## Other cells
    else {    plot(x=x,y=y, 
         type="l", col="red",
         xlab = "", ylab = "", ylim=c(0,1), xaxt = "n", yaxt = "n",
         mar=c(.1, .1, .1, .1)
         )

    }
    
    mtext(text = "Joint Exceedence Probability Plot", side = 3, outer = TRUE, line = 3)
    mtext(text = filename, side = 3, outer = TRUE, line = 2, cex = par("cex")*1)
    mtext(text = "Percentile Range (0-25, 25-50, 50-75, 75-100)", side = 1, outer = TRUE, line = 1, cex = par("cex")*1)
    mtext(text = "Joint Exceedence Probability ", side = 2, outer = TRUE, line = 3, cex = par("cex")*1)
    
  }
}

