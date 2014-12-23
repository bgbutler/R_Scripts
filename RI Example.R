# This code sets up a dialog box for entering the parameters

require(tcltk)
#set up the boxes
ri <- tktoplevel()
heading <- tklabel(ri, text = "Reinsurance Program")
l.limit <- tklabel(ri, text = "Limit")
l.cession <- tklabel(ri, text = "Cession")
l.attach <- tklabel(ri, text = "Attachment")
e.limit <- tkentry(ri, width = 10)
e.cession <- tkentry(ri, width = 10)
e.attach <- tkentry(ri, width = 10)

#Align the labesl and the entry boxes
tkgrid(heading, columnspan=2)
tkgrid(l.limit, e.limit)
tkgrid(l.cession, e.cession)
tkgrid(l.attach, e.attach)
tkgrid.configure(l.limit, l.cession, l.attach, sticky="e")
tkgrid.configure(e.limit, e.cession, e.attach, sticky="w")

#set up the control variables
tkconfigure(e.limit, textvariable="tlimit")
tkconfigure(e.cession.textvariable="tcession")
tkconfigure(e.attach.textvariable="tattach")

print(tclvar$tlimit)
print(tclvar$tcession)
print(tclvar$tattach)








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



