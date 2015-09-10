

#set up the libraries to use




#import the global member report
url <- "K:/Sandbox/Global Report Dashboard/GlobalMemberReport_(2015-09-01T14-45-46).csv"
gmrRaw <- read.csv(url, header = TRUE, sep=",", as.is = FALSE)

#parse the contributions and logins by day of week
gmrRaw$Contributions.by.day.of.week..Sun...Sat..UTC. <- as.character(gmrRaw$Contributions.by.day.of.week..Sun...Sat..UTC.)
gmrRaw$conDay <- strsplit(gmrRaw$Contributions.by.day.of.week..Sun...Sat..UTC.,"[|]")

gmrRaw$Logins.by.day.of.week..Sun...Sat..UTC. <- as.character(gmrRaw$Logins.by.day.of.week..Sun...Sat..UTC.)
gmrRaw$loginDay <- strsplit(gmrRaw$Logins.by.day.of.week..Sun...Sat..UTC.,"[|]")

#create the column counts and then clean up the fields
#this takes about 8 hrs to run
count <- length(gmrRaw$conDay)

for (i in 1:count){
    gmrRaw$conSun[i] <- gmrRaw$conDay[[i]][1]
    gmrRaw$conMon[i] <- gmrRaw$conDay[[i]][2]
    gmrRaw$conTue[i] <- gmrRaw$conDay[[i]][3]
    gmrRaw$conWed[i] <- gmrRaw$conDay[[i]][4]
    gmrRaw$conThu[i] <- gmrRaw$conDay[[i]][5]
    gmrRaw$conFri[i] <- gmrRaw$conDay[[i]][6]
    gmrRaw$conSat[i] <- gmrRaw$conDay[[i]][7]
    gmrRaw$loginSun[i] <- gmrRaw$loginDay[[i]][1]
    gmrRaw$loginMon[i] <- gmrRaw$loginDay[[i]][2]
    gmrRaw$loginTue[i] <- gmrRaw$loginDay[[i]][3]
    gmrRaw$loginWed[i] <- gmrRaw$loginDay[[i]][4]
    gmrRaw$loginThu[i] <- gmrRaw$loginDay[[i]][5]
    gmrRaw$loginFri[i] <- gmrRaw$loginDay[[i]][6]
    gmrRaw$loginSat[i] <- gmrRaw$loginDay[[i]][7]
}

#convert the characters to factors remove old files
gmrRaw$conSun <- as.numeric(gmrRaw$conSun)
gmrRaw$conMon <- as.numeric(gmrRaw$conMon)
gmrRaw$conTue <- as.numeric(gmrRaw$conTue)
gmrRaw$conWed <- as.numeric(gmrRaw$conWed)
gmrRaw$conThu <- as.numeric(gmrRaw$conThu)
gmrRaw$conFri <- as.numeric(gmrRaw$conFri)
gmrRaw$conSat <- as.numeric(gmrRaw$conSat)

gmrRaw$loginSun <- as.numeric(gmrRaw$loginSun)
gmrRaw$loginMon <- as.numeric(gmrRaw$loginMon)
gmrRaw$loginTue <- as.numeric(gmrRaw$loginTue)
gmrRaw$loginWed <- as.numeric(gmrRaw$loginWed)
gmrRaw$loginThu <- as.numeric(gmrRaw$loginThu)
gmrRaw$loginFri <- as.numeric(gmrRaw$loginFri)
gmrRaw$loginSat <- as.numeric(gmrRaw$loginSat)

#remove the hour logins and contributions
gmrRaw$Contributions.by.hour..community.TZ. <- NULL
gmrRaw$Logins.by.hour..community.TZ. <- NULL

gmrRaw$conDay <- NULL
gmrRaw$loginDay <- NULL


write.csv(gmrRaw, file = "K:/Sandbox/Global Report Dashboard/gmrRaw_09012015.csv", row.names = F)
write.csv(gmrRaw, file = "K:/Sandbox/Global Report Dashboard/gmrClean_09012015.csv", row.names = F)

