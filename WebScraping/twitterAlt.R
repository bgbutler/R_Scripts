

#this is a primary way to connect to Twitter
#it may not work
#load the required pacakges
library(twitteR)
library(ROAuth)
library(RCurl)
library(httr)

key = "AS9oq1tqMgBFhmRM2rPQBOUNT"
secret = "8mPBNtKMiwKp7hjPSnGMwbiruXjSgTD5xc7syq6lOa8Q8k2h8k"

setwd("K:\Sandbox\R\twitteR")

#set up certificate
download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="C:/Users/bbutler/Documents/cacert.pem",
              method="auto")

authenticate <- OAuthFactory$new(consumerKey = key,
                                 consumerSecret = secret,
                                 requestURL = 'https://api.twitter.com/oauth/request_token',
                                 accessURL = 'https://api.twitter.com/oauth/access_token',
                                 authURL = 'https://apo.twitter.com/oauth/authorize')

authenticate$handshake(cainfo = "C:/Users/bbutler/Documents/cacert.pem")

#insert pin and run

save(authenticate, file="twitterAuthentication.Rdata")

registerTwitterOAuth(authenticate)