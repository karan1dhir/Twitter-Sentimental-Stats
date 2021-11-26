
library(twitteR)
library(ROAuth)
library(httr)
library(base64enc)
library(dplyr)
library(plyr)
library(dismo)
library(maps)
library(ggmap)



# Setting up the credentials
TwitterClient <- function(searchString,no_of_tweets){
  
  api_key = as.character("mpJ47SSV27W875Coe1VRbI6hs")
  api_secret = as.character("1ukBRBBESV5WZqkAsA0vqeXeUJzKmOQVvmahgfglkeX5qCXluC")
  access_token = as.character("783402164169256960-cn8FYO2fhZZpxbf10dkdiLbJIcbvA7k")
  access_token_secret = as.character("KJzwcBGQoT9s0oOXsGRIxoMsxuYXzlS7cF5rleNHViLsa")
  
  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
  
  inital_date <- Sys.Date() - 60
  current_date <- Sys.Date(); 
  MTweets <- searchTwitter(searchString,n = no_of_tweets,since =as.character(inital_date),until=as.character(current_date))
  df <- do.call("rbind",lapply(MTweets,as.data.frame))
  return(list(t_str= MTweets,t_df = df))
}

## Cleaning the tweets 

clean.tweets = function(tweet)
{
  # convert to lower case
  tweet = tolower(tweet)
  # remove rt
  tweet = gsub("rt", "", tweet)
  # remove at
  tweet = gsub("@\\w+", "", tweet)
  # remove punctuation
  tweet = gsub("[[:punct:]]", "", tweet)
  # remove numbers
  tweet = gsub("[[:digit:]]", "", tweet)
  # remove links http
  tweet = gsub("http\\w+", "", tweet)
  # remove tabs
  tweet = gsub("[ |\t]{2,}", "", tweet)
  # remove blank spaces at the beginning
  tweet = gsub("^ ", "", tweet)
  # remove blank spaces at the end
  tweet = gsub(" $", "", tweet)
  # some other cleaning text
  tweet = gsub('https://','',tweet)
  tweet = gsub('http://','',tweet)
  tweet = gsub('[^[:graph:]]', ' ',tweet)
  tweet = gsub('[[:punct:]]', '', tweet)
  tweet = gsub('[[:cntrl:]]', '', tweet)
  tweet = gsub('\\d+', '', tweet)
  return(tweet)
}



## API call-out to get all the tweets belonging to a particular hash-tag.
searchString <- "Trump"
no_of_tweets <- 3500
tweet_obj <- TwitterClient(searchString,no_of_tweets)
tweets.txt <- sapply(tweet_obj$t_str, function(t)t$getText())


filtered_tweets <- clean.tweets(tweets.txt)
#View(twitter_results)



userInfo <- lookupUsers(twitter_results$screenName)
userFrame <- twListToDF(userInfo)
locatedUsers <- !is.na(userFrame$location)










