
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
  
  api_key = "mpJ47SSV27W875Coe1VRbI6hs"
  api_secret = "1ukBRBBESV5WZqkAsA0vqeXeUJzKmOQVvmahgfglkeX5qCXluC"
  access_token = "783402164169256960-cn8FYO2fhZZpxbf10dkdiLbJIcbvA7k"
  access_token_secret = "KJzwcBGQoT9s0oOXsGRIxoMsxuYXzlS7cF5rleNHViLsa"
  
  setup_twitter_oauth('mpJ47SSV27W875Coe1VRbI6hs',
                      '1ukBRBBESV5WZqkAsA0vqeXeUJzKmOQVvmahgfglkeX5qCXluC',
                      '783402164169256960-cn8FYO2fhZZpxbf10dkdiLbJIcbvA7k',
                      'KJzwcBGQoT9s0oOXsGRIxoMsxuYXzlS7cF5rleNHViLsa')
  
  MTweets <- searchTwitter(searchString, n = no_of_tweets,since ='2017-09-01',until='2021-11-06')
  df <- do.call("rbind",lapply(MTweets,as.data.frame))
  
  return (df)
}

searchString <- "#Catalonia"
no_of_tweets <- 3500

## API call-out to get all the tweets belonging to a particular hashtag.
twitter_results <- TwitterClient(searchString,no_of_tweets)
#View(twitter_results)


register_google(key = "AIzaSyDQZYNlMzjIUpZBRYfP4d8MQgXqcxL7Ovc",write=TRUE)

userInfo <- lookupUsers(twitter_results$screenName)
userFrame <- twListToDF(userInfo)
locatedUsers <- !is.na(userFrame$location)
locations <- geocode(userFrame$location[locatedUsers])
with(locations,plot(longitude,latitude))

head(locatedUsers)



