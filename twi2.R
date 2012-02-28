library(twitteR)
library(dismo)
library(XML)
library(maps)
library(geosphere)

#get basic info about user

tmp <- getUser("fire_fox")
userlocation <- geocode(tmp$location)
userLL <- userlocation[ , c("lon", "lat")]




followers = tmp$getFollowers(n = NULL)
tmp$screenName

followersname <- sapply(followers, function(x){screenName(x)})
followersname <- as.character(followersname)
k <- length(followersname)


followersLocation = sapply(followers,function(x){location(x)})
followersLocation <- as.data.frame(do.call(rbind, followersLocation))

gcodesfollowers <- geocode(followersLocation$V1)
gcodesfollowers <- gcodesfollowers[!is.na(gcodesfollowers$lon), ]
unigcodesfollowers <- unique.data.frame(gcodesfollowers)
unigcodesfollowers <- unigcodesfollowers[, c("lon", "lat")]

nFollowers <- length(unigcodesfollowers$lat)
map("world", col="#191919", fill=TRUE, bg="#000000", mar=rep(0,4))

for (i in 1:nFollowers){
  inter <- gcIntermediate(userLL, c(unigcodesfollowers$lon[i], unigcodesfollowers$lat[i]), n=100, addStartEnd=TRUE)
  lines(inter, col = "red", lwd = 0.8)
}

for (i in 2:10){
  draw_followers(followersname[i])
}

draw_followers(followersname[3])
#get followers
#get location
#clearing
#get followers for every follower
#get location
#clearing
#get codes
#draw
#function(user_name or user_id)
#get followers
#get location
#remove na
#get geocodes
#draw followers
draw_followers("drapinska")
draw_followers("exandr77")
draw_followers("frutik")

draw_followers <- function(userName){
  
  tmp1 <- getUser(userName)  
  tmpuserlocation <- geocode(tmp1$location)
  tnFollowers <- 1
  
  if (is.na(tmpuserlocation$lon)){stop("We can't find the latitude and longitude of your location from Twitter")}
    
    
    tmpuserLL <- tmpuserlocation[ , c("lon", "lat")]   
    tfollowers = tmp1$getFollowers(300)
    tfollowersLocation = sapply(tfollowers,function(x){location(x)})
    tfollowersLocation <- as.data.frame(do.call(rbind, tfollowersLocation))
  
    tgcodesfollowers <- geocode(tfollowersLocation$V1)
    tgcodesfollowers <- tgcodesfollowers[!is.na(tgcodesfollowers$lon), ]
    tunigcodesfollowers <- unique(data.frame(tgcodesfollowers))
    tunigcodesfollowers <- tunigcodesfollowers[, c("lon", "lat")]
    
    tnFollowers <- length(tunigcodesfollowers$lat)
    
        for (i in 1:tnFollowers){
        inter <- gcIntermediate(tmpuserLL, c(tunigcodesfollowers$lon[i], tunigcodesfollowers$lat[i]), n=100, addStartEnd=TRUE)
        lines(inter, col = i, lwd = 0.8)
        }   
}
