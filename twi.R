library(twitteR)
library(dismo)
library(XML)
library(maps)
library(geosphere)

#get basic info about user

tmp <- getUser("fire_fox")
#followers=tmp$getFollowers(n=nMax)

#location, cat longtitude and latitude from location
userlocation <- geocode(tmp$location)
userLL <- userlocation[ , c("lon", "lat")]

#followers and following, their locations
followers = tmp$getFollowers(n = NULL)
followersLocation = sapply(followers,function(x){location(x)})
followersLocation <- as.data.frame(do.call(rbind, followersLocation))

following = tmp$getFriends(n = NULL)
followingLocation = sapply(following,function(x){location(x)})
followingLocation <- as.data.frame(do.call(rbind, followingLocation))

#convert location to geocodes

gcodesfollowers <- geocode(followersLocation$V1)
gcodesfollowers <- gcodesfollowers[!is.na(gcodesfollowers$lon), ]
unigcodesfollowers <- unique.data.frame(gcodesfollowers)
unigcodesfollowers <- unigcodesfollowers[, c("lon", "lat")]

gcodesfollowing <- geocode(followingLocation$V1)
gcodesfollowing <- gcodesfollowing[!is.na(gcodesfollowing$lon), ]
unigcodesfollowing <- unique.data.frame(gcodesfollowing)
unigcodesfollowing <- unigcodesfollowing[, c("lon", "lat")]


nFollowers <- length(unigcodesfollowers$lat)
nFollowing <- length(unigcodesfollowing$lat)

#created graphs for followers and following
par(mfrow=c(2,1),mar=rep(0,4))

map("world", col="#191919", fill=TRUE, bg="#000000", mar=rep(0,4))
mtext("@fire_fox Follower Map",col="white")

for (i in 1:nFollowers){
  inter <- gcIntermediate(userLL, c(unigcodesfollowers$lon[i], unigcodesfollowers$lat[i]), n=100, addStartEnd=TRUE)
  lines(inter, col = "white", lwd = 0.8)
}     

map("world", col="#191919", fill=TRUE, bg="#000000", mar=rep(0,4))
mtext("@fire_fox Following Map",col="white")

for (i in 1:nFollowing){
  inter <- gcIntermediate(userLL, c(unigcodesfollowing$lon[i], unigcodesfollowing$lat[i]), n=100, addStartEnd=TRUE)
  lines(inter, col = "white", lwd = 0.8)
}

mtext("created by @fire_fox",col="white", side = 1)















