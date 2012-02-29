library(twitteR)
library(dismo)
library(XML)
library(maps)
library(geosphere)

#function, which draw followers on map

draw_followers <- function(userName, colorFollowers){
 
  #get name and location, transform location into geocodes
  tmp1 <- getUser(userName)  
  tmpuserlocation <- geocode(tmp1$location)
  tnFollowers <- 1
  
  if (is.na(tmpuserlocation$lon)){stop("We can't find the latitude and longitude of your location from Twitter")}
    
    
    tmpuserLL <- tmpuserlocation[ , c("lon", "lat")]   
    tfollowers <- tmp1$getFollowers(300)
    tfollowersLocation <- sapply(tfollowers,function(x){location(x)})
    tfollowersLocation <- as.data.frame(do.call(rbind, tfollowersLocation))
  
    tgcodesfollowers <- geocode(tfollowersLocation$V1)
    tgcodesfollowers <- tgcodesfollowers[!is.na(tgcodesfollowers$lon), ]
    tunigcodesfollowers <- unique(data.frame(tgcodesfollowers))
    tunigcodesfollowers <- tunigcodesfollowers[, c("lon", "lat")]
    # draw links beetween user and his followers
    tnFollowers <- length(tunigcodesfollowers$lat)
    
        for (i in 1:tnFollowers){
        inter <- gcIntermediate(tmpuserLL, c(tunigcodesfollowers$lon[i], tunigcodesfollowers$lat[i]), n=100, addStartEnd=TRUE)
        lines(inter, col = colorFollowers, lwd = 0.8)
        }   
}


tmp <- getUser("fire_fox")

#get names of followers
followers = tmp$getFollowers(n = NULL)
followersname <- sapply(followers, function(x){screenName(x)})
followersname <- as.character(followersname)
k <- length(followersname)

#draw map of followers' followers
#map("world", col="#191919", fill=TRUE, bg="#000000", mar=rep(0,4))
map("world", col="#191919", fill=TRUE, bg="white", mar=rep(0,4))


for (i in 2:10){
  draw_followers(followersname[i], "lightblue")
}

#draw map of followers
draw_followers("fire_fox", "red")




