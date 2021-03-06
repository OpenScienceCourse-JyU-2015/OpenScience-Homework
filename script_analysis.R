## Matthieu's comment
# This script is missing some setup code - it works if it is run in the same R
# session after the previous script, but it doesn't run if it is started on its
# own in a fresh R session.
# It is good practice to make each script working as a stand-alone unit.

# (I understand that here the main issue was about not being able to save the
# database to a file in the previous script.)

library(DBI)
mydb = dbConnect(RSQLite::SQLite(), "mydb.sqlite")


# Analysis script

park <- dbReadTable(mydb, "treesPark")
street <- dbReadTable(mydb, "treesStreet")
trees <- rbind(park, street) # this is the dataset for the full city

#function 1 ####
# Eric's comment: You have done a good job of including commments in your code, 
# but it would have been nice to have a little bit more explanation as to what 
# the function does.

#function for parks
parkfun <- function(x) {
  genus <- subset(park, park$genus==x)
  round(sqrt(((genus$TREELOCATIONX[2:length(genus[,1])]-genus$TREELOCATIONX[1:length(genus[,1])-1])^2) + 
         ((genus$TREELOCATIONY[2:length(genus[,1])]-genus$TREELOCATIONY[1:length(genus[,1])-1])^2)),2)
}

# teste
parkfun("Acer")

#function for streets
stfun <- function(x) {
  genus <- subset(street, street$genus==x)
  round(sqrt(((genus$TREELOCATIONX[2:length(genus[,1])]-genus$TREELOCATIONX[1:length(genus[,1])-1])^2) + 
             ((genus$TREELOCATIONY[2:length(genus[,1])]-genus$TREELOCATIONY[1:length(genus[,1])-1])^2)),2)
}

#teste
stfun("Acer")


## Matthieu's comment: the code calculates distances, but not between all the
## pairs of trees, only between the pairs generated by matching one tree with the
## next in the table (n-1 pairs).
## 
## The total number of pairs is the number of cells in an upper triangular distance
## matrix of size n x n, i.e. (n * (n - 1) / 2 ) pairs.

## One easy way is simply to use the dist function from R:

pairDist = function(data, x) {
    genus = subset(data, genus == x)
    distances = dist(genus[, c("TREELOCATIONX", "TREELOCATIONY")], method = "euclidean")
    return(as.vector(distances))
}

# Note that this function takes two arguments, data and x, so that one can
# specify which dataset to use with the function, instead of writing two
# separate functions for the street and park datasets.

# Test

dS = pairDist(street, "Acer")
dP = pairDist(park, "Acer")
all = rbind(street, park)
dA = pairDist(all, "Acer")

plot(density(dA, bw = 500), ylim = c(0, 2e-4))
lines(density(dS, bw = 500), col = "blue")
lines(density(dP, bw = 500), col = "red")

## End of Matthieu's comment


# function 2 ####
#function2 for the parks

parkfun2 <- function(x,y){
  #part 1 - calculate genus average distance
  genus <- subset(park, park$genus==x)
  dist1 <- round(sqrt(((genus$TREELOCATIONX[2:length(genus[,1])]-genus$TREELOCATIONX[1:length(genus[,1])-1])^2) + 
               ((genus$TREELOCATIONY[2:length(genus[,1])]-genus$TREELOCATIONY[1:length(genus[,1])-1])^2)),2)
  meandist <- mean(dist1)
  
  #part 2 - calculate the distances of all other genus with counts bigger than y
  parknames <- names(table(park$genus)>y)
  
  distvector <- matrix(NA, nrow = length(unique(parknames)),ncol = 2)
  for(i in seq(unique(parknames))){
    genusname2 <- unique(park$genus)[i]
    distvector[i,1] <- genusname2
    genus2 <- park[which(park$genus==genusname2),]
    dist2 <- round(sqrt(((genus2$TREELOCATIONX[2:length(genus2[,1])]-genus2$TREELOCATIONX[1:length(genus2[,1])-1])^2) + 
                          ((genus2$TREELOCATIONY[2:length(genus2[,1])]-genus2$TREELOCATIONY[1:length(genus2[,1])-1])^2)),2)
    distvector[i,2] <- as.numeric(round(mean(dist2),2))
    distvector2 <- distvector[-which(distvector[,2]=="NaN"),]
    
  }
  
  distvector2 <- as.data.frame(distvector2)
  colnames(distvector2) <- c("genus", "average dist")
  
  #part 3 - calculate the differences between the averages
  meandist
  distvector2$relativedist <- -1*round(as.numeric(distvector2[,2])-meandist,2)
  print(distvector2)
}

#teste
parkfun2("Acer",1000)

#function2 for the streets

stfun2 <- function(x,y){
  #part 1 - calculate genus average distance
  genus <- subset(street, street$genus==x)
  dist1 <- round(sqrt(((genus$TREELOCATIONX[2:length(genus[,1])]-genus$TREELOCATIONX[1:length(genus[,1])-1])^2) + 
                        ((genus$TREELOCATIONY[2:length(genus[,1])]-genus$TREELOCATIONY[1:length(genus[,1])-1])^2)),2)
  meandist <- mean(dist1)
  
  #part 2 - calculate the distances of all other genus with counts bigger than y
  stnames <- names(table(street$genus)>y)

  distvector <- matrix(NA, nrow = length(unique(stnames)),ncol = 2)
  for(i in seq(unique(stnames))){
    genusname2 <- unique(street$genus)[i]
    distvector[i,1] <- genusname2
    genus2 <- street[which(street$genus==genusname2),]
    dist2 <- round(sqrt(((genus2$TREELOCATIONX[2:length(genus2[,1])]-genus2$TREELOCATIONX[1:length(genus2[,1])-1])^2) + 
                          ((genus2$TREELOCATIONY[2:length(genus2[,1])]-genus2$TREELOCATIONY[1:length(genus2[,1])-1])^2)),2)
    distvector[i,2] <- as.numeric(round(mean(dist2),2))
    distvector2 <- distvector[-which(distvector[,2]=="NaN"),]
    
  }
  
  distvector2 <- as.data.frame(distvector2)
  colnames(distvector2) <- c("genus", "average dist")
  
  #part 3 - calculate the differences between the averages
  meandist
  distvector2$relativedist <- -1*round(as.numeric(distvector2[,2])-meandist,2)
  print(distvector2)
}

#test
stfun2("Acer",100)

## Matthieu's comments
# Here again I am afraid the instructions were not crystal clear :)
# The aim was to have a function that would be called like:
#   averageDistGenusMinCount(trees, minCount = 50)
# And the output would be something like:
#   Acer     14500.89
#   Albies   50489.14
#   ...      ...
# Where, for each genus, we have the average distance between trees of this exact genus.

# Also, the coding idea was to reuse the previous functions, so that you don't
# have to code everything in just one function.

# This is one way how it can be done:

pairDistGenus = function(data, x) {
    # Return the distances between all pairs of trees of genus 'x' in dataset
    # 'data'
    genus = subset(data, genus == x)
    distances = dist(genus[, c("TREELOCATIONX", "TREELOCATIONY")], method = "euclidean")
    return(as.vector(distances))
}

averageDistGenus = function(data, x) {
    # Return the average distance between all pairs of trees of genus 'x' in
    # dataset 'data'
    distances = pairDistGenus(data = data, x = x)
    return(mean(distances))
}

averageDistGenusMinCount = function(data, minCount) {
    # Return a table with average distances between all trees within each genus
    # for which there are at least 'minCount' trees in the dataset 'data'
    counts = table(data$genus)
    genus = names(counts)[counts >= minCount]
    avgDist = rep(NA, length(genus))
    for (g in seq_along(genus)) {
        avgDist[g] = averageDistGenus(data = data, x = genus[g])
    }
    # Eric's comment: 'counts', which is a named vector needs to be converted to a 
    # plain vector, otherwise the name and value columns are added to the dataframe.
    return(data.frame(genus = genus, count = as.vector(counts[counts >= minCount]),
                      avgDist = avgDist))
}

# Test

dists = averageDistGenusMinCount(trees, 50)
dim(dists)
dists

plot(dists$count, dists$avgDist)
# Some species seem more aggregated than other
dists$genus[dists$avgDist < 3500]

## End of Matthieu's comment

####################################################################################
#install.packages(c('RgoogleMaps', 'PBSmapping',"GISTools"))
library(GISTools)
library(ggplot2)
library(RgoogleMaps)
library(PBSmapping)
library(sp)

#graph 1
# distribution of the points in parks and strees

pointsparks <- SpatialPoints(cbind(park$LONGITUDE,park$LATITUDE))
pointsstreets <- SpatialPoints(cbind(street$LONGITUDE,street$LATITUDE))

bb <- qbbox(lon = park$LONGITUDE, lat = park$LATITUDE)

mymap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "map.png", GRAYSCALE =FALSE)

par(mfrow=c(1,2))
PlotOnStaticMap(mymap,pointsparks@coords[,2],pointsparks@coords[,1], pch=21, bg="red", cex=0.8)
PlotOnStaticMap(mymap,pointsstreets@coords[,2],pointsstreets@coords[,1], pch=21, bg="blue", cex=0.5)
dev.off()

#graph 2
# differences in growth between streets and parks
treeheight <- ggplot(trees, aes(x=trees$TREEHEIGHTinMETRES,y=..count.., fill=TYPEOFTREE) )+
  geom_histogram(binwidth=5)+
  ylab("Count")+
  xlab("Height")+
  theme_bw()
treeheight

#graph 3
# trees conditions according to height and locality
condition <- ggplot(trees, aes(x=CONDITION, y=TREEHEIGHTinMETRES, fill=TYPEOFTREE))+
  geom_boxplot()
condition

## CONDITION is actually an ordered factor, let's recode it appropriately:
trees$CONDITION = factor(trees$CONDITION, levels = c("Dead", "Dying",
  "Very Poor", "Poor", "Fair", "Good"), ordered = T)

## Maybe this view is also interesting
# Eric's comment: Yes, I think it is easier to read this version
condition2 <- ggplot(trees, aes(x=TYPEOFTREE, y=TREEHEIGHTinMETRES, fill=CONDITION))+
  geom_boxplot()
condition2


#graph 4
# tree vigour according to height and locality
vigour <- ggplot(trees, aes(x=VIGOUR, y=TREEHEIGHTinMETRES, fill=TYPEOFTREE))+
  geom_boxplot()
vigour

## Maybe this view is also interesting
vigour2 <- ggplot(trees, aes(x=TYPEOFTREE, y=TREEHEIGHTinMETRES, fill=VIGOUR))+
  geom_boxplot()
vigour2


#graph 5
# Acer in the parks and the distance to the other Acers
a <- parkfun("Acer")
hist(a,xlab = "distance")

# disconect the database
dbDisconnect(mydb)

