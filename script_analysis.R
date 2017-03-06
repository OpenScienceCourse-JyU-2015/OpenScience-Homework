# Analysis script

park <- dbReadTable(mydb, "treesPark")
street <- dbReadTable(mydb, "treesStreet")

#function 1 ####

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
parkfun("Acer",1000)

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

####################################################################################
install.packages(c('RgoogleMaps', 'PBSmapping',"GISTools"))
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

#graph 4
# tree vigour according to height and locality
vigour <- ggplot(trees, aes(x=VIGOUR, y=TREEHEIGHTinMETRES, fill=TYPEOFTREE))+
  geom_boxplot()
vigour

#graph 5
# Acer in the parks and the distance to the other Acers
a <- parkfun("Acer")
hist(a,xlab = "distance")

# disconect the database
dbDisconnect(mydb)

