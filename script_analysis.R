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

