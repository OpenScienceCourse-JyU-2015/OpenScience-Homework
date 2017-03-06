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
