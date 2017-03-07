
# installing the relevant packages
#install.packages(c("tidyr","plyr"))
library(tidyr)
library(plyr)


#import data set
trees <- read.csv("odTrees.csv",stringsAsFactors = F)

summary(trees)

#FIRTS STEP: cleaning the data ####
#check all the columns and make the corrections

colnames(trees)

#replace all N/A per NA
trees[trees=="N/A"] <- NA

sort(unique(trees$SPECIES))
# replace \ for empty spaces
trees$SPECIES <- gsub("\"","",trees$SPECIES)
# replace "Abies(type)" by "Abies (type)"
## Gabriel's version:
#trees$SPECIES[grep("Abies",trees$SPECIES)][3] <- "Abies (type)"
## Matthieu's comment: More robust (does not rely on using the index [3])
trees$SPECIES[grepl("Abies(type)",trees$SPECIES, fixed = T)] <- "Abies (type)"
  
unique(trees$SPECIESTYPE)  

unique(trees$AGE)   
# replace empty lines for NA and standardize nomenclature
trees$AGE[which(trees$AGE=="")] <- NA
trees$AGE[which(trees$AGE=="Semi-Mature")] <- "Semi Mature"
  
unique(trees$DESCRIPTION)    
# standardize nomenclature
trees$DESCRIPTION[which(trees$DESCRIPTION=="Semi-Mature")] <- "Semi Mature"
trees$DESCRIPTION[which(trees$DESCRIPTION=="Juvenile/Young")] <- "Juvenile Young"

unique(trees$TREESURROUND)    
# replace empty lines for NA and normalize nomenclature
trees$TREESURROUND[which(trees$TREESURROUND=="Bramble/Briars")] <- "Bramble Briars"

unique(trees$DIAMETERinCENTIMETRES)
## Gabriel's version
## multiply negative values per -1, since cm cannot be negative
#trees$DIAMETERinCENTIMETRES[which(trees$DIAMETERinCENTIMETRES<0)] <- -1*trees$DIAMETERinCENTIMETRES[which(trees$DIAMETERinCENTIMETRES<0)]
## Matthieu's comment:
## The thing is that we do not really know the reason why some numbers are negative.
## Can we really assume it is just a sign error? There are many of them.
## The most conservative approach could be just to set those values to NA
trees$DIAMETERinCENTIMETRES[trees$DIAMETERinCENTIMETRES<=0)] = NA

unique(trees$SPREADRADIUSinMETRES)
## Gabriel's version:
## multiply negative values per -1, since negatives are not allowed
#trees$SPREADRADIUSinMETRES[which(trees$SPREADRADIUSinMETRES<0)] <- -1*trees$SPREADRADIUSinMETRES[which(trees$SPREADRADIUSinMETRES<0)]
## Matthieu's comment:
## Same thing as for the negative diameters: let's just set those impossible values to NA
trees$SPREADRADIUSinMETRES[trees$SPREADRADIUSinMETRES<=0] = NA

unique(trees$TREETAG)
# replace empty lines for NA 
trees$TREETAG[which(trees$TREETAG=="")] <- NA

unique(trees$TREEHEIGHTinMETRES)    
## Gabriel's version:
## multiply negative values per -1, since negatives are not allowed
#trees$TREEHEIGHTinMETRES[which(trees$TREEHEIGHTinMETRES<0)] <- -1*trees$TREEHEIGHTinMETRES[which(trees$TREEHEIGHTinMETRES<0)]
## Matthieu's comment:
## Again, let's just set them to NA
trees$TREEHEIGHTinMETRES[trees$TREEHEIGHTinMETRES<=0] = NA
# exclude bizarre value
## Gabriel's version:
#trees$TREEHEIGHTinMETRES[which(trees$TREEHEIGHTinMETRES==832)] <- NA
## Matthieu's comment:
# More robust to set a threshold instead of a precise value:
trees$TREEHEIGHTinMETRES[which(trees$TREEHEIGHTinMETRES>=200)] <- NA

# end of the data cleaning  ####

#the data has now been cleaned, so lets make it tidy
# SECOND: make the data tidy #####

sort(unique(trees$SPECIES))
  length(trees$SPECIES)
  
  #create a colunm for those observation that are species types and assign type objects
  trees$herbariumtypes <- rep(NA, length(trees$SPECIES))
  trees$herbariumtypes[grep("type",trees$SPECIES)] <- "type"
  
  
  #create a colunm for the mixed information present in the "species" colunm.
  #all species with this attribute will be renamed to "mixed"
  trees$mixedinfo <- rep(NA, length(trees$SPECIES))
  trees$mixedinfo[grep("Mixed",trees$SPECIES)] <- trees$SPECIES[grep("Mixed",trees$SPECIES)]
  trees$SPECIES[grep("Mixed",trees$SPECIES)] <- "Mixed spp"
  
  # the information "not known" will also get a new colunm called "indet"
  # all the species indentifyed as "not known" will become "indet spp"
  trees$indet <- rep(NA, length(trees$SPECIES))
  trees$indet[grep("Not known",trees$SPECIES)] <- trees$SPECIES[grep("Not known",trees$SPECIES)]
  trees$SPECIES[grep("Not known",trees$SPECIES)] <- "Indet spp"
   
  #create a column for genusand get the genus name only
  trees$genus <-  sapply(strsplit(trees$SPECIES," "),`[`,1)

# end of data tidying ####

# write the new file in the disk ####
  write.csv(trees, "odTrees_afterGMM.csv",row.names = FALSE )


  
  
