## Gabriel's version

# RSQLite
dir()

#install.packages(c("RSQLite","DBI"))
library(RSQLite)
library(DBI)

#creating a database

mydb <- dbConnect(RSQLite::SQLite(), "mydb.sqlite")

#loading the data to the database

trees <- read.csv("odTrees_afterGMM.csv")
dbWriteTable(mydb, "trees", trees)
  dbListTables(mydb)

# using queries to split the database into 2
# the first colunm of the tree database "TYPEOFTREE" will be split in two to compose two datatables
  
   treesStreet <- dbGetQuery(mydb, 'SELECT * FROM trees WHERE "TYPEOFTREE" = "StreetTree"')
   treesPark   <- dbGetQuery(mydb, 'SELECT * FROM trees WHERE "TYPEOFTREE" = "ParkTree"')

#write on database
   dbWriteTable(mydb, "treesStreet", treesStreet)
   dbWriteTable(mydb, "treesPark", treesPark)

# Close database connection
dbDisconnect(mydb)



## Matthieu's version

db = dbConnect(RSQLite::SQLite(), "trees-db.sqlite")
# This created a database file on the hard drive

# The way you stored your data is:
# - you stored all the (curated) tree data frame into one table
# - then you splitted the dataset into two tables based on the TYPEOFTREE
#   variable to indulge our request about storing the data in two tables - but
#   this was not really what we meant about storing a tidy dataset in two tables!

# Our aim was to store the data is a way that avoid redundancy of
# information. You can read the Wikipedia page about First normal form here to
# see what I mean: https://en.wikipedia.org/wiki/First_normal_form

# In the curated tree dataset you produced with script_cleaning_data.R, the
# columns "herbariumtypes", "mixedinfo", "indet" and "genus" all derives from
# "SPECIES", so it would make sense to split the data frame into two tables:
# - one table with the information about each individual tree
# - one table with the information which is relevant to each SPECIES entry

# This is one way to do it:
tableIndividuals = trees[, c("TYPEOFTREE", "SPECIESTYPE", "SPECIES", "AGE", "DESCRIPTION",
                             "TREESURROUND", "VIGOUR", "CONDITION", "DIAMETERinCENTIMETRES",
                             "SPREADRADIUSinMETRES", "TREELOCATIONX", "TREELOCATIONY",
                             "LONGITUDE", "LATITUDE", "TREETAG", "TREEHEIGHTinMETRES")]
tableSpecies = unique(trees[, c("SPECIES", "herbariumtypes", "mixedinfo", "indet", "genus")])

# Let's see the dimensions of those tables:
dim(tableIndividuals)
dim(tableSpecies)
# tableSpecies is much more compact because duplicated rows were removed!

# If we need to rebuild the full table for some analysis, we can use the "merge"
# function:
fullTable = merge(tableIndividuals, tableSpecies, by = "SPECIES")
dim(fullTable)
# Here we realise that the reconstructed fullTable is bigger than the original
# trees data frame. Let's try to find out why:
which(table(tableSpecies$SPECIES)>1)
# We realise the issues is that the SPECIES entry "Mixed spp" has multiple rows
# in tableSpecies, while we were expected only one row for each species:
tableSpecies[tableSpecies$SPECIES=="Mixed spp", ]
# This is because we lost some information from the SPECIES column in the
# script "script_cleaning_data.R" (and transferred it to the mixedinfo
# column). We shouldn't have proceed this way; instead we could have kept the
# SPECIES column unchanged, and set the variable mixedinfo to TRUE/FALSE to
# indicate whether or not the SPECIES entry was a mix.

# Let's correct that:
trees$SPECIES = as.character(trees$SPECIES) # To avoid issue with factor levels
trees$mixedinfo = as.character(trees$mixedinfo)
trees$SPECIES[which(trees$SPECIES == "Mixed spp")] = trees$mixedinfo[which(trees$SPECIES == "Mixed spp")]
trees$isMixedSpecies = !is.na(trees$mixedinfo)

# Now we can split the full dataset into our tidy tables:
tableIndividuals = trees[, c("TYPEOFTREE", "SPECIESTYPE", "SPECIES", "AGE", "DESCRIPTION",
                             "TREESURROUND", "VIGOUR", "CONDITION", "DIAMETERinCENTIMETRES",
                             "SPREADRADIUSinMETRES", "TREELOCATIONX", "TREELOCATIONY",
                             "LONGITUDE", "LATITUDE", "TREETAG", "TREEHEIGHTinMETRES")]
tableSpecies = unique(trees[, c("SPECIES", "herbariumtypes", "isMixedSpecies", "indet", "genus")])
# Let's remove the NA entry for SPECIES:
tableSpecies = tableSpecies[!is.na(tableSpecies$SPECIES), ]

# Let's see the dimensions of those tables:
dim(tableIndividuals)
dim(tableSpecies)

# Let's test merging again the tidy tables:
fullTable = merge(tableIndividuals, tableSpecies, by = "SPECIES")
dim(fullTable)
# These dimensions are the original dimensions, minus 339 rows which had no
# SPECIES information:
sum(is.na(trees$SPECIES))

# The tables:
# - tableIndividuals
# - tableSpecies
# are the ones we want to store in the SQLite database.
dbWriteTable(db, "individuals", tableIndividuals)
dbWriteTable(db, "species", tableSpecies)

dbDisconnect(db)

# We now have the data stored in a tidy format in a database.

# Separating columns to avoid redundancy makes many things easier: for example,
# if we want to add some information about the species such as flowering season
# or any other traits, we only have to update one place, the species table, and
# the information will be propagated to all rows as needed when we perform a
# merge between the individuals and the species tables.

# Eric's comment: Although the location data is not needlessly repeated, one 
# option might be to make a location table, which can be joined to the tree 
# data by 'TREETAG'. This way, if you wanted to add some more location data, or 
# derive location- based data (e.g. distance to nearest neighbouring tree) you 
# wouldn't need to also load the tree data.

