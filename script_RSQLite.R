# RSQLite
dir()

install.packages(c("RSQLite","DBI"))
library(RSQLite)
library(DBI)

#creating a database

mydb <- dbConnect(RSQLite::SQLite(), dbnames="mydb.sqlite")

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
   
# disconect the database
dbDisconnect(mydb)



