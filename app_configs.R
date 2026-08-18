## read in data (replace with your file path)
pedigree_download<-read.csv("data/2025ParentPedigree.csv") #needs to be updated each year

historical_crosses<-read.csv("data/HistoricalCrossEntries.csv") #needs to be updated each year

blup_data<-read.csv("data/StageWiseParentBLUPS.csv")
colnames(blup_data)[1]<-"Clone"

full_amat<-PedMatrix(read.csv("data/2025ParentPedigree_Full.csv"))
parent_amat<-full_amat[rownames(full_amat)%in%pedigree_download$Accession, colnames(full_amat)%in%pedigree_download$Accession]



## INIT DB CONNECTION ----------------------

location_iid_map2 <- list(
  "WICSCBS_25" = "3917"
)

# location_iid_map <- list(
#   "WICSCBS" = "3922"
# )

#Blocking vector
block_vector<-c("1"="main")

crosses_iid_map<-list(
  "FL_25"="3940"  #needs to be updated each year
)



brap <- brapi::as.ba_db(
  secure = FALSE,
  protocol = "https://",
  db =Sys.getenv("URL"),
  port = 80,
  apipath = NULL,
  multicrop = FALSE,
  crop = "",
  user = Sys.getenv("USERNAME"),
  password = Sys.getenv("PASS"),
  token = "",
  granttype = "password",
  clientid = "rbrapi",
  bms = FALSE,
  version = "v1"
)

brap2 <- brapi::as.ba_db(
  secure = FALSE,
  protocol = "https://",
  db =  Sys.getenv("URL"),
  port = 80,
  apipath = NULL,
  multicrop = FALSE,
  crop = "",
  user = Sys.getenv("USERNAME"),
  password = Sys.getenv("PASS"),
  token = "",
  granttype = "password",
  clientid = "rbrapi",
  bms = FALSE,
  version = "v2"
)
