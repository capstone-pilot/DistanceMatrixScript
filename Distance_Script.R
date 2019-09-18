#Package for geospatial distance between two points
if(!require(geosphere)){
  install.packages("geosphere")
  require(geosphere)
}
#Set working directory where files exist
setwd("~/Desktop/Pilot/Data")
#Main locations file
PFJ <- read.csv("Master_PFJ_US_Locations.csv")
#Incomplete list of competitors
competitor <- read.csv("Matt_Price_Competitor_List.csv")

#Subset data frames to useful information
DF1_Dist <- unique(PFJ[,c(1,7,8,10,11)])
numDF1 <- nrow(DF1_Dist)
DF1_Dist$STATION_NAME<- paste0(rep("PFJ_", numDF1), DF1_Dist$LOCATION_ID)

##Begin Data Cleaning

#Competitor Dataset
# competitor$Station.Name<-str_trim(as.character(competitor$Station.Name))
# competitor$Station.Name<-as.factor(competitor$Station.Name)
competitor$City <- str_trim(competitor$City)
competitor$Longitude <- round(competitor$Longitude, 4)
competitor$Latitude <- round(competitor$Latitude, 4)

#Standardize PFJ names
temp <- c()
comp_Idx <- which(substring(competitor$Station.Name,1,5)!="PILOT")
numDF2 <- length(comp_Idx)

#Subset Competitors dataset
DF2_Dist <- competitor[comp_Idx,c(1,4:7)]
DF2_Dist <- cbind(DF2_Dist,"STATION_NAME"=paste0(rep("COMP_",numDF2), DF2_Dist$OPIS.ID))

#Match column names for join
names(DF2_Dist) <- names(DF1_Dist)

#Initialize output matrix
compMatrix <- matrix( rep(NA,numDF1*numDF2), nrow = numDF1, ncol = numDF2 )
PFJMatrix <- matrix( rep(NA,numDF1*numDF1), nrow = numDF1, ncol = numDF1 )

#Rename Rows and Columns accordingly
rownames(compMatrix) <- DF1_Dist$STATION_NAME
colnames(compMatrix) <- DF2_Dist$STATION_NAME
rownames(PFJMatrix) <- DF1_Dist$STATION_NAME
colnames(PFJMatrix) <- DF1_Dist$STATION_NAME

#Calculate distances between stores
start=Sys.time()
for(i in 1:numDF1 ){
  for(j in 1:numDF2){
    if(i==j){
      compMatrix[i,j] <- NA
    }else{
      compMatrix[i,j] <- distHaversine( c(DF1_Dist$ADDRESS_LONGITUDE[i],DF1_Dist$ADDRESS_LATITUDE[i]), c(DF2_Dist$ADDRESS_LONGITUDE[j], DF2_Dist$ADDRESS_LATITUDE[j]) )
    }
  }
  print( paste0( ( ( i*j)/(numDF1*numDF2) )*100, "%" ) )
}
Sys.time()-start

start=Sys.time()
for(i in 1:numDF1 ){
  for(j in 1:numDF1){
    if(i==j){
      PFJMatrix[i,j] <- NA
    }else{
      PFJMatrix[i,j] <- distHaversine( c(DF1_Dist$ADDRESS_LONGITUDE[i],DF1_Dist$ADDRESS_LATITUDE[i]), c(DF1_Dist$ADDRESS_LONGITUDE[j], DF1_Dist$ADDRESS_LATITUDE[j]) )
    }
  }
  print( paste0( ( ( i*j)/(numDF1*numDF1) )*100, "%" ) )
}
Sys.time()-start

#Converts distance from meters to miles (3.28084ft/m and 5280ft/mi)
compMatrix <- (compMatrix*3.28084)/5280
PFJMatrix <- (PFJMatrix*3.28084)/5280

fileName1 <- paste0(getwd(),"/PFJ_Matrix.csv")
fileName2 <- paste0(getwd(),"/Competitor_Matrix.csv")
write.csv(PFJMatrix, file = fileName1)
write.csv(compMatrix, file = fileName2)

###---------------------------------------READ IN MATRICES---------------------------------------------###
PFJ_Matrix_Source <- read.csv(paste0(getwd(),"/PFJ_Matrix.csv"))
Comp_Matrix_Source <-  read.csv(paste0(getwd(),"/Competitor_Matrix.csv"))

##-------------------------PFJ Distance----------------------------##

start=Sys.time()
PFJ_OneMile <- numeric()
PFJ_FiveMile <- numeric()
PFJ_FifteenMile <- numeric()
PFJ_MinDist <- numeric()
for(i in 1:nrow(PFJ_Matrix_Source)){
  PFJ_OneMile[i] <- sum(ifelse( !is.na(PFJ_Matrix_Source[i,-1]) & (PFJ_Matrix_Source[i,-1]>0) & (PFJ_Matrix_Source[i,-1]<1) ,1,0))
  PFJ_FiveMile[i] <- sum(ifelse( !is.na(PFJ_Matrix_Source[i,-1]) & (PFJ_Matrix_Source[i,-1]>=1) & (PFJ_Matrix_Source[i,-1]<5),1,0 ))
  PFJ_FifteenMile[i] <- sum(ifelse( !is.na(PFJ_Matrix_Source[i,-1]) & (PFJ_Matrix_Source[i,-1]>=5) & (PFJ_Matrix_Source[i,-1]<15),1,0 ))
  PFJ_MinDist[i] <- min(PFJ_Matrix_Source[i,-1], na.rm = TRUE)
}
Sys.time()-start

##------------------------Competitor Distance------------------------##

start=Sys.time()
Comp_OneMile <- numeric()
Comp_FiveMile <- numeric()
Comp_FifteenMile <- numeric()
Comp_MinDist <- numeric()
for(i in 1:nrow(Comp_Matrix_Source)){
  Comp_OneMile[i] <- sum(ifelse( !is.na(Comp_Matrix_Source[i,-1]) & (Comp_Matrix_Source[i,-1]>0) & (Comp_Matrix_Source[i,-1]<1) ,1,0))
  Comp_FiveMile[i] <- sum(ifelse( !is.na(Comp_Matrix_Source[i,-1]) & (Comp_Matrix_Source[i,-1]>=1) & (Comp_Matrix_Source[i,-1]<5),1,0 ))
  Comp_FifteenMile[i] <- sum(ifelse( !is.na(Comp_Matrix_Source[i,-1]) & (Comp_Matrix_Source[i,-1]>=5) & (Comp_Matrix_Source[i,-1]<15),1,0 ))
  Comp_MinDist[i] <- min(Comp_Matrix_Source[i,-1], na.rm = TRUE)
}
Sys.time()-start









#-------------------------------------------BitRot-----------------------------------------------#
#pilotTest <- Pilot_Idx[c(8,57,70,97,195,275,329,335,341,349,350,447,476)]

#Pilot Stores with store# only
# temp <- lapply(
#   competitor$Station.Name[Pilot_Idx[!(Pilot_Idx%in%pilotTest)]],
#   function(x){
#     if(!is.na(str_match(x, "#(.*?) ")[,2]) && str_count(str_match(x, "#(.*?) ")[,2])){
#       str_match(x, "#(.*?) ")[,2]
#     }
#     else if(!is.na(str_match(x, "#(.*?)")[,2]) && str_count(str_match(x, "#(.*?)")[,2])){
#       str_match(x, "#(.*?)")[,2]
#     }
#     else{
#       str_match(x, "# (.*?) ")[,2]
#     }
#   }
# )

##Pilot Stores with store# and special symbol
#Implies location only has disel lanes and doesn't own convenience store
# temp2 <- c()
# temp2 <- lapply(
#   competitor$Station.Name[pilotTest],
#   function(x){
#     if(!is.na(str_match(x, "#(.*?)- ")[,2]) && str_count(str_match(x, "#(.*?)- ")[,2])){
#       str_match(x, "#(.*?)- ")[,2]
#     }
#     else if(!is.na(str_match(x, "#(.*?)-")[,2]) && str_count(str_match(x, "#(.*?)-")[,2])){
#       str_match(x, "#(.*?)-")[,2]
#     }
#     else if(!is.na(str_match(x, "# (.*?)- ")[,2]) && str_count(str_match(x, "# (.*?)- ")[,2])){
#       str_match(x, "#(.*?)-")[,2]
#     }
#     else{
#       str_match(x, "#(.*?)/")[,2]
#     }
#   }
# )

#Final list of standardized Pilot names
#finalTemp <- c(temp,temp2)

# j<-1
# for(i in Pilot_Idx){
#   competitor$Station.Name[i] <- paste0("Pilot #",finalTemp[[j]][1])
#   j<-j+1 }
