#Package for geospatial distance between two points
if(!require(geosphere)){
  install.packages("geosphere")
  require(geosphere)
}
if(!require(stringr)){
  install.packages("stringr")
  require(stringr)
}


#Set working directory where files exist
setwd("~/Desktop/Pilot/Data")
#Main locations file
PFJ <- read.csv("./Pilot_Updated/Master_PFJ_US_Locations_Aggregated.csv")
#List of Travel Centers
travelCenters <- read.csv("./Pilot_Updated/Location_To_Opis.csv")
#Incomplete list of competitors
competitor <- read.csv("Matt_Price_Competitor_List.csv")
#Subset data frames to useful information
PFJ_Unique <- unique(PFJ[,c(1,25,26)])
PFJ_Idx <- which(PFJ_Unique$location_id%in%travelCenters$LOCATION_ID)

DF1_Dist <- PFJ_Unique[PFJ_Idx,]
numDF1 <- nrow(DF1_Dist)
DF1_Dist$STATION_NAME<- paste0(rep("PFJ_", numDF1), DF1_Dist$location_id)

##Begin Data Cleaning

#Competitor Dataset
# competitor$Station.Name<-str_trim(as.character(competitor$Station.Name))
# competitor$Station.Name<-as.factor(competitor$Station.Name)
competitor$City <- str_trim(competitor$City)
competitor$Longitude <- round(competitor$Longitude, 4)
competitor$Latitude <- round(competitor$Latitude, 4)

#Standardize PFJ names
anti_comp_Idx <- which(competitor$OPIS.ID%in%travelCenters$OPIS_TRUCKSTOP_ID)

#Subset Competitors dataset
DF2_Dist <- competitor[-anti_comp_Idx,c(1,6,7)]
numDF2 <- nrow(DF2_Dist)
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

###---------------------------------------READ IN MATRICES---------------------------------------------###
# PFJ_Matrix_Source <- read.csv(paste0(getwd(),"/PFJ_Matrix.csv"))
# Comp_Matrix_Source <-  read.csv(paste0(getwd(),"/Competitor_Matrix.csv"))

##-------------------------PFJ Distance----------------------------##
PFJ_Matrix_Source <- PFJMatrix
Comp_Matrix_Source <- compMatrix

start=Sys.time()
PFJ_OneMile <- numeric()
PFJ_FiveMile <- numeric()
PFJ_FifteenMile <- numeric()
PFJ_MinDist <- numeric()
for(i in 1:nrow(PFJ_Matrix_Source)){
  PFJ_OneMile[i] <- sum(ifelse( !is.na(PFJ_Matrix_Source[i,]) & (PFJ_Matrix_Source[i,]>0) & (PFJ_Matrix_Source[i,]<1) ,1,0))
  PFJ_FiveMile[i] <- sum(ifelse( !is.na(PFJ_Matrix_Source[i,]) & (PFJ_Matrix_Source[i,]>=1) & (PFJ_Matrix_Source[i,]<5),1,0 ))
  PFJ_FifteenMile[i] <- sum(ifelse( !is.na(PFJ_Matrix_Source[i,]) & (PFJ_Matrix_Source[i,]>=5) & (PFJ_Matrix_Source[i,]<15),1,0 ))
  PFJ_MinDist[i] <- min(PFJ_Matrix_Source[i,], na.rm = TRUE)
  PFJ_Matrix_Source[i,i] <- ifelse(is.na(PFJ_Matrix_Source[i,i]),0,PFJ_Matrix_Source[i,i])
}
Sys.time()-start

##------------------------Competitor Distance------------------------##

start=Sys.time()
Comp_OneMile <- numeric()
Comp_FiveMile <- numeric()
Comp_FifteenMile <- numeric()
Comp_MinDist <- numeric()
for(i in 1:nrow(Comp_Matrix_Source)){
  Comp_OneMile[i] <- sum(ifelse( !is.na(Comp_Matrix_Source[i,]) & (Comp_Matrix_Source[i,]>0) & (Comp_Matrix_Source[i,]<1) ,1,0))
  Comp_FiveMile[i] <- sum(ifelse( !is.na(Comp_Matrix_Source[i,]) & (Comp_Matrix_Source[i,]>=1) & (Comp_Matrix_Source[i,]<5),1,0 ))
  Comp_FifteenMile[i] <- sum(ifelse( !is.na(Comp_Matrix_Source[i,]) & (Comp_Matrix_Source[i,]>=5) & (Comp_Matrix_Source[i,]<15),1,0 ))
  Comp_MinDist[i] <- min(Comp_Matrix_Source[i,], na.rm = TRUE)
  Comp_Matrix_Source[i,i] <- ifelse(is.na(Comp_Matrix_Source[i,i]),0,Comp_Matrix_Source[i,i])
  print(paste0( (i/nrow(Comp_Matrix_Source))*100, "%" ))
}
Sys.time()-start

PFJ_Matrix_Source <- cbind(PFJ_Matrix_Source, PFJ_OneMile, PFJ_FiveMile, PFJ_FifteenMile, PFJ_MinDist)
Comp_Matrix_Source <- cbind(Comp_Matrix_Source, Comp_OneMile, Comp_FiveMile, Comp_FifteenMile, Comp_MinDist)

fileName1 <- paste0(getwd(),"/PFJ_Matrix.csv")
fileName2 <- paste0(getwd(),"/Competitor_Matrix.csv")
write.csv(PFJ_Matrix_Source, file = fileName1)
write.csv(Comp_Matrix_Source, file = fileName2)
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
