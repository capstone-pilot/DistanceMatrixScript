#
if(!require(geosphere)){
  install.packages("geosphere")
  require(geosphere)
}
if(!require(beepr)){
  install.packages("beepr")
  require(beepr)
}

setwd("~/Desktop/Pilot/Data")
PFJ <- read.csv("Master_PFJ_US_Locations.csv")
competitor <- read.csv("Matt_Price_Competitor_List.csv")

DF1_Dist <- unique(PFJ[,c(1,7,8,10,11)])
DF2_Dist <- unique(competitor[,c(1,3:7)])

numDF1 <- nrow(DF1_Dist)
numDF2 <- nrow(DF2_Dist)
DistanceMatrix <- matrix( rep(NA,numDF1*numDF2), nrow = numDF1, ncol = numDF2 )
rownames(DistanceMatrix) <- seq(from=1, to=numDF1, by=1)
colnames(DistanceMatrix) <- seq(from=1, to=numDF2, by=1)

start=Sys.time()
for(i in 1:numDF1 ){
  for(j in 1:numDF2){
    DistanceMatrix[i,j] <- distHaversine( c(DF1_Dist$ADDRESS_LONGITUDE[i],DF1_Dist$ADDRESS_LATITUDE[i]), c(DF2_Dist$Longitude[j], DF2_Dist$Latitude[j]) )
  }
  print( paste0( ( ( i*j)/(numDF1*numDF2) )*100, "%" ) )
}
Sys.time()-start

fileName <- paste0(getwd(),"/Distance_Matrix.csv")
write.csv(DistanceMatrix, file = fileName)