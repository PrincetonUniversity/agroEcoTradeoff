require(RANN) #package for nearest neighbor

class <- read.csv("Zambia_soil_classify.csv", header = TRUE) #classification from Soil of Zambia tiff
class$SOILSYMB <- as.character(class$SOILSYMB)
class$SOILTYPE <- as.character(class$SOILTYPE) #soiltypes must be characters for strsplit later
class$SOILTYPE2 <- as.character(class$SOILTYPE2)
class$SOILTYPE3 <- as.character(class$SOILTYPE3)

centroid <- read.csv("zamsoils_centroid.csv", header = TRUE) #shapefile data
centroid$SOILSYMB <- as.character(centroid$SOILSYMB)
centroid$class1 <- 0
centroid$class2 <- 0
centroid$class3 <- 0

for (i in 1:nrow(centroid)) {
  symbolmatch <- which(class$SOILSYMB == centroid$SOILSYMB[[i]])
  if (length(symbolmatch) == 1) {
    centroid$class1[i] <- class[symbolmatch, 2]
    centroid$class2[i] <- class[symbolmatch, 3]
    centroid$class3[i] <- class[symbolmatch, 4]
  }
}


profiles <- read.csv("Zambia_soil_profiles.csv", header = TRUE) #ZARI data
profiles$FAO.Classification <- as.character(profiles$FAO.Classification) #classification factor to character

#Each unique soil profile on just one line
newprofiles <- profiles[1,]
j <- 1
k <- 1
for (i in 2:nrow(profiles)) {
  if (newprofiles[j, 4] == profiles[i, 4]) {
    for (m in 1:26) {
      newprofiles[j, 31 + (k-1)*26 + m] <- profiles[i, m + 5]
    }
    k <- k + 1
  }
  else {
    newprofiles[j+1, 1:ncol(profiles)] <- profiles[i,]
    j <- j + 1
    k <- 1
  }
}

#retain the second word of the classification to match with the shapefile soiltype
for (i in 1:nrow(newprofiles))
  newprofiles$Classification[i] <- strsplit(newprofiles$FAO.Classification[i]," ")[[1]][2]
newprofiles$Classification <- as.character(newprofiles$Classification) #classification factor to character

bestmatch <- NULL

for (i in 1:nrow(centroid)) {
  matches <- NULL
  
  #Try to match the classification exactly
  for (j in (ncol(centroid)-2):ncol(centroid))
    matches <- c(matches, which(newprofiles$FAO.Classification == centroid[i,j]))
  
  #If no exact matches, just try to match the first level class
  if (length(matches) == 0) {
    for (j in (ncol(centroid)-2):ncol(centroid))
      matches <- c(matches, which(newprofiles$Classification == strsplit(centroid[i,j]," ")[[1]][2]))
  }
  
  #If still no matches, use the nearest neighbor
  if (length(matches) == 0)
    bestmatch[i] <- nn2(newprofiles[,4:5], query = centroid[i, 2:3], k = 1)[1]
  else {
    #Find the nearest neighbor among matches
    bestmatch[i] <- matches[as.numeric(nn2(newprofiles[matches, 4:5], query = centroid[i, 2:3], k = 1)[1])]
  }
}

#List the best matches in the centroid data frame
centroid$match <- bestmatch
centroid$match <- as.numeric(centroid$match)

#Create the attribute table
at <- data.frame(NULL)
for (i in 1:nrow(centroid)) {
  at <- rbind(at, newprofiles[centroid$match[i],])
}

at$Classification <- NULL


polyIdent <- 5#Number of nonrepeated columns from profiles in newprofiles
polyRep <- ncol(profiles) - polyIdent #Number of repeated columns
#Maximum number of horizons for one polygon
nreps <- (ncol(at) - (polyIdent))/(polyRep)  

#Column names for at
newNames <- names(at)[1:ncol(profiles)]
for (i in 1:(nreps - 1)){
  newNames <- c(newNames, newNames[(polyIdent+1):ncol(profiles)])
}
names(at) <- newNames

#Add the soil ID to be used as the common attribute for the join
at$SOILS_ID <- centroid$SOILS_ID

#Write the table
write.table(at, file = "Zambia_soil_at.csv", sep = ",", col.names = TRUE, row.names = FALSE)

#Create a csvt file for the join
colTypes <- c("Integer", "String", "String", "Real", "Real", "String", "Integer", "Integer", "String", "String", 
              "Real", "Real", "Real", "Real", "Real", "Real", "Real", "Real", "Real", "Real",
              "Real", "Real", "Real", "Real", "Real", "Integer", "Integer", "Integer", "String", "Real", "Real")
for (i in 1:(nreps - 1)){
  colTypes <- c(colTypes, colTypes[(polyIdent+1):ncol(profiles)])
}
colTypes <- c(colTypes, "Real") #For soil ID

write.table(data.frame(t(colTypes)), file = "Zambia_soil_at.csvt", sep = ",", col.names = FALSE, row.names = FALSE)


