### FIRST ATTEMPT. DO NOT USE! USE profToPolyByClass.R INSTEAD!

require(RANN) #package for nearest neighbor

centroid <- read.csv("zamsoils_centroid.csv", header = TRUE) #shapefile data

profiles <- read.csv("Zambia_soil_profiles.csv", header = TRUE) #ZARI data
profiles$FAO.Classification <- as.character(profiles$FAO.Classification) #classification factor to character
#retain the second word of the classification to match with the shapefile soiltype
for (i in 1:nrow(profiles))
  profiles$FAO.Classification[i] <- strsplit(profiles$FAO.Classification[i]," ")[[1]][2]
#if it ends in "l" append an "s" to match the shapefile attribute table
endInL <- grep(pattern = "[:alpha:]*l", x = profiles$FAO.Classification)
profiles$FAO.Classification[endInL] <- paste0(profiles$FAO.Classification[endInL], "s")

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

# Nearest Neighbor approach (found not to work! too many classification/soil type mismatches!) 
nn <- nn2(newprofiles[,4:5], query = centroid[,2:3], k = 1)
centroid$nearest <- nn$nn.idx

#for (i in 1:nrow(centroid))
#  centroid$match[i] <- newprofiles$FAO.Classification[centroid$nearest[i]]