## Data processing for durations for the Google API results

library(rjson)
library(tidyverse)
library(dplyr)
library(jsonlite)

stops <- read.csv("narrow_stops.csv")

stops$address = toupper(stops$address)
stops$cityName1 = toupper(stops$cityName1)
stops$stateName = toupper(stops$stateName)

stops$fullAdress =  paste(stops$address,stops$cityName1,
                          stops$stateName, stops$zipcode)

exportJSON <- toJSON(stops)
write(exportJSON, "stops.json")

### Import and process duration file
duration_json <- fromJSON("duration_json4.json")

nrow(duration_json) 
n = sqrt(nrow(duration_json))

n=100
durationMatrix = matrix( nrow=n, ncol=n)
durationMatrix


for(i in seq(1, n, 1)) {
  for(j in seq(1, n, 1)) {
    origin_destination = duration_json %>% 
      filter(origin_id_reference == i) %>%  
      filter(destination_id_reference == j) 
    durationMatrix[i,j] = origin_destination$duration[1] 
  }
}

View(durationMatrix)

colnames(durationMatrix) = c(seq(1:n))

for(i in seq(1, n, 1)) {
  for(j in seq(1, n, 1)) {
    if(is.na(durationMatrix[i,j])){
      durationMatrix[i,j] = durationMatrix[j,i]
    }
  }
}

write.csv(durationMatrix, "duration.csv")

