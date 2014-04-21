library(dplyr)
library(ggplot2)
library(maptools)
library(rgdal)
library(rgeos)
library(reshape)
library(scales)

load.data <- function() {
  result <- data.frame()
  
  for(i in 2003:2013) {
    nin <- read.csv(paste("nin/nino-registrations-overseas-borough-", as.character(i), ".csv", sep =""),
                    header=TRUE, colClasses="character")
    
    colname <- paste("Italy", as.character(i))
    
    italians <- nin[1:33,] %.% 
      select(name = Area, Italy) %.%
      mutate(Italy = as.numeric(gsub(",", "", Italy)),
             name = factor(name))
    
    italians[, colname] <- italians$Italy
    italians$Italy <- NULL
    
    if (length(result) == 0) {
      result <- data.frame(name = italians$name)
    }
    
    result <- inner_join(result, italians)
  }
  
  return(result)
}

italians <- load.data()
sport <- readOGR(dsn = ".", "london_sport")
proj4string(sport) <- CRS("+init=epsg:27700")
sport.f <- fortify(sport, region = "ons_label")
sport.f <- merge(sport.f, sport@data, by.x = "id", by.y = "ons_label")
sport.f <- inner_join(italians, sport.f)
sport.f <- melt(sport.f, id = c("name", "lat", "long", "group", "order", "piece", "id", "Partic_Per", "Pop_2001", "hole"))

ggplot() + geom_polygon(data=sport.f, aes(long, lat, group = group, fill = value)) + 
  coord_equal() + labs(x = "Easting (m)", y = "Northing (m)", fill = "# Italians") + 
  ggtitle("NIN registrations of Italians") +
  geom_point(aes(x = X1, y = X2), data=label_point, alpha=0) + 
  theme_bw() +
  facet_wrap(~ variable) + scale_fill_gradient(low="white", high=muted("red"), space="Lab")