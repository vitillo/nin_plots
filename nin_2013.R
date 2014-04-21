library(dplyr)
library(ggplot2)
library(maptools)
library(rgdal)
library(rgeos)

nin <- read.csv("nin/nino-registrations-overseas-borough-2013.csv",header=TRUE,colClasses="character")

italians <- nin[1:33,] %.% 
  select(name = Area, Italy) %.%
  mutate(Italy = as.numeric(gsub(",", "", Italy)),
         name = factor(name)) %.%
  arrange(Italy)

sport <- readOGR(dsn = ".", "london_sport")
proj4string(sport) <- CRS("+init=epsg:27700")
sport.f <- fortify(sport, region = "ons_label")
sport.f <- merge(sport.f, sport@data, by.x = "id", by.y = "ons_label")
sport.f <- inner_join(italians, sport.f)

label_point <- data.frame(coordinates(sport), name=sport@data$name)

# Don't overlap with COL
label_point[label_point$name == "Tower Hamlets",]$X2 = label_point[label_point$name == "Tower Hamlets",]$X2 + 500

ggplot() + geom_polygon(data=sport.f, aes(long, lat, group = group, fill = Italy)) + 
  coord_equal() + labs(x = "Easting (m)", y = "Northing (m)", fill = "# Italians") + 
  ggtitle("NIN registration of Italians in 2013") +
  geom_point(aes(x = X1, y = X2), data=label_point, alpha=0) + 
  geom_text(color="white", size = 4, aes(x = X1, y = X2, label=name), data=label_point) +
  theme_bw()