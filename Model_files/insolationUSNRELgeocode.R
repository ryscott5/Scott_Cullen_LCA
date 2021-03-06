require(raster)
library(maptools)
library(ggplot2)
library(grid)
library(raster)
library(rasterVis)
library(rgdal)
library(shapefiles)
library(maps)
union<-readOGR("energy/intersect", layer="intersect")
require(MASS)
summary(union$Capacity_b)
solonly<-union
solonly<-na.omit(solonly)
solonly<-subset(solonly, Capacity_b!="NA")
solonly$Capacity_b<-factor(solonly$Capacity_b, c("Solar = 1 MW","Solar = 5 MW","Solar = 2 MW","Solar = 1.2 MW","Solar = 1.5 MW","Solar = 1.3 MW","Solar = 0.9 MW","Solar = 10 MW", "Solar = 1.1 MW", "Solar = 20 MW", "Solar = 3 MW", "Solar = 1.9 MW"))
solarinstalledinsol<-data.frame(solonly$ANNUAL)
solarinstalledinsol$GRIDCODE<-solonly$GRIDCODE
head(solarinstalledinsol)
rm(union)
rm(solonly)
