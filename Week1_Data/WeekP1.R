install.packages(c("sf", "tmap", "tmaptools", "RSQLite", "tidyverse"), 
                 repos = "https://www.stats.bris.ac.uk/R/")
install.packages("rgdal")
library(rgdal)
library(sf)
# change this to your file path!!!
shape <- st_read("/Users/jay/Library/CloudStorage/OneDrive-UniversityCollegeLondon/GIS&S/Week1/INCLASS/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
summary(shape)
plot(shape)
library(sf)
shape %>% 
  st_geometry() %>%
  plot()
library(tidyverse)
#this needs to be your file path again
mycsv <-  read_csv("/Users/jay/Library/CloudStorage/OneDrive-UniversityCollegeLondon/GIS&S/Week1/INCLASS/fly-tipping-borough.csv")  
mycsv 
shape <- shape%>%
  merge(.,
        mycsv,
        by.x="GSS_CODE", 
        by.y="Row Labels")
shape%>%
  head(., n=10)
library(tmap)
tmap_mode("plot")
# change the fill to your column name if different
shape %>%
  qtm(.,fill = "2011-12")
shape %>%
  st_write(.,"/Users/jay/Library/CloudStorage/OneDrive-UniversityCollegeLondon/GIS&S/Week1/INCLASS/Rwk1.gpkg",
           "london_boroughs_fly_tipping",
           delete_layer=TRUE)
library(readr)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(),dbname="/Users/jay/Library/CloudStorage/OneDrive-UniversityCollegeLondon/GIS&S/Week1/INCLASS/Rwk1.gpkg")
con %>%
  dbListTables()
con %>%
  dbWriteTable(.,
               "original_csv",
               mycsv,
               overwrite=TRUE)

con %>% 
  dbDisconnect()
