library(sf)
library(here)
st_layers(here("Week3_Data", "gadm36_AUS_gpkg", "gadm36_AUS.gpkg"))
Ausoutline <- st_read(here("Week3_Data", "gadm36_AUS_gpkg", "gadm36_AUS.gpkg"), 
                      layer='gadm36_AUS_0')
print(Ausoutline)
st_crs(Ausoutline)$proj4string
Ausoutline <- Ausoutline %>%
  st_set_crs(., 4326)

#or more concisely
#Ausoutline <- st_read(here("prac3_data", "gadm36_AUS.gpkg"), 
                      #layer='gadm36_AUS_0') %>% 
  #st_set_crs(4326)

AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(.,3112)

print(AusoutlinePROJECTED)

#From sf to sp
AusoutlineSP <- Ausoutline %>%
  as(., "Spatial")

#From sp to sf
AusoutlineSF <- AusoutlineSP %>%
  st_as_sf()

library(raster)
library(terra)
jan<-terra::rast(here("Week3_Data", "wc2", "wc2.1_5m_tavg_01.tif"))
# have a look at the raster layer jan
jan
plot(jan)

# set the proj 4 to a new object

pr1 <- terra::project(jan, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#newproj<-"ESRI:54009"
# get the jan raster and give it the new proj4
#pr1 <- jan %>%
  #terra::project(., newproj)
plot(pr1)
pr1 <- pr1 %>%
  terra::project(., "EPSG:4326")
plot(pr1)
library(fs)
dir_info("Week3_Data/")

library(tidyverse)
listfiles<-dir_info("Week3_Data/wc2/") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()

#have a look at the file names 
listfiles

worldclimtemp <- listfiles %>%
  terra::rast()

#have a look at the raster stack
worldclimtemp

# access the january layer
worldclimtemp[[1]]

month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(worldclimtemp) <- month
worldclimtemp$Jan

site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
          "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
          "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")
# Extract the data from the Rasterstack for all points 
AUcitytemp<- terra::extract(worldclimtemp, samples)

Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>% 
  add_column(Site = site, .before = "Jan")
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")

Perthtemp <- Aucitytemp2[3,]
hist(as.numeric(Perthtemp))

library(tidyverse)
#define where you want the breaks in the historgram
userbreak<-c(8,10,12,14,16,18,20,22,24,26)

# remove the ID and site columns
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")

t<-Perthtemp %>%
  dplyr::select(Jan:Dec)

hist((as.numeric(t)), 
     breaks=userbreak, 
     col="red", 
     main="Histogram of Perth Temperature", 
     xlab="Temperature", 
     ylab="Frequency")

histinfo <- as.numeric(t) %>%
  as.numeric()%>%
  hist(.)
histinfo

plot(Ausoutline$geom)

AusoutSIMPLE <- Ausoutline %>%
  st_simplify(., dTolerance = 1000) %>%
  st_geometry()%>%
  plot()

print(Ausoutline)
crs(worldclimtemp)
Austemp <- Ausoutline %>%
  # now crop our temp data to the extent
  terra::crop(worldclimtemp,.)

# plot the output
plot(Austemp)

exactAus<-terra::mask(Austemp, Ausoutline)

#subset using the known location of the raster
hist(exactAus[[3]], col="red", main ="March temperature")

exactAusdf <- exactAus %>%
  as.data.frame()
library(ggplot2)
# set up the basic histogram
gghist <- ggplot(exactAusdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))
squishdata<-exactAusdf%>%
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )
twomonths <- squishdata %>%
  # | = OR
  filter(., Month=="Jan" | Month=="Jun")

meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))
meantwomonths

ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=meantwomonths, 
             aes(xintercept=mean, 
                 color=Month),
             linetype="dashed")+
  labs(title="Ggplot2 histogram of Australian Jan and Jun
       temperatures",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

data_complete_cases <- squishdata %>%
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))

# Plot faceted histogram
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))

data_complete_cases <- squishdata %>%
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))

# Plot faceted histogram
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))

library(plotly)
# split the data for plotly based on month

jan <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jan")
jun <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jun")
# give axis titles
x <- list (title = "Temperature")
y <- list (title = "Frequency")
# set the bin width
xbinsno<-list(start=0, end=40, size = 2.5)
# plot the histogram calling all the variables we just set
ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)

ihist

# mean per month
meanofall <- squishdata %>%
  group_by(Month) %>%
  summarise(mean = mean(Temp, na.rm=TRUE))

# print the top 1
head(meanofall, n=1)
