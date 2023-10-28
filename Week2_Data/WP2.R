A <- 1
B <- 2
C <- A+B
C
ls()
rm(A)
X<-function(data, argument1, argument2, argument3)
  
#create some datasets, first a vector of 1-100 and 101-200
Data1 <- c(1:100)
Data2 <- c(101:200)
#Plot the data
plot(Data1, Data2, col="red")

#just for fun, create some more, this time some normally distributed
#vectors of 100 numbers
Data3 <- rnorm(100, mean = 53, sd=34)
Data4 <- rnorm(100, mean = 64, sd=14)
#plot
plot(Data3, Data4, col="blue")

df <- data.frame(Data1, Data2)
plot(df, col="green")

library(tidyverse)
#show the first 10 and then last 10 rows of data in df...
df %>%
  head()

df %>%
  tail()

df[1:10, 1]
df[5:15,]
df[c(2,3,6),2]
df[,1]

library(dplyr)
df <- df %>%
  dplyr::rename(column1 = Data1, column2=Data2)
df %>% 
  dplyr::select(column1)
df$column1
df[["column1"]]

LondonDataOSK<- read.csv("ward-profiles-excel-version.csv", 
                         header = TRUE, 
                         sep = ",",  
                         encoding = "latin1")
#LondonDataOSK<- read.csv("ward-profiles-excel-version.csv", 
                         #sep=",")
install.packages("here")
library(here)
here::here()
LondonDataOSK<- read.csv(here::here("ward-profiles-excel-version.csv"), 
                                     header = TRUE, sep = ",",  
                                     encoding = "latin1")
#wang the data in straight from the web using read_csv, 
#skipping over the 'n/a' entries as you go...
LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                       locale = locale(encoding = "latin1"),
                       na = "n/a")
class(LondonData)
# or, if you have your old skool data
class(LondonDataOSK)

Datatypelist <- LondonData %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

LondonData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                       locale = locale(encoding = "latin1"))
#install.packages("XQuartz")?
#LondonData <- edit(LondonData)
summary(df)

LondonBoroughs<-LondonData[626:658,]
LondonBoroughs<-LondonData%>%
  slice(626:658)
Femalelifeexp<- LondonData %>% 
  filter(`Female life expectancy -2009-13`>90)
LondonBoroughs<- LondonData %>% 
  filter(str_detect(`New code`, "^E09"))
LondonBoroughs$`Ward name`
LondonBoroughs %>% 
  dplyr::select(`Ward name`) %>%
  print()
LondonBoroughs<-LondonBoroughs %>%
  distinct()
#select columns 1,19,20 and 21
LondonBoroughs_manualcols<-LondonBoroughs[,c(1,19,20,21)]
#select columns 1,19,20 and 21
LondonBoroughs_dplyrcols<-LondonBoroughs %>%
  dplyr::select(c(1,19,20,21))
LondonBoroughs_contains<-LondonBoroughs %>% 
  dplyr::select(contains("expectancy"), 
                contains("obese - 2011/12 to 2013/14"),
                contains("Ward name")) 

install.packages('janitor')?
library(janitor)

LondonBoroughs <- LondonBoroughs %>%
  dplyr::rename(Borough=`Ward name`)%>%
  clean_names()

LondonBoroughs <- LondonBoroughs %>%
  #here the ., means all data
  clean_names(., case="big_camel")

Life_expectancy <- LondonBoroughs %>% 
  #new column with average of male and female life expectancy
  mutate(averagelifeexpectancy= (FemaleLifeExpectancy2009_13 +
                                   MaleLifeExpectancy2009_13)/2)%>%
  #new column with normalised life expectancy
  mutate(normalisedlifeepectancy= averagelifeexpectancy /
           mean(averagelifeexpectancy))%>%
  #select only columns we want
  dplyr::select(NewCode,
                Borough,
                averagelifeexpectancy, 
                normalisedlifeepectancy)%>%
  #arrange in descending order
  #ascending is the default and would be
  #arrange(normalisedlifeepectancy)
  arrange(desc(normalisedlifeepectancy))

#top of data
slice_head(Life_expectancy, n=5)
#bottom of data
slice_tail(Life_expectancy,n=5)

Life_expectancy2 <- Life_expectancy %>%
  mutate(UKcompare = case_when(averagelifeexpectancy>81.16 ~ "above UK average",
                               TRUE ~ "below UK average"))
Life_expectancy2

Life_expectancy2_group <- Life_expectancy2 %>%
  mutate(UKdiff = averagelifeexpectancy-81.16) %>%
  group_by(UKcompare)%>%
  summarise(range=max(UKdiff)-min(UKdiff), count=n(), Average=mean(UKdiff))

Life_expectancy2_group

Life_expectancy3 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(where(is.numeric), round, 3))%>%
  mutate(across(UKdiff, round, 0))%>%
  mutate(UKcompare = case_when(averagelifeexpectancy >= 81 ~ 
                                 str_c("equal or above UK average by",
                                       UKdiff, 
                                       "years", 
                                       sep=" "), 
                               TRUE ~ str_c("below UK average by",
                                            UKdiff,
                                            "years",
                                            sep=" ")))%>%
  group_by(UKcompare)%>%
  summarise(count=n())
Life_expectancy3
Life_expectancy4 <- Life_expectancy %>%
  mutate(UKdiff = averagelifeexpectancy-81.16)%>%
  mutate(across(is.numeric, round, 3))%>%
  mutate(across(UKdiff, round, 0))
plot(LondonBoroughs$MaleLifeExpectancy2009_13,
     LondonBoroughs$PercentChildrenInReceptionYearWhoAreObese2011_12To2013_14)
install.packages("plotly")
library(plotly)
plot_ly(LondonBoroughs, 
        #data for x axis
        x = ~MaleLifeExpectancy2009_13, 
        #data for y axis
        y = ~PercentChildrenInReceptionYearWhoAreObese2011_12To2013_14, 
        #attribute to display when hovering 
        text = ~Borough, 
        type = "scatter", 
        mode = "markers")
install.packages("maptools")
install.packages(c("classInt", "tmap"))

# might also need these ones
install.packages(c("RColorBrewer", "sp", "rgeos", 
                   "tmaptools", "sf", "downloader", "rgdal", 
                   "geojsonio"))
#Load Packages (ignore any error messages about being built under a 
#different R version):
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(tmap)
library(tmaptools)
library(sf)
library(rgdal)
library(geojsonio)
# this will take a few minutes
# geojson in local folder
#EW <- st_read(here::here("prac2_data",
#                         "Local_Authority_Districts__December_2015__Boundaries.geojson"))

# shapefile in local folder
EW <- st_read(here::here("Week2_Data",
                         "Local_Authority_Districts_(December_2015)_Boundaries",
                         "Local_Authority_Districts_(December_2015)_Boundaries.shp"))
LondonMap<- EW %>%
  filter(str_detect(lad15cd, "^E09"))

#plot it using the qtm function
qtm(LondonMap)

LondonData <- clean_names(LondonData)

#EW is the data we read in straight from the web
BoroughDataMap <- EW %>%
  clean_names()%>%
  # the . here just means use the data already loaded
  filter(str_detect(lad15cd, "^E09"))%>%
  merge(.,
        LondonData, 
        by.x="lad15cd", 
        by.y="new_code",
        no.dups = TRUE)%>%
  distinct(.,lad15cd,
           .keep_all = TRUE)
tmap_mode("plot")
qtm(BoroughDataMap, 
    fill = "rate_of_job_seekers_allowance_jsa_claimants_2015")
install.packages("rJava")
install.packages("OpenStreetMap")
  
library(OpenStreetMap)
library(rJava)

tmaplondon <- BoroughDataMap %>%
  st_bbox(.) %>% 
  tmaptools::read_osm(., type = "osm", zoom = NULL)

tmap_mode("plot")
tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(BoroughDataMap) + 
  tm_polygons("rate_of_job_seekers_allowance_jsa_claimants_2015", 
              style="jenks",
              palette="YlOrBr",
              midpoint=NA,
              title="Rate per 1,000 people",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Job seekers' Allowance Claimants", legend.position = c("right", "bottom"))
Life_expectancy4map <- EW %>%
  inner_join(., 
             Life_expectancy4,
             by = c("lad15cd" = "NewCode"))%>%
  distinct(.,lad15cd, 
           .keep_all = TRUE)
tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(Life_expectancy4map) + 
  tm_polygons("UKdiff", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              title="Number of years",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in life expectancy", legend.position = c("right", "bottom"))

flytipping <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv")
flytipping1 <- read_csv("https://data.london.gov.uk/download/fly-tipping-incidents/536278ff-a391-4f20-bc79-9e705c9b3ec0/fly-tipping-borough.csv", 
                        col_types = cols(
                          code = col_character(),
                          area = col_character(),
                          year = col_character(),
                          total_incidents = col_number(),
                          total_action_taken = col_number(),
                          warning_letters = col_number(),
                          fixed_penalty_notices = col_number(),
                          statutory_notices = col_number(),
                          formal_cautions = col_number(),
                          injunctions = col_number(),
                          prosecutions = col_number()
                        ))
# view the data
view(flytipping1)
#convert the tibble into a tidy tibble
flytipping_long <- flytipping1 %>% 
  pivot_longer(
    cols = 4:11,
    names_to = "tipping_type",
    values_to = "count"
  )
# view the data
view(flytipping_long)
flytipping2 <- flytipping1[,1:4]
#pivot the tidy tibble into one that is suitable for mapping
flytipping_wide <- flytipping_long %>% 
  pivot_wider(
    id_cols = 1:2,
    names_from = c(year,tipping_type),
    names_sep = "_",
    values_from = count
  )

view(flytipping_wide)

widefly <- flytipping2 %>% 
  pivot_wider(
    names_from = year, 
    values_from = total_incidents)
view(widefly)

wideflymap <- EW %>%
  inner_join(., 
             widefly,
             by = c("lad15cd" = "code"))%>%
  distinct(.,lad15cd, 
           .keep_all = TRUE)
tmap_mode("plot")

tm_shape(tmaplondon)+
  tm_rgb()+
  tm_shape(wideflymap) + 
  tm_polygons("2011-12", 
              style="pretty",
              palette="Blues",
              midpoint=NA,
              title="total_incidents",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Total Incidents for 2011_12", legend.position = c("right", "bottom"))
