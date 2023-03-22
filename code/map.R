#stand-alone script to generate a map of Ontario with the cities included in the survey

###########################################################################
# IMPORTANT: The shapefiles used to create the map are too large to the uploaded to GitHub. Therefore, information
# on where the data can be obtained is provided below. The user needs to download the files
# from the links provided, unzip them, and place them in a directory where they can be read.
# In this case, the files were placed in the "map_data" folder within the "data" directory.

# Shapefile for LHINs
  ## Description for the dataset from the Ontario website:
  ## https://data.ontario.ca/dataset/ontario-s-health-region-geographic-data/resource/9fe4e91a-adff-4a34-bbe6-d43d596e4867
  ## the data can be downloaded from:
  # https://www150.statcan.gc.ca/n1/en/pub/82-402-x/2018001/data-donnees/boundary-limites/arcinfo/HRL_035b18a-eng.zip?st=QxbZnLnU


# Shapefile for cities 
  ## Description for the dataset:
  ## https://natural-resources.canada.ca/earth-sciences/geography/download-geographical-names-data/9245
  ##The data can be downloaded from:
  # https://ftp.cartes.canada.ca/pub/nrcan_rncan/vector/geobase_cgn_toponyme/prov_shp_eng/cgn_on_shp_eng.zip

library(rgdal)
library(sf)
library(viridis)
library(here)
library(tidyverse)
library(ggspatial)
library(magrittr)
library(scico)

thm1<-scale_fill_viridis_d(option="mako",begin=0,end=1,direction = 1)

thm2<-scale_fill_scico_d(palette = "nuuk",begin=0,end=1,direction = -1)

mytheme<-theme(axis.line=element_blank(),
               axis.text.x=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks=element_blank(),
               axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               legend.position="right",
               panel.background=element_blank(),
               panel.border=element_blank(),
               panel.grid.major=element_blank(),
               panel.grid.minor=element_blank(),
               plot.background=element_blank())



# use sf to read lhin data
lhins <- st_read(here("data","map_data","LHINs","HRL_035b18a_e.shp"))


# use sf to read Ontario data
on <- st_read(here("data","map_data","cgn_on_shp_eng","cgn_on_shp_eng.shp"))

# use sf to transform coordinates
on1<-st_transform(on,crs=st_crs(lhins))

### test using rgdal and fortify


# use rgdal to read the LHIN data again. Why? Because it seems easier to fortify to use with ggplot than using the 
# sf method
lhins1 <- readOGR("data/map_data/LHINs/HRL_035b18a_e.shp",stringsAsFactors = FALSE)

# fortify, keep the LHIN English names
fort_lhin <- fortify(lhins1,region="ENGNAME")


# fortify
fort_on <- fortify(on1)


 # get coordinates (code from https://stackoverflow.com/questions/54734771/sf-write-lat-long-from-geometry-into-separate-column-and-keep-id-column)

fort_on <- fort_on %>%
             mutate(long = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

## there are many places in this dataset that are repeated entries (many geographic areas as lakes, points, etc). Keep only 
# the category of "Populated place" and remove duplicates

fort_on<-fort_on %>% 
  subset(CATEGORY=="Populated Place" | CATEGORY=="Administrative Area") %>%
  group_by(GEONAME) %>% 
  filter(duplicated(GEONAME) | n()==1)

#remove entries with duplicated name

fort_on<-fort_on %>% distinct(GEONAME, .keep_all = TRUE)

# Assign Health Regions to the LHIN dataset

fort_lhin<-fort_lhin %>%
  mutate(Health_Region=
           case_when(id=="Central"~ "Central",
                     id=="Central West"~"Central",
                     id=="Mississauga Halton"~"Central",
                     id=="North Simcoe Muskoka"~"Central",
                     id=="Central East"~"East",
                     id=="South East"~"East",
                     id=="Champlain"~"East",
                     id=="North East"~"North East",
                     id=="North West"~"North West",
                     id=="Toronto Central"~"Toronto",
                     id=="South West"~"West",
                     id=="Hamilton Niagara Haldimand Brant"~"West",
                     id=="Waterloo Wellington"~"West",
                     id=="Erie St. Clair"~"West"
           )
  )

# map to see if it works
# ggplot(data=fort_lhin,aes(x=long, y=lat,group=group,fill=Health_Region))+
#   geom_polygon()+
#   mytheme


## get clean dataset of the survey to get cities

source(here::here("code","data_preparation_map.R"))

# get percentage of having received the vaccine by city


perc<-clean_data %>% group_by(city,Health_Region) %>% 
  summarise(Percent = sum(first_dose == "yes")/n())

## for plotting the map, fix two names that are different between the clean dataset
## and the data from Ontario:  Amherstburg(is mispelled Amhertsburg) and Big Trout Lake 
## (called Kitchenuhmaykoosib Aaki 84 in the dataset)

perc$city[perc$city=="Amhertsburg"] <- "Amherstburg"

perc$city[perc$city=="Big Trout Lake"] <- "Kitchenuhmaykoosib Aaki 84"


#join to get coordinates for each city
test<-left_join(perc,fort_on, by=c( "city" = "GEONAME"))

  
m1<-ggplot(data=fort_lhin,aes(x=long, y=lat,group=group,fill=Health_Region))+
  geom_polygon(color="#808080")+
  geom_point(data=test,
             inherit.aes = FALSE,
             show.legend = FALSE,
             color="#f98e09",
             fill="#fcffa4",
             shape=21,
             size=0.6,
             stroke=0.7,
             aes(x=long,
                 y=lat,
                 alpha=0.8))+
  annotation_scale(
                   location = "tr",
                   bar_cols = c("grey60", "white"),
                  # text_family = "Calibri"
                 ) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      #text_family = "Calibri"
    )
  )+
 guides(fill=guide_legend(title="Health Region"))+
  mytheme+
  thm1

m1

ggsave(here("data","map_data","map.pdf"))
