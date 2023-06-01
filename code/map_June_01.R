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
library(lubridate)
library(ggspatial)
library(magrittr)
library(scico)
library(ggrepel)
library(RColorBrewer)
library(rayshader)

thm1<-scale_fill_viridis_d(option="mako",begin=0,end=1,direction = 1)

thm2<-scale_fill_scico_d(palette = "bukavu",begin=0,end=1,direction = -1)

thm3<-c("#e7d4e8","#d9f0d3","#762a83","#af8dc3","#1b7837","#7fbf7b")

thm4<-scale_fill_brewer(palette="PRGn",direction =-1,type="diverging")

mytheme<-theme(axis.line=element_blank(),
               axis.text.x=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks=element_blank(),
               axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               legend.position="bottom",
               legend.text=element_text(10),
               panel.background=element_blank(),
               panel.border=element_blank(),
               panel.grid.major=element_blank(),
               panel.grid.minor=element_blank()
               )



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
           case_when(id=="Central"~ "3.Central",
                     id=="Central West"~"3.Central",
                     id=="Mississauga Halton"~"3.Central",
                     id=="North Simcoe Muskoka"~"3.Central",
                     id=="Central East"~"4.East",
                     id=="South East"~"4.East",
                     id=="Champlain"~"4.East",
                     id=="North East"~"5.North East",
                     id=="North West"~"6.North West",
                     id=="Toronto Central"~"1.Toronto",
                     id=="South West"~"2.West",
                     id=="Hamilton Niagara Haldimand Brant"~"2.West",
                     id=="Waterloo Wellington"~"2.West",
                     id=="Erie St. Clair"~"2.West"
           )
  )

# map to see if it works
# ggplot(data=fort_lhin,aes(x=long, y=lat,group=group,fill=Health_Region))+
#   geom_polygon()+
#   mytheme


## get clean dataset of the survey to get cities

source(here::here("code","data_preparation_map.R"))

# get percentage of having received the vaccine by city


perc<-clean_data %>% 
  group_by(city,Health_Region) %>% 
  summarise(Percent = sum(first_dose == "yes")/n())

perc<-clean_data %>% 
  group_by(city) %>% 
  count()

perc$Percent<-perc$n

perc$lg<-log(perc$Percent)

## for plotting the map, fix five names that are different between the clean dataset
## and the data from Ontario:  Amherstburg(is mispelled Amhertsburg), Big Trout Lake 
## (called Kitchenuhmaykoosib Aaki 84 in the dataset), Orleans (spelled Orléans),
## Newcastle (appears as New Castle Village), Mitchell/Ontario (appears as "Mitchell"),
## also, Fallingbrook did not appear in the dataset from Ontario and is not included in the
## final plot.

perc$city[perc$city=="Amhertsburg"] <- "Amherstburg"

perc$city[perc$city=="Big Trout Lake"] <- "Kitchenuhmaykoosib Aaki 84"

perc$city[perc$city=="Orleans"] <- "Orléans"

perc$city[perc$city=="Newcastle"] <- "Newcastle Village"

perc$city[perc$city=="Mitchell/Ontario"] <- "Mitchell"




#join to get coordinates for each city
test<-left_join(perc,fort_on, by=c( "city" = "GEONAME"))

## get centroid coordinates for LHINs

centroids.df <- as.data.frame(coordinates(lhins1))

# add labels and change order so only the ones to be plotted are in sequence

centroids.df$label <- c(0,2,0,0,0,0,1,0,0,4,0,3,5,6)

centroids.df<-centroids.df %>%
  filter(label!=0)

## add names of Health Regions

centroids.df$names<-c("West","Toronto","East","Central", "North East","North West")

centroids.df$names <- as.factor(paste(centroids.df$label, ".", centroids.df$names))
# 

breaks <- c(1,10,100,2000)

m1<-ggplot(data=fort_lhin,aes(x=long, 
                              y=lat,
                              group=group
                             )
           )+
  geom_polygon(aes(fill=Health_Region),
               size=0.2,
               alpha=0.8,
               show.legend = FALSE)+
  geom_point(data=test,
             aes(x=long,
                 y=lat,
                 color=Percent
             ),
             alpha=1,
             inherit.aes = FALSE,
             show.legend = TRUE
             )+
  geom_path(data=fort_lhin,aes(x=long,
                               y=lat,
                               group=group),
            color="black",
            size=0.2,
            alpha=0.5,
            show.legend = FALSE,
            inherit.aes = FALSE)+
  scale_color_gradient2(low = "#191900", 
                        mid = "blue", 
                        high = "#f2ea0a",
                        midpoint=1,
                        trans="log",
                        breaks = breaks,
                        name="Observations"
                        )+
  geom_label_repel(data=centroids.df,
                   inherit.aes = FALSE,
                   aes(label = label,
                       x = V1,
                       y = V2
                   ),
                   nudge_y = 10,
                   size=3,
                   fontface="bold",
                   color="black",
                   show.legend = TRUE
  )+
  geom_point(data=test%>%
               subset(city=="Toronto"),
             aes(x=long,
                 y=lat
             ),
             alpha=1,
             inherit.aes = FALSE,
             show.legend = FALSE,
             color="#f2ea0a"
  )+
  annotation_scale(
                   location = "tr",
                   bar_cols = c("grey60", "white")) +
  annotation_north_arrow(
    location = "tr", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
    )
  )+
guides(fill=guide_legend(title="Health Region",override.aes = list(label="")))+
scale_fill_manual(values=thm3)+
  mytheme
  

m1

## the figure was saved using the Export -> save as pdf option from RStudio with 
## size 8 x 9 in landscape
