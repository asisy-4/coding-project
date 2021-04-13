

rm(list = ls())
getwd() 
setwd("") 


install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

###########################
# 2.Draw map with polygon #
###########################

# Load data from sf library 
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Data and basic plot (ggplot and geom_sf)
ggplot(data = world) +
  geom_sf()

# Add Title, subtitle, and axis labels (ggtitle, xlab, ylab)
ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$name)), " countries)"))

# Map color (geom_sf)
ggplot(data = world) + 
  geom_sf(color = "black", fill = "lightgreen")

ggplot(data = world) +
  geom_sf(aes(fill = pop_est), color="white", size=0.5) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")

# Projection and extent (coord_sf)
ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = "+init=epsg:3035")

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = st_crs(3035))

# Restrict to China
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(73, 136), ylim = c(10, 54), expand = FALSE)

# Scale bar and North arrow (package ggspatial)
library("ggspatial")
ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(73, 136), ylim = c(10, 54))

# Country names and other names (geom_text and annotate)
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

ggplot(data = world) +
  geom_sf() +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkblue", check_overlap = T, fontface = "bold", size=3) +
  annotate(geom = "text", x = 114, y = 16, label = "South China Sea", 
           fontface = "italic", color = "grey22", size = 4) +
  coord_sf(xlim = c(73, 136), ylim = c(10, 54), expand = FALSE)


# Final map
ggplot(data = world) +
  geom_sf(fill= "antiquewhite") + 
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkblue", check_overlap = FALSE, fontface = "bold", size=3) +
  annotate(geom = "text", x = 114, y = 16, label = "South China Sea", 
           fontface = "italic", color = "grey22", size = 4) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(73, 136), ylim = c(10, 54), expand = FALSE)+ 
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("Map of China and South China Sea") + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "aliceblue"))

# Saving the map with ggsave
ggsave("china_map.pdf")
ggsave("china_map.png", width = 6, height = 6, dpi = "screen")


##############
# Add layers #
##############

# Field sites (point data)
library(readstata13)

# import the location data
coronavirus  <- read.dta13("coronavirus_china_data.dta", encoding="UTF-8")
sites <- subset(coronavirus, date=="2020-01-23" & !is.na(coronavirus$x))

# add location layers
ggplot(data = world) +
  geom_sf() +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkblue", check_overlap = FALSE, fontface = "bold", size=3) +
  geom_point(data=sites, aes(x=x, y=y), size=2) +
  coord_sf(xlim = c(73, 136), ylim = c(10, 54), expand = FALSE)


ggplot(data = world) +
  geom_sf(fill="black", alpha=0.6) +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkblue", check_overlap = FALSE, fontface = "bold", size=3) +
  geom_point(data=sites, aes(x=x, y=y, size=confirmed), color="red", alpha=0.3) +
  coord_sf(xlim = c(73, 136), ylim = c(10, 54), expand = FALSE)


#######################################
# Import a shapefile + Attribute Join #
#######################################

# Import a shapefiles
unzip("city2007.zip")

chinacity <- st_read("city2007/city_region.shp")
chinacity

ggplot() +
  geom_sf(data=chinacity) +
  ggtitle("China Prefecture Map") +
  coord_sf()

# suppose we want to plot the cumulative confirmed cases from 2019-12-01 to 2020-02-25, then first we have to caclulate the cumulated value by aggregating the new confirmed cases each day
unique(coronavirus$date)

coronavirus_city <- subset(coronavirus, !is.na(coronavirus$cityid))

# aggregate confirmed cases by city id
library(dplyr)
coronavirus_cumu <-
coronavirus_city %>%
  group_by(cityid) %>%
  summarise(confirmed = sum(confirmed, na.rm = T))

# another way: generate a cumulative sum
library(lubridate)
coronavirus_city$date <- ymd(coronavirus_city$date)

coronavirus_city <-
  coronavirus_city %>%
  arrange(cityid, date)

coronavirus_city <-
  coronavirus_city %>%
  arrange(date, cityid)

coronavirus_city <-
  coronavirus_city %>%
  group_by(cityid) %>%
  mutate(cumu_confimred=cumsum(confirmed))

# Can we then subset the data at date=2020-02-25 (the last day) to get the cumulateive aggregate?

# May not! If the data only have observation when there is updated number for one particular date ---> An issue of unbalanced panel 

# To get a balanced panel data

#install.packages("plm")
library(plm)
coronavirus_city <- pdata.frame(coronavirus_city, index = c("cityid", "date"), drop.index = F)
pdim(coronavirus_city)$balanced
coronavirus_bal <- make.pbalanced(coronavirus_city)

# it does not work! another way?
library(tidyr)
w_coronavirus_city <- 
  coronavirus_city %>% 
  spread(key=date, value=confirmed)

coronavirus_city <- 
  w_coronavirus_city %>% 
  gather(`2019-12-01`: `2020-02-25`, key = "date", value = "confirmed")

# remove duplicates cases
coronavirus_city <- distinct(coronavirus_city, cityid, date, .keep_all = T)

# generate the cumulative aggregate again!
coronavirus_city <-
  coronavirus_city %>%
  group_by(cityid) %>%
  mutate(cumu_confimred=cumsum(confirmed))

# join a data frame to a spatial data frame by an attribute 
#install.packages("tigris")
library(tigris)
chinacity2 <- geo_join(chinacity, coronavirus_cumu, 'CNTY_CODE', 'cityid')

# make a map!
ggplot() +
  geom_sf(data=subset(chinacity2, !is.na(confirmed) & confirmed!=0), aes(fill=confirmed), color="white", size=0.1) +
  geom_sf(data=subset(chinacity2, is.na(confirmed) | confirmed==0), fill="gainsboro", color="white", size=0.1) +
  scale_fill_gradient(trans = "log10") + 
  ggtitle("China Cumulative Confirmed Cases, by 25 Feb 2020") +
  coord_sf()

# If we wanted to plot our map as a ‘true??? choropleth map we need to convert our continouse variable into a categoriacal one, according to whichever brackets we want to use
# This requires two steps:
  ## Determine the quantile breaks;
  ## Add a categorical variable to the object which assigns each continious vaule to a bracket
library(dplyr)
library(classInt)
breaks_qt <- classIntervals(c(min(chinacity2$confirmed) - .00001, chinacity2$confirmed), n = 7, style = "quantile")
breaks_qt

# We use cut to confirmed case variable into intervals and code them according to which interval they are in
# Lastly, we can use scale_fill_brewer() and add our color palette
chinacity2 <- 
  mutate(chinacity2, confirmed_cat = cut(confirmed, breaks_qt$brks)) 

ggplot(chinacity2) + 
  geom_sf(aes(fill=confirmed_cat)) +
  scale_fill_brewer(palette = "OrRd") +
  ggtitle("China Cumulative Confirmed Cases, by 25 Feb 2020") +
  coord_sf()


##############################
# Adding basemaps with ggmap #
##############################

install.packages('bitops')
install.packages("ggmap")
library(ggmap)

# An simple example! Let us find hkbu first! 
hkbu <- data.frame(longitude = c(114.18, 114.20), latitude = c(22.35, 22.33))
hkbu

hkbubox <-make_bbox(lon=hkbu$longitude, lat=hkbu$latitude, f = .10)

# get a google api to download the basemap from google
register_google(key = "AIzaSyDsh0yKm-aUByFWfSdEgGGkqIUrZMBL7BU")

hkbu_map <- get_map(location = hkbubox, maptype = "terrain", source = "google")
ggmap(hkbu_map)


china <- data.frame(longitude = c(75, 135), latitude = c(20, 54))
chinabox <-make_bbox(lon=china$longitude, lat=hkbu$latitude, f = .10)
china_map <- get_map(location = chinabox, maptype = "terrain", source = "osm")

ggplot(china) + 
  geom_sf(data=chinacity2, aes(fill=confirmed_cat)) +
  scale_fill_brewer(palette = "OrRd") +
  ggtitle("China Cumulative Confirmed Cases, by 25 Feb 2020") +
  coord_sf()

# get road map from open street project 
install.packages("osmdata")
library(osmdata)

kowloon <- opq(bbox =  c(114.16, 22.35, 114.22, 22.30)) %>% 
  add_osm_feature(key = 'highway') %>% 
  osmdata_sf() %>% 
  osm_poly2line()

kowloon_center <- kowloon$osm_lines %>% 
  select(highway)

kowloon_center

ggplot(data = kowloon_center) + geom_sf()






############################
# 2.4 Choropleth with tmap #
############################


library(tmap)

# load in the crime rate data
philly_crimes_sf <- st_read("PhillyCrimerate/PhillyCrimerate.shp")
philly_crimes_sf

# get a base map
ph_basemap <- get_map(location=c(lon = -75.16522, lat = 39.95258), zoom=11, maptype = 'terrain-background', source = 'stamen')
ggmap(ph_basemap)

tm_shape(philly_crimes_sf) +
  tm_polygons("homic_rate", 
              style="quantile", 
              title="Philadelphia \nhomicide density \nper sqKm")

tmap_mode("view")
tmap_last()

# The tmap library also includes functions for simple spatial operations, geocoding and reverse geocoding using OSM. For more check vignette("tmap-getstarted")

################################
# 2.5 Web mapping with leaflet #
################################

# leaflet provides bindings to the ‘Leaflet??? JavaScript library, “the leading open-source JavaScript library for mobile-friendly interactive maps???

library(leaflet) 

# reproject
philly_WGS84 <- st_transform(philly_crimes_sf, 4326)

leaflet(philly_WGS84) %>%
  addPolygons()

# set a fillColor for each polygon based on homic_rate and make it look nice by adjusting fillOpacity and smoothFactor (how much to simplify the polyline on each zoom level)
pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)

# add a popup with the homic_rate values. We will create as a vector of strings, that we then supply to addPolygons()
p_popup <- paste0("<strong>Homicide Density: </strong>", philly_WGS84$homic_rate)

leaflet(philly_WGS84) %>%
  addPolygons(
    stroke = FALSE, # remove polygon borders
    fillColor = ~pal_fun(homic_rate), # set fill color with function from above and value
    fillOpacity = 0.8, smoothFactor = 0.5, # make it nicer
    popup = p_popup)  # add popup


# Add a basemap, which defaults to OSM, with addTiles()
leaflet(philly_WGS84) %>%
  addPolygons(
    stroke = FALSE, 
    fillColor = ~pal_fun(homic_rate),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = p_popup) %>%
  addTiles()

# Add a legend using addLegend()

leaflet(philly_WGS84) %>%
  addPolygons(
    stroke = FALSE, 
    fillColor = ~pal_fun(homic_rate),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = p_popup) %>%
  addTiles() %>%
  addLegend("bottomright",  # location
            pal=pal_fun,    # palette function
            values=~homic_rate,  # value to be passed to palette function
            title = 'Philadelphia homicide density per sqkm') # legend title

# Set the labels for our breaks manually 
## get quantile breaks. Add .00001 offset to catch the lowest value
breaks_qt <- classIntervals(c(min(philly_crimes_sf$homic_rate) - .00001, philly_crimes_sf$homic_rate), n = 5, style = "quantile")
breaks_qt

library(RColorBrewer)

leaflet(philly_WGS84) %>%
  addPolygons(
    stroke = FALSE, 
    fillColor = ~pal_fun(homic_rate),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = p_popup) %>%
  addTiles() %>%
  addLegend("bottomright", 
            colors = brewer.pal(5, "YlOrRd"), 
            labels = paste0("up to ", format(breaks_qt$brks[-1], digits = 2)),
            title =  'Philadelphia homicide density per sqkm')


# Add a control to switch to another basemap from "Carto", and option to turn the polygon off and on

leaflet(philly_WGS84) %>%
  addPolygons(
    stroke = FALSE, 
    fillColor = ~pal_fun(homic_rate),
    fillOpacity = 0.8, smoothFactor = 0.5,
    popup = p_popup,
    group = "philly") %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("CartoDB.DarkMatter", group = "Carto") %>%
  addLegend("bottomright", 
            colors = brewer.pal(5, "YlOrRd"), 
            labels = paste0("up to ", format(breaks_qt$brks[-1], digits = 2)),
            title = 'Philadelphia homicide density per sqkm') %>%
  addLayersControl(baseGroups = c("OSM", "Carto"), 
                   overlayGroups = c("philly"))  


install.packages("raster")
install.packages("pacman")
library(raster)
library(pacman)
# Load new packages
pacman::p_load(rgeos, GISTools)
# Load old packages
p_load(lubridate, rgdal, raster, broom, rgeos, GISTools)
p_load(dplyr, sp, ggplot2, ggthemes, fields, magrittr, viridis, ggmap)
# My ggplot2 theme
theme_ed <- theme(
  legend.position = "bottom",
  panel.background = element_rect(fill = NA),
  # panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey95", size = 0.3),
  panel.grid.major = element_line(color = "grey95", size = 0.3),
  panel.grid.minor = element_line(color = "grey95", size = 0.3),
  legend.key = element_blank())

# The directory stored the nightlight data
dir_nl <- "C:/Users/steff/Dropbox/Shenzhen U/Big Data & Finance/L4 Data"

########################
# 3.1 read raster data #
########################

list.files(path=dir_nl, pattern ="*.tif")
nl_gz_201912  <- raster("nl_gz_1912_v3.tif")
gz_t <- readOGR(dsn=dir_nl,layer="gz")
sz_t <- readOGR(dsn=dir_nl,layer="sz_t")

###################################
# 3.2 plot and summarize a raster #
###################################

plot(nl_gz_201912)

library(viridisLite)
library(viridis)
plot(nl_gz_201912, col = viridis(1e3), axes = F, box = F)
lines(gz_t, col = "white", lwd = 0.8)

# R's description of the raster
nl_gz_201912
# R's summary of the raster
nl_gz_201912 %>% summary()
# The slot names
nl_gz_201912 %>% slotNames()
# The first 6 values
nl_gz_201912 %>% getValues() %>% head()
# The first six coordinates
nl_gz_201912 %>% coordinates() %>% head()

####################################
# 3.3 reclassify value in a raster #
####################################

## A histogram of the raster’s values (logged):
ggplot(data = data.frame(light = getValues(nl_gz_201912)),
       aes(x = log(light))) +
  geom_histogram(bins = 20, fill = viridis(20)) +
  ylab("Count") +
  xlab("log(radiance)") +
  ggtitle("Distribution of light radiance near Guangzhou, China",
          subtitle = "December 2016, logged radiance") +
  theme_bw()

ggplot(data = data.frame(light = getValues(nl_gz_201912)),
       aes(x = light)) +
  geom_histogram(bins = 20, fill = viridis(20)) +
  ylab("Count") +
  xlab("log(radiance)") +
  ggtitle("Distribution of light radiance near Guangzhou, China",
          subtitle = "December 2016, logged radiance") +
  theme_bw()

## reclassify value in a raster 
bl <- c(-0.13, 0:100)
bh <- c(0:100, 600)
nv <- c(0:100, 100)
m <- cbind(bl, bh, nv)
nl_gz_201912_re <- reclassify(nl_gz_201912, m)

plot(nl_gz_201912_re, col = viridis(1e3), axes = F, box = F)
lines(gz_t, col = "white", lwd = 0.8)

##########################
# 3.4 Rasters in ggplot2 #
##########################

## Convert raster to matrix and then to data frame
nl_gz_201912_df <- nl_gz_201912_re %>% rasterToPoints() %>% tbl_df()
## Plot with ggplot
ggplot(data = nl_gz_201912_df,
       aes(x = x, y = y, fill = nl_gz_1912_v3)) +
  geom_raster() +
  ylab("") + xlab("") +
  ggtitle("Lights at night in Guangzhou",
          subtitle = "December 2019") +
  theme_bw()

#############
# 3.5 Masks #
#############

library(rgeos)
# Take the union of the shenzhen block polygons
sz_union <- gUnaryUnion(sz_t)
plot(sz_union)

# Mask night lights raster with Shenzhen's outline
lights_masked <- mask(x = nl_gz_201912_re, mask = sz_union)

# Convert raster to matrix and then to data frame
masked_df <- lights_masked %>% rasterToPoints() %>% tbl_df()
# Plot with ggplot
ggplot(data = masked_df,
       aes(x = x, y = y, fill = nl_gz_1912_v3)) +
  geom_raster() +
  ylab("") + xlab("") +
  ggtitle("Lights at night in Shenzhen, masked",
          subtitle = "December 2019") +
  scale_fill_viridis(option = "D") +
  theme_bw()
#!  Try it yourself using the guangzhou block shapefile 

