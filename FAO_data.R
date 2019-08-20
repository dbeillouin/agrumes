Agrumes <- read.csv("~/Documents/agrumes_CIRAD/Agrumes_database.csv", sep=";") %>%
  dplyr::select("Area","Item","Year","Element", "Value")%>% 
  spread(Element, Value)


### MAP
library(tidyverse)
library(magrittr)
library(dplyr)
library(eurostat)
library(sf)
## Linking to GEOS 3.6.2, GDAL 2.2.3, PROJ 4.9.3
library(tmap)


# Download geospatial data from GISCO
geodata <- get_eurostat_geospatial(output_class = "sf",
                                   resolution = "60",
                                   nuts_level = 3,
                                   year = 2013)


geodata$NUTS_NAME<-tolower(geodata$NUTS_NAME)
geodata$NUTS_NAME<-gsub("(.*),.*", "\\1", geodata$NUTS_NAME)
geodata$NUTS_NAME<- gsub('\\ ', '_', geodata$NUTS_NAME)
geodata$NUTS_NAME<- gsub('\\-', '_', geodata$NUTS_NAME)
RES2$NUTS_NAME<-tolower(RES2$NUTS_NAME)

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}
geodata$NUTS_NAME<-Unaccent(geodata$NUTS_NAME)
geodata$NUTS_NAME <- gsub("-", "_", geodata$NUTS_NAME)
geodata$NUTS_NAME <- gsub(" ", "_", geodata$NUTS_NAME)


# on supprime les îles
geodata %<>%   filter(!grepl("guyane",NUTS_NAME))
geodata %<>%   filter(!grepl("reunion",NUTS_NAME))
geodata %<>%   filter(!grepl("martinique",NUTS_NAME))
geodata %<>%   filter(!grepl("guadeloupe",NUTS_NAME))
geodata %<>%   filter(!grepl("mayotte",NUTS_NAME))
geodata %<>%   filter(!grepl("acore",NUTS_NAME))
geodata %<>%   filter(!grepl("canar",NUTS_NAME))
geodata %<>%   filter(!grepl("la_palma",NUTS_NAME))
geodata %<>%   filter(!grepl("lanzarote",NUTS_NAME))
geodata %<>%   filter(!grepl("tenerife",NUTS_NAME))
geodata %<>%   filter(!grepl("la_gomera",NUTS_NAME))
geodata %<>%   filter(!grepl("gran_canaria",NUTS_NAME))
geodata %<>%   filter(!grepl("fuerteventura",NUTS_NAME))
geodata %<>%   filter(!grepl("el_hierro",NUTS_NAME))




########
library(tmap)
data("World")
world_tmap <- st_as_sf(World)
qtm(world_tmap)



######
RES2<-Agrumes  %>% filter(Year == "2017", Item %in% c(
"Tangerines, mandarins, clementines, satsumas"))
"Apples",
"Olives",
"Oranges"))

unique(Agrumes$Item)


names(RES2)[1]<- "name"
RES2$name<-tolower(RES2$name)
world_tmap$name<-tolower(world_tmap$name)

DATA <- merge(world_tmap , RES2, by="name")

DATA$Yield<- as.numeric(DATA$Yield)
my_interactive_map <- tm_shape(DATA) +
  tm_polygons("Yield", id = "geo", palette="RdYlGn", n=8, title="Yield",legend.outside = FALSE)
 # tm_facets(by = "Item",free.scales= TRUE)
tm_facets(by = "Item",as.layers = TRUE)

, free.coords = FALSE, drop.units = TRUE,free.scales= TRUE)

  tmap_options(limits = c(facets.view = 4))+
  tm_layout(legend.outside = TRUE)
my_interactive_map <- tmap_leaflet(my_interactive_map)
my_interactive_map <- my_interactive_map
my_interactive_map
