
library(rgdal)
library(tidyverse)
library(sf)
library(raster)

nlcd = raster("../R_output_files/nlcd/nlcd90_mt.tif")

sections = read_sf("../R_output_files/PLSS_sections/PU_sections.shp") %>% 
  st_transform(.,crs(nlcd)) %>% 
  as(.,"Spatial")

counties = read_sf("../R_input_files/mt_counties/County.shp") %>% 
  dplyr::select(NAME) %>% 
  st_transform(.,crs(nlcd)) 

county_names = as.list(counties$NAME)

PU_crop_fun = function(county_name){
  
  print(county_name)
  
  dir.create(paste0("../R_output_files/PLSS_sections/",county_name))
  
  county_i = counties %>%
    filter(NAME == county_name) %>%
    as(.,"Spatial")
  
  county_i_PUs = raster::intersect(sections,county_i)
  
  writeOGR(obj = county_i_PUs,
           dsn = paste0("../R_output_files/PLSS_sections/",county_name),
           layer = paste0("pu_",county_name),
           driver = "ESRI Shapefile",
           overwrite_layer = T)
  
}

lapply(X = county_names, FUN = PU_crop_fun)
