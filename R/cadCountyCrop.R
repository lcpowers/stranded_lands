
library(rgdal)
library(tidyverse)
library(sf)
library(raster)

nlcd = raster("../R_output_files/nlcd/nlcd90_mt.tif")
cad = read_sf("../R_input_files/Cadastral20191231/mt_cadastral_clean.shp") %>% 
  filter(!is.na(TotalValue)) %>% # 937252 -> 900910
  dplyr::select(OBJECTID,PARCELID,PropertyID,LegalDescr,PropType,TotalAcres,TotalLandV,TotalValue,CountyName) %>% 
  st_transform(.,crs(nlcd)) %>% 
  st_simplify()

county_names = as.list(unique(cad$CountyName))

cad_crop = function(county_name){
  
  print(county_name)
  
  # dir.create(paste0("../R_output_files/cadastral/",county_name))
  
  county_i_cad = cad %>%
    filter(CountyName == county_name) %>%
    as(.,"Spatial")
  
  writeOGR(obj = county_i_cad,
           dsn = paste0("../R_output_files/cadastral/",county_name),
           layer = paste0("cadastral_",county_name),
           driver = "ESRI Shapefile",
           overwrite_layer = T)
  
  
  
}

lapply(X = county_names, FUN = cad_crop)
