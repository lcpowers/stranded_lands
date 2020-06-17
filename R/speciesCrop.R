#### Need to add notes to this file!!##

library(raster)
library(tidyverse)
library(sf)
library(rgeos)

mt_nlcd_270 = raster("./R_output_files/nlcd/mt_nlcd_270.tif")
mt_outline = read_sf("./R_input_files/mt_outline/StateofMontana.shp") %>% 
  dplyr::select(geometry) %>% 
  st_transform(.,crs(mt_nlcd_270))

species_files = list.files("./R_input_files/species", pattern = ".tif", full.names = T)

for(i in species_files){
  
  timestamp()
  tmp_name = str_extract(string = i,pattern = "(?<=./R_input_files/species/).*(?=.tif)")
  print(tmp_name)
  tmp = raster(i)
  crs(tmp) = crs(mt_nlcd_270)
  tmp_crop = crop(tmp,extent(mt_outline))
  tmp_mask = mask(tmp_crop,mt_outline)
  tmp_rsmpl = resample(tmp_mask,mt_nlcd_270,"ngb")
  assign(tmp_name,tmp_rsmpl)
  remove(tmp,tmp_name,tmp_rsmpl)
  
}


mt_counties = read_sf("../R_input_files/mt_counties/County.shp") %>% 
  st_transform(.,crs(nlcd)) %>% 
  dplyr::select(NAME) 

county_names = mt_counties@data$NAME

species_names =  str_extract(string = species_files,
                             pattern = "(?<=../R_input_files/species/).*(?=.tif)")

for(i in county_names){
  
  print(i)
  dir.create(paste0("../R_output_files/species/",i,"/"))
  county_outline = filter(mt_counties, NAME == i) %>% as(.,"Spatial")
  
  for(j in species_names){
    
    print(j)
    ras_extent = as(extent(eval(as.name(j))),"SpatialPolygons") # Make a polygon of the extent of the current species' raster
    
    if(gIntersects(ras_extent,county_outline)){ # If the species extent polygon overlaps with the current county outline...
      species_crop = crop(eval(as.name(j)),county_outline) # Crop the species raster to the county outline
      species_mask = mask(species_crop,county_outline) # then mask it to the county outline
      species_mask[species_mask == 3] = 1 # Change the default 3 value to 1
      
      if(cellStats(species_mask,sum) > 0){ # If there are presence cells in that raster (i.e. a small corner of the extent box without and actual species presence wasn't what overlapped)...
        
        # Write the output raster
        writeRaster(species_mask, paste0("../R_output_files/species/",i,"/",j,".tif"), overwrite = T)
      
      }
      # Remove those files to make sure the don't interfere with the next loop iteration. Unnecessary, but good for peace of mind
      remove(ras_extent,species_crop,species_mask)
    }
  }
}


