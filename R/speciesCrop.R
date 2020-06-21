#### Need to add notes to this file!!##

library(raster)
library(tidyverse)
library(sf)
library(rgeos)

nlcd_30 = raster("./R_input_files/nlcd/NLCD_2016_Land_Cover_L48_20190424.img")

mt_outline = read_sf("./R_input_files/mt_outline/StateofMontana.shp") %>% 
  dplyr::select(geometry) %>% 
  st_transform(.,crs(nlcd_30))

mt_nlcd_30 = mask(crop(nlcd_30,mt_outline),mt_outline)

species_files = list.files("./R_input_files/species", pattern = ".tif", full.names = T)

# This loop significantly speeds up the next loop in which we crop each species' raster to the outline of each county
for(i in species_files){
  
  timestamp() # To track and anticipate time to process
  tmp_name = str_extract(string = i,pattern = "(?<=./R_input_files/species/).*(?=.tif)") # Get name of current species being process
  print(tmp_name) # Print species name to track progress
  tmp = raster(i) # Read it in as a raster
  tmp_crop = crop(tmp,extent(mt_outline)) # Crop raster to Montana Outline
  # tmp_agg = aggregate(tmp_crop, fact = 9, fun = max, na.rm = T, expand = T) # Increase resolution to 270x270
  tmp_mask = mask(tmp_crop,mt_outline) # Mask the raster to Montana Outline
  assign(tmp_name,tmp_mask)
  writeRaster(eval(as.name(tmp_name)),paste0("./R_output_files/species/montana/",tmp_name,".tif"))
  remove(tmp,tmp_name,tmp_mask)
  
}


mt_counties = read_sf("./R_input_files/mt_counties/County.shp") %>% 
  st_transform(.,crs(mt_nlcd_30)) %>% 
  dplyr::select(NAME) 

county_names = mt_counties$NAME

species_names =  str_extract(string = species_files,
                             pattern = "(?<=./R_input_files/species/).*(?=.tif)")

for(i in county_names){
  
  timestamp()
  print(i)
  dir.create(paste0("./R_output_files/species/",i,"/"))
  county_outline = filter(mt_counties, NAME == i) %>% as(.,"Spatial")
  county_ext = extent(county_outline)
  nlcd_mask = mask(crop(mt_nlcd_30,county_ext),county_outline)
  
  for(j in species_names){
    
   # print(j)
    ras_extent = as(extent(eval(as.name(j))),"SpatialPolygons") # Make a polygon of the extent of the current species' raster 
    crs(ras_extent) = crs(mt_nlcd_30)
    
   if(gIntersects(ras_extent,county_outline)){ # If the species extent polygon overlaps with the current county outline...
      species_crop = crop(eval(as.name(j)),county_ext) # Crop the species raster to the county outline
      species_mask = mask(species_crop,county_outline) # then mask it to the county outline
      species_rsmpl = resample(species_mask,nlcd_mask)
      species_rsmpl[species_rsmpl > 0] = 1 # Change the default value 3 to 1
      
      if(cellStats(species_rsmpl,sum) > 0){ # If there are presence cells in that raster (i.e. a small corner of the extent box without any actual species presence wasn't what overlapped)...
        
        # Write the output raster
        writeRaster(species_rsmpl, paste0("./R_output_files/species/",i,"/",j,"_30.tif"), overwrite = T)
        
      }
      # Remove those files to make sure the don't interfere with the next loop iteration. Unnecessary, but good for peace of mind
      remove(ras_extent,species_crop,species_mask,species_rsmpl)
    }
  }
}


