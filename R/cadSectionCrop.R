
library(rgdal)
library(tidyverse)
library(sf)
library(raster)

# 1. Read in county cadastral data and calculate area
# 2. Read in county County Sections
# 3. Get list of Custer County section PU IDs
# 4. Crop Cad to PU and calculate area_B of each polygon
# 5. Store in List?
# 6. Rbind each element of list with forloop or apply?

# County Names List
counties = read_sf("../R_input_files/mt_counties/County.shp") %>% dplyr::select(NAME)
county_names = counties$NAME


# Function that crops cadastral data to the outline of each section
cad_PU_crop_fun = function(PUid_i){
  
  # Show current PUid index for progress check
  print(PUids %>% {which(. == PUid_i)})
  
  # Filter PU Shapefile for current section
  PU_i = filter(county_i_PUs,pu_id == PUid_i) %>% as(.,"Spatial")
  
  # Crop Cadastral data to outline of PU
  cad_clip = raster::intersect(county_i_cad, PU_i)
  
  if(!is.null(cad_clip))
    {
    
    cad_clip = st_as_sf(cad_clip)
    
    # Create another column with the new area of each polygon (post-clip area)
    cad_clip$area_B = as.numeric(st_area(cad_clip))
  
    # Place in output list
    cadPUcrop_list[[as.character(PUid_i)]] <- cad_clip
   }
  
}

for(county_name in county_names){

  print(county_name)
  timestamp()
  
  # Read in cadastral data for County i
  county_i_cad = read_sf(paste0("../R_output_files/cadastral/",county_name,"/cadastral_",county_name,".shp")) %>% 
    mutate(area_A = as.numeric(st_area(.))) %>% 
    as(.,"Spatial")
  
  # Read in Planning Units (PLSS Sections) for county i
  county_i_PUs = read_sf(paste0("../R_output_files/PLSS_sections/",county_name,"/pu_",county_name,".shp"))
  
  # Get vector of Planning Unit IDs for county i
  PUids = county_i_PUs$pu_id
  
  # Initialize list where the output of the following loop will be stored
  cadPUcrop_list = vector(mode = "list",length = length(PUids))
  names(cadPUcrop_list) = PUids
  
  # Apply the function to the current county
  cadPUcrop_list = lapply(PUids, cad_PU_crop_fun)
  
  # Initialize a data.frame to be able to bind all output together
  tmp = cadPUcrop_list[[1]]
  
  # Bind all elements of the function output list 
  for(i in 2:length(cadPUcrop_list)){
    
    # Get list element i
    tmp_b = cadPUcrop_list[[i]]
    
    # Row bind list element i with the compiling output dataframe
    tmp = rbind(tmp,tmp_b)
  
    }
  
  tmp_storage = tmp
  # tmp = tmp_storage
  
  # Find the proportion of the total area of each cadastral polygon that overlaps with section polygons. For example, if cadatral polygon overlaps with both section A and section B, what proportion of cadastral polygon 1 overlaps with A and what propotion overlaps with B
  tmp$TV_prop = (tmp$area_B/tmp$area_A)
  
  # Keep all individual parcels that are at least 5% the size of the original cadastral parcel before the crop
  tmp_fltr = tmp %>% 
    filter(TV_prop > 0.0500)
  
  # Make the counter parts of the parcels that were removed in the previous step == 1, to capture the full value of the original parcel
  tmp_fltr$TV_prop[tmp_fltr$TV_prop>=0.9500] <- 1 
  
  # Find the proportional value of each parcel
  tmp_fltr$Final_TV = round(tmp_fltr$TotalValue*tmp_fltr$TV_prop)
  
  # Compare the sum of parcel values in the final df to the original DF
  TV_difference = sum(county_i_cad$TotalValue) - sum(tmp_fltr$Final_TV)
  
  writeOGR(obj = as(tmp_fltr,"Spatial"),
           dsn = paste0("../R_output_files/cadastral_sectionAdjust/",county_name),
           layer = paste0(county_name,"_cadSectionAdj"),
           driver = "ESRI Shapefile",
           overwrite_layer = T)
  
  timestamp()
  
  }
