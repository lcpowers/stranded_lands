library(raster)
library(sf)
library(tidyverse)

mt_nlcd <- raster("./R_output_files/nlcd/mt_nlcd_270.tif")

mt_corridors <- raster("./R_input_files/circuitscape/AllMontana_270_100k_corridors_truncated_at_100k1.tif")
crs(mt_corridors) <- crs(mt_nlcd)

plot(mt_corridors)

corr25 = ncell(mt_corridors[mt_corridors<=25000])
corr50 = ncell(mt_corridors[mt_corridors>25000&mt_corridors<=50000])+corr25
percinc50 = (corr50 - corr25)/corr25
totinc50 = corr50 - corr25

corr75 = ncell(mt_corridors[mt_corridors>50000&mt_corridors<=75000])+corr50
percinc75 = (corr75 - corr50)/corr50
totinc75 = corr75 - corr50

corr100 = ncell(mt_corridors[mt_corridors>75000&mt_corridors<=100000])+corr75
percinc100 = (corr100 - corr75)/corr75
totinc100 = corr100 - corr75


mt_counties <- read_sf("./R_input_files/mt_counties/County.shp") %>% 
  dplyr::select(NAME) %>% 
  st_transform(.,crs(mt_nlcd))

avg_cwd_df = data.frame(county = mt_counties$NAME,
                        avg_cwd = rep(NA,length(mt_counties$NAME)),
                        coverage = rep(NA,length(mt_counties$NAME)))

corr_crop_fun = function(county_name){
  
  print(county_name)
  county_outline = mt_counties %>% 
    filter(NAME == county_name)

  tmp_corr_raster = mask(crop(mt_corridors,county_outline),county_outline)
  
  avg_cwd = round(cellStats(tmp_corr_raster,mean, na.rm = T),4)
  avg_cwd_df$avg_cwd[avg_cwd_df$county == county_name] <<- avg_cwd
  
  coverage = ncell(tmp_corr_raster[!is.na(tmp_corr_raster)])/ncell(tmp_corr_raster)*100
  avg_cwd_df$coverage[avg_cwd_df$county == county_name] <<- coverage
  
}

lapply(mt_counties$NAME, corr_crop_fun)

avg_cwd_quants = avg_cwd_df %>% 
  mutate(quantile = ntile(avg_cwd,4))

coverage_quant_df = avg_cwd_df %>% 
  mutate(quantile = ntile(coverage,4))

# 1 = 
# 2 = 
# 3 = 
# 4 = 

# Create empty raster
ras = raster()
ras = setExtent(ras,mt_corridors)
res(ras) = res(mt_corridors)

coverage_crop_fun = function(county_name){
  
  ## Get county outline
  print(county_name)
  county_outline = mt_counties %>% 
    filter(NAME == county_name)
  
  ## Crop/mask MT corridor raster to county outline
  tmp_corr_raster = mask(crop(mt_corridors,county_outline),county_outline)
  
  ## Retrieve what quantile the current county is in
  quant = as.numeric(avg_cwd_quants$quantile[avg_cwd_quants$county == county_name])
  
  ## Convert raster values to NA depending on quantile
  if(quant == 1){
    
    ## Coverage is low keep 100k cwd corridors
    writeRaster(tmp_corr_raster,
                paste0("./R_output_files/scratch/avg_cwd_corridorcrops/",county_name,".tif"),
                overwrite = T)
    
  }
  
  if(quant == 2){
    
    ## Use 75k cwd corridors -- convert any > 75000 to NA
    tmp_corr_raster[tmp_corr_raster>75000] <- NA
    writeRaster(tmp_corr_raster, 
                paste0("./R_output_files/scratch/avg_cwd_corridorcrops/",county_name,".tif"),
                overwrite = T)
    
  }
  
  if(quant == 3){
    
    ## Use 50k cwd corridors -- convert any > 50,000 to NA
    tmp_corr_raster[tmp_corr_raster>50000] <- NA
    writeRaster(tmp_corr_raster,
                paste0("./R_output_files/scratch/avg_cwd_corridorcrops/",county_name,".tif"),
                overwrite = T)
    
  }
  
  if(quant == 4){
    
    ## Use 25k cwd corridors -- convert any > 25000 to NA
    tmp_corr_raster[tmp_corr_raster>25000] <- NA
    writeRaster(tmp_corr_raster,
                paste0("./R_output_files/scratch/avg_cwd_corridorcrops/",county_name,".tif"),
                overwrite = T)
    
  }
  
}

lapply(mt_counties$NAME, coverage_crop_fun)

timestamp()
corr_list <- list.files(path = "./R_output_files/scratch/avg_cwd_corridorcrops/",recursive = T,full.names = T)
corr_rasters <- lapply(corr_list, raster)
corrs_merged <- do.call(merge,corr_rasters) 

writeRaster(corrs_merged,"./R_output_files/scratch/avg_cwd_corridorcrops/MONTANA.tif")
  