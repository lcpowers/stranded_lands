## Cropping Marxan input layers to speed up Marxan Parameter tuning

cropped_pus = read_sf("./R_input_files/PUs/custer_croppedforMarxan.shp") %>% 
  as.data.frame() %>% 
  dplyr::select(id)

custer_puvspr = read.table("./R_output_files/MarxanInput_byCounty/CUSTER/puvspr2.dat", header = T) %>% 
  filter(pu %in% cropped_pus$id)

write_delim(custer_puvspr, path = paste0("./R_output_files/MarxanInput_byCounty/CUSTER/puvspr_crop.dat"), delim =",")

custer_pus = read.table("./R_output_files/MarxanInput_byCounty/CUSTER/pu.dat",header = T) %>% 
  filter(id %in% cropped_pus$id)

sum(custer_pus$cost[custer_pus$status == 1] + custer_pus$cost[custer_pus$status == 2])

custer_pus$status[custer_pus$status == 1] = 0
table(custer_pus$status)

write_delim(custer_pus, path = paste0("./R_output_files/MarxanInput_byCounty/CUSTER/pu_crop.dat"), delim =",")

custer_bound = read.table("./R_output_files/MarxanInput_byCounty/CUSTER/bound.dat",header = T) %>% 
  filter(id1 %in% cropped_pus$id & id2 %in% cropped_pus$id)

custer_bound_full = read.table("./R_output_files/MarxanInput_byCounty/CUSTER/bound.dat",header = T)
custer_bound_full$boundary = (1/custer_bound_full$boundary) * 1000

custer_bound$boundary = 1/custer_bound$boundary
custer_bound$boundary = custer_bound$boundary*1000

write_delim(custer_bound_full, path = paste0("./R_output_files/MarxanInput_byCounty/CUSTER/bound.dat"), delim =",")

max(custer_pus$cost)/max(custer_bound_final$boundary)
