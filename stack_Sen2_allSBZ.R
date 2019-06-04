library(raster)
library(sp)
library(rgdal)
library(stringr)
library(RStoolbox)
  
# Get shapefile of SBZs in UTM31N
file_SBZ <- "../inbo-downloadsubimages_python/inputs/shp_allSBZ/SBZ-Hdeel_diss_UES_UFS_UTM31N.shp"
shp_SBZ <- readOGR(file_SBZ)

# Get directories of EUS and EFS to calculate overlap with SBZ subarea later
dirs <- list.dirs("Q:/Projects/PRJ_RemSen/Change detection 2018/Sentinel 2 images/begin May/")
dirs_UES <- dirs[ grepl("UES", dirs)]
dirs_UFS <- dirs[ grepl("UFS", dirs)]

head_dir <- "Q:/Projects/PRJ_RemSen/Change detection 2018/change-detection files/data/Sen2_data/"
sub_dir <- "begin May/"
total_dir <- paste0(head_dir, sub_dir)

#Loop over SBZs

for (n in 1:length(shp_SBZ)){
  # Keep name and code of SBZ (subarea)
  studysite <- shp_SBZ[n,]
  SBZ_code = shp_SBZ[n,]$DEELGEBIED
  SBZ_name = shp_SBZ[n,]$NAAM
  
  # Calculate spatial overlap between Sen2 tiles and SBZ subarea
  extent_overlap_UES <- intersect(extent(raster(list.files(dirs_UES[1], pattern = "10M.*tif$", full.names = TRUE, recursive = TRUE)[1])), shp_SBZ[n,])
  if (length(extent_overlap_UES)!=0){
    area_overlap_UES <- (extent_overlap_UES@xmax - extent_overlap_UES@xmin)*(extent_overlap_UES@ymax - extent_overlap_UES@ymin)
  } else {
    area_overlap_UES <- 0
  }
  extent_overlap_UFS <- intersect(extent(raster(list.files(dirs_UFS[1], pattern = "10M.*tif$", full.names = TRUE, recursive = TRUE)[1])), shp_SBZ[n,])
  if (length(extent_overlap_UFS)!=0){
    area_overlap_UFS <- (extent_overlap_UFS@xmax - extent_overlap_UFS@xmin)*(extent_overlap_UFS@ymax - extent_overlap_UFS@ymin)
  } else{
    area_overlap_UFS <- 0
  }
  # Determine the tile based on the amount of overlap
  if (area_overlap_UFS > area_overlap_UES){
    Sen2_tile <- "UFS"
  } else {
    Sen2_tile <- "UES"
  }

  # Create a folder for the outputs of it does not yet exist
  if (file.exists(file.path(total_dir, SBZ_code)) == FALSE){
    dir.create(file.path(total_dir, SBZ_code))
  }
  
  dirs <- list.dirs("Q:/Projects/PRJ_RemSen/Change detection 2018/Sentinel 2 images/begin May/")
  if (Sen2_tile == "UES"){
    dirs <- dirs[ grepl("UES", dirs)]
  } else {
    dirs <- dirs[ grepl("UFS", dirs)]
  }
  
  
  # dirs aanpassen zodat alleen de Sen2 beelden met minder dan 10% wolken over de studysite overblijven
  j = 0
  for (i in dirs){
    j = j+1
    cloud_mask_img <- list.files(i, "CLOUDMASK.*10M.*tif$", full.names = TRUE, recursive = TRUE)
    cloud_in <- raster(cloud_mask_img[1])
    crop_cloud <- crop(cloud_in, studysite)
    shape_rast <- rasterize(studysite, crop_cloud)   
    cloud_mask <- mask(x=crop_cloud, mask=shape_rast)
    cloud_perc <- freq(cloud_mask, digits=1, value=1)/(freq(cloud_mask, digits=1, value=1) + freq(cloud_mask, digits=1, value=0))
    if (cloud_perc > 0.1){
      dirs <- dirs[-j]
    }
  }
   
  print("final dirs")
  print(dirs)

  
  # Rasterstacks maken
  list_10m <- list()
  list_20m <- list()
  names_list <- list()
  for (i in dirs){
    date_loc <- str_locate(i, ".*S2._")[1,2]
    date_string <- str_sub(i, date_loc+1,date_loc+8)
    
    # 10m stack (make a function of this later)
    stack_name_10m <- paste(date_string,"Rasterstack_10m", SBZ_code, sep = "_")
    list_img_10m <- list.files(i, pattern = "10M.*tif$", full.names = TRUE, recursive = TRUE)
    stack_images_10m <- stack()
    
    cloud_mask_img <- list.files(i, "CLOUDMASK.*10M.*tif$", full.names = TRUE, recursive = TRUE)
    cloud_in <- raster(cloud_mask_img[1])
    crop_cloud <- crop(cloud_in, studysite)
    shape_rast <- rasterize(studysite, crop_cloud)   
    cloud_mask <- mask(x=crop_cloud, mask=shape_rast)
    start_loc <- str_locate(cloud_mask_img, "V10./")[1,2]
    end_loc <- str_locate(cloud_mask_img, "V10..tif")[1,2]
    name_cloud_out <- paste(total_dir, SBZ_code, "/", str_sub( cloud_mask_img,start_loc + 1,end_loc - 4) , "_", SBZ_code,".tif", sep = "")
    writeRaster(cloud_mask, name_cloud_out, overwrite=TRUE)
    
    shadow_mask_img <- list.files(i, "SHADOWMASK.*10M.*tif$", full.names = TRUE, recursive = TRUE)
    shadow_in <- raster(shadow_mask_img[1])
    crop_shadow <- crop(shadow_in, studysite)
    shadow_mask <- mask(x=crop_shadow, mask=shape_rast)
    start_loc <- str_locate(shadow_mask_img, "V10./")[1,2]
    end_loc <- str_locate(shadow_mask_img, "V10..tif")[1,2]
    name_mask_out <- paste(total_dir, SBZ_code, "/", str_sub(shadow_mask_img,start_loc + 1,end_loc - 4) , "_", SBZ_code,".tif", sep = "")
    writeRaster(shadow_mask,name_mask_out, overwrite=TRUE)
    
    for (j in c(3:length(list_img_10m))){
      sen2_in <- raster(list_img_10m[j])
      start_loc <- str_locate(list_img_10m[j], "V10./")[1,2]
      end_loc <- str_locate(list_img_10m[j], "V10..tif")[1,2]
      name_sen2_out <- paste(total_dir, SBZ_code, "/", str_sub(list_img_10m[j],start_loc + 1,end_loc - 4) , "_", SBZ_code,".tif", sep = "")
      crop_sen2 <- crop(sen2_in, studysite)
      studsite_mask <- rasterize(studysite, crop_sen2)   
      extr_sen2 <- mask(x=crop_sen2, mask=studsite_mask)
      extr_sen2 <- mask(x=extr_sen2, mask=crop_cloud, inverse = FALSE, maskvalue=1)
      extr_sen2 <- mask(x=extr_sen2, mask=crop_shadow, inverse = FALSE, maskvalue=1)
      writeRaster(extr_sen2/10000, name_sen2_out, overwrite=TRUE)
      stack_images_10m <- stack(stack_images_10m, extr_sen2/10000)
    }
    
    stack_images_10m <- stack(stack_images_10m, cloud_mask)
    stack_images_10m <- stack(stack_images_10m, shadow_mask)
    names(stack_images_10m) <- c("B02", "B03", "B04", "B08", "Cloud Mask", "Shadow Mask")
    assign(stack_name_10m, stack_images_10m)
    
    #20m stack maken (ook met resampled banden B02, B03 en B04)
    stack_name_20m <- paste(date_string,"Rasterstack_20m", SBZ_code, sep = "_")
    list_img_20m <- list.files(i, pattern = "20M.*tif$", full.names = TRUE, recursive = TRUE)
    stack_images_20m <- stack()
    
    cloud_mask_img <- list.files(i, "CLOUDMASK.*20M.*tif$", full.names = TRUE, recursive = TRUE)
    cloud_in <- raster(cloud_mask_img[1])
    crop_cloud <- crop(cloud_in, studysite)
    shape_rast <- rasterize(studysite, crop_cloud)   
    cloud_mask <- mask(x=crop_cloud, mask=shape_rast)
    start_loc <- str_locate(cloud_mask_img, "V10./")[1,2]
    end_loc <- str_locate(cloud_mask_img, "V10..tif")[1,2]
    name_cloud_out <-paste(total_dir, SBZ_code, "/", str_sub(cloud_mask_img,start_loc + 1,end_loc - 4) , "_", SBZ_code,".tif", sep = "")
    writeRaster(cloud_mask, name_cloud_out, overwrite=TRUE)
    
    shadow_mask_img <- list.files(i, "SHADOWMASK.*20M.*tif$", full.names = TRUE, recursive = TRUE)
    shadow_in <- raster(shadow_mask_img[1])
    crop_shadow <- crop(shadow_in, studysite)
    shadow_mask <- mask(x=crop_shadow, mask=shape_rast)
    start_loc <- str_locate(shadow_mask_img, "V10./")[1,2]
    end_loc <- str_locate(shadow_mask_img, "V10..tif")[1,2]
    name_mask_out <- paste(total_dir, SBZ_code, "/", str_sub(shadow_mask_img,start_loc + 1,end_loc - 4) , "_", SBZ_code,".tif", sep = "")
    writeRaster(shadow_mask,name_mask_out, overwrite=TRUE)
    
    for (j in c(4:length(list_img_20m))){
      sen2_in <- raster(list_img_20m[j])
      start_loc <- str_locate(list_img_20m[j], "V10./")[1,2]
      end_loc <- str_locate(list_img_20m[j], "V10..tif")[1,2]
      name_sen2_out <- paste(total_dir, SBZ_code, "/", str_sub(list_img_20m[j],start_loc + 1,end_loc - 4) , "_", SBZ_code,".tif", sep = "")
      crop_sen2 <- crop(sen2_in, studysite)
      studsite_mask <- rasterize(studysite, crop_sen2)   
      extr_sen2 <- mask(x=crop_sen2, mask=studsite_mask)
      extr_sen2 <- mask(x=extr_sen2, mask=crop_cloud, inverse = FALSE, maskvalue=1)
      extr_sen2 <- mask(x=extr_sen2, mask=crop_shadow, inverse = FALSE, maskvalue=1)
      writeRaster(extr_sen2/10000, name_sen2_out, overwrite=TRUE)
      stack_images_20m <- stack(stack_images_20m, extr_sen2/10000)
    }
    
    for (j in c(3,2,1)){
      band_20m <- resample(stack_images_10m[[j]], stack_images_20m[[1]], method = "bilinear")
      stack_images_20m <- stack(band_20m, stack_images_20m)
    }
    stack_images_20m <- stack(stack_images_20m, cloud_mask)
    stack_images_20m <- stack(stack_images_20m, shadow_mask)
    names(stack_images_20m) <- c("B02", "B03", "B04", "B05", "B06", "B07", "B11", "B12", "B8A", "Cloud Mask", "Shadow Mask")
    assign(stack_name_20m, stack_images_20m)
    
    # create list of rasterstacks to perform calculations on later
    names_list <- c(names_list, date_string)
    list_10m <- c(list_10m, stack_images_10m)
    list_20m <- c(list_20m, stack_images_20m)
  }
  
  
  names(list_10m) <- names_list
  names(list_20m) <- names_list
  final_list <- list(list_10m, list_20m)
  names(final_list) <- c("10m_stack", "20m_stack")
  save(final_list, file = paste0(total_dir, SBZ_code, "/", SBZ_code, "_stacks.Rdata"))
  print(paste0("SBZ ", n, " of ", length(shp_SBZ), " done"))
}

