stack_studysite <- function(shp_studysite, sentile, studysite_name ){
   
  library(raster)
  library(sp)
  library(rgdal)
  library(stringr)
  
  studysite <- readOGR(shp_studysite)
  
  if (file.exists(file.path("Q:/Projects/PRJ_RemSen/change detection 2018/Cases/R analysis/Change_detection/data/Sen2_bands/", studysite_name)) == FALSE){
    dir.create(file.path("Q:/Projects/PRJ_RemSen/change detection 2018/Cases/R analysis/Change_detection/data/Sen2_bands/", studysite_name))
  }
  
  if (file.exists(file.path("Q:/Projects/PRJ_RemSen/change detection 2018/Cases/R analysis/Change_detection/data/Sen2layerstacks/", studysite_name)) == FALSE){
    dir.create(file.path("Q:/Projects/PRJ_RemSen/change detection 2018/Cases/R analysis/Change_detection/data/Sen2layerstacks/", studysite_name))
  }
    
  dirs <- list.dirs("Q:/Projects/PRJ_RemSen/Change detection 2018/Sentinel 2 images May")
  dirs <- dirs[ grepl(sentile, dirs) ]
  print ('original dirs')
  print(dirs)
  
  
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
    stack_name_10m <- paste(date_string,"Rasterstack_10m", studysite_name, sep = "_")
    list_img_10m <- list.files(i, pattern = "10M.*tif$", full.names = TRUE, recursive = TRUE)
    stack_images_10m <- stack()
    
    cloud_mask_img <- list.files(i, "CLOUDMASK.*10M.*tif$", full.names = TRUE, recursive = TRUE)
    cloud_in <- raster(cloud_mask_img[1])
    crop_cloud <- crop(cloud_in, studysite)
    shape_rast <- rasterize(studysite, crop_cloud)   
    cloud_mask <- mask(x=crop_cloud, mask=shape_rast)
    start_loc <- str_locate(cloud_mask_img, "V101/")[1,2]
    end_loc <- str_locate(cloud_mask_img, "V101.tif")[1,2]
    name_cloud_out <- paste("Q:/Projects/PRJ_RemSen/change detection 2018/Cases/R analysis/Change_detection/data/Sen2_bands/", studysite_name, "/", str_sub( cloud_mask_img,start_loc + 1,end_loc - 4) , "_Kalmthout.tif", sep = "")
    writeRaster(cloud_mask, name_cloud_out, overwrite=TRUE)
    
    shadow_mask_img <- list.files(i, "SHADOWMASK.*10M.*tif$", full.names = TRUE, recursive = TRUE)
    shadow_in <- raster(shadow_mask_img[1])
    crop_shadow <- crop(shadow_in, studysite)
    shadow_mask <- mask(x=crop_shadow, mask=shape_rast)
    start_loc <- str_locate(shadow_mask_img, "V101/")[1,2]
    end_loc <- str_locate(shadow_mask_img, "V101.tif")[1,2]
    name_mask_out <- paste("Q:/Projects/PRJ_RemSen/change detection 2018/Cases/R analysis/Change_detection/data/Sen2_bands/", studysite_name, "/", str_sub( shadow_mask_img,start_loc + 1,end_loc - 4) , "_Kalmthout.tif", sep = "")
    writeRaster(shadow_mask,name_mask_out, overwrite=TRUE)
    
    for (j in c(3:length(list_img_10m))){
      sen2_in <- raster(list_img_10m[j])
      start_loc <- str_locate(list_img_10m[j], "V101/")[1,2]
      end_loc <- str_locate(list_img_10m[j], "V101.tif")[1,2]
      name_sen2_out <- paste("Q:/Projects/PRJ_RemSen/change detection 2018/Cases/R analysis/Change_detection/data/Sen2_bands/", studysite_name, "/", str_sub(list_img_10m[j],start_loc + 1,end_loc - 4) , "_Kalmthout.tif", sep = "")
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
    stack_name_20m <- paste(date_string,"Rasterstack_20m", studysite_name, sep = "_")
    list_img_20m <- list.files(i, pattern = "20M.*tif$", full.names = TRUE, recursive = TRUE)
    stack_images_20m <- stack()
    
    cloud_mask_img <- list.files(i, "CLOUDMASK.*20M.*tif$", full.names = TRUE, recursive = TRUE)
    cloud_in <- raster(cloud_mask_img[1])
    crop_cloud <- crop(cloud_in, studysite)
    shape_rast <- rasterize(studysite, crop_cloud)   
    cloud_mask <- mask(x=crop_cloud, mask=shape_rast)
    start_loc <- str_locate(cloud_mask_img, "V101/")[1,2]
    end_loc <- str_locate(cloud_mask_img, "V101.tif")[1,2]
    name_cloud_out <- paste("Q:/Projects/PRJ_RemSen/change detection 2018/Cases/R analysis/Change_detection/data/Sen2_bands/", studysite_name, "/", str_sub( cloud_mask_img,start_loc + 1,end_loc - 4) , "_Kalmthout.tif", sep = "")
    writeRaster(cloud_mask, name_cloud_out, overwrite=TRUE)
    
    shadow_mask_img <- list.files(i, "SHADOWMASK.*20M.*tif$", full.names = TRUE, recursive = TRUE)
    shadow_in <- raster(shadow_mask_img[1])
    crop_shadow <- crop(shadow_in, studysite)
    shadow_mask <- mask(x=crop_shadow, mask=shape_rast)
    start_loc <- str_locate(shadow_mask_img, "V101/")[1,2]
    end_loc <- str_locate(shadow_mask_img, "V101.tif")[1,2]
    name_mask_out <- paste("Q:/Projects/PRJ_RemSen/change detection 2018/Cases/R analysis/Change_detection/data/Sen2_bands/", studysite_name, "/", str_sub( shadow_mask_img,start_loc + 1,end_loc - 4) , "_Kalmthout.tif", sep = "")
    writeRaster(shadow_mask,name_mask_out, overwrite=TRUE)
    
    for (j in c(4:length(list_img_20m))){
      sen2_in <- raster(list_img_20m[j])
      start_loc <- str_locate(list_img_20m[j], "V101/")[1,2]
      end_loc <- str_locate(list_img_20m[j], "V101.tif")[1,2]
      name_sen2_out <- paste("Q:/Projects/PRJ_RemSen/change detection 2018/Cases/R analysis/Change_detection/data/Sen2_bands/", studysite_name, "/",str_sub(list_img_20m[j],start_loc + 1,end_loc - 4) , "_Kalmthout.tif", sep = "")
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
  save(, file = paste0("Q:/Projects/PRJ_RemSen/Change detection 2018/Cases/R analysis/Change_detection/data/sen2layerstacks/", studysite_name, "/", studysite_name, "_stacks.Rdata"))
  
  return(final_list)
}
