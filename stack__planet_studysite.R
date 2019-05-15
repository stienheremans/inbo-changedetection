stack_studysite <- function(name_studysite,shp_studysite){
  
  library(raster)
  library(sp)
  library(rgdal)
  library(stringr)
  library(readxl)
  library(dplyr)
  library(rasterVis)
  library(rlist)
  
  studysite <- readOGR(shp_studysite) #still needed after python download?
  dir <- paste0("Q:/Projects/PRJ_RemSen/Change detection 2018/inbo-downloadsubimages_python/outputs/Planet_data/", name_studysite)
  
  list_filenames <- list.files(dir, pattern = "*tif$", full.names = TRUE, recursive = TRUE)
  
  # Put all the actual raster files in a list
  list_rasters <- lapply(list_filenames, stack)
  
  # Get the accompanying attributes of the rasters from the excel saved in the download process (python)
  excel_filename <- paste0(dir, "/image_lists.xlsx")
  attr_rasters <- as.data.frame(read_excel(excel_filename, sheet = "final_ims_list")) 
  
  # Get one of the image ids with max overlap with the study site
  attr_rasters %>% arrange(desc(overlap)) %>% slice(1) %>% select(im_id) -> im_id_max_overlap
  
  # Select the image with the image id selected above from the list of filenames
  lapply(list_filenames, grep, pattern = im_id_max_overlap, value = T) %>% unlist() %>% stack()-> im_max_overlap
  
  # Make all rasters have the extent of the image selected above
  list_proj_rasters = list()
  j = 0
  for (i in list_rasters){
    j = j+1
    list_proj_rasters[j] <- projectRaster(i,im_max_overlap)
    print(paste0("Image ", j, " of ", length(list_rasters), " done"))
  }
  
  # Make a single rasterstack of all image bands from the entire time series
  stack_all_bands <- list_proj_rasters[[1]]
  for (i in 2:length(list_proj_rasters)){
    stack_all_bands <- stack(stack_all_bands, list_proj_rasters[[i]])
  }
  
  # Change the names of the layers in the raster stack to more easily understandable values
  labels_stack <- names(stack_all_bands)
  label_dates <- lapply(labels_stack, str_sub, 2, 9)
  label_bands <- lapply(labels_stack, str_sub,-1,-1)
  new_labels <- paste0("im",label_dates, "_band",label_bands)
  names(stack_all_bands) <- new_labels
  
  # Make a rasterstack per band
  stack_band1 <- stack_all_bands[[1]]
  for (i in 2:length(names(stack_all_bands))){
    if (grepl("band1", names(stack_all_bands)[i])){
      stack_band1 <- stack(stack_band1, stack_all_bands[[i]])
    }
  }
  
  stack_band2 <- stack_all_bands[[2]]
  for (i in 3:length(names(stack_all_bands))){
    if (grepl("band2", names(stack_all_bands)[i])){
      stack_band2 <- stack(stack_band2, stack_all_bands[[i]])
    }
  }
  
  stack_band3 <- stack_all_bands[[3]]
  for (i in 4:length(names(stack_all_bands))){
    if (grepl("band3", names(stack_all_bands)[i])){
      stack_band3 <- stack(stack_band3, stack_all_bands[[i]])
    }
  }
  
  stack_band4 <- stack_all_bands[[4]]
  for (i in 5:length(names(stack_all_bands))){
    if (grepl("band4", names(stack_all_bands)[i])){
      stack_band4 <- stack(stack_band4, stack_all_bands[[i]])
    }
  }
  
  
  # Save stack so we don't have to calculate each time
  file_loc <- paste0(dir, "/Planet_band_stack_", name_studysite, ".RData")
  save(stack_all_bands, file = file_loc)
  
  # Calculate ndvi for each item in the list of images
  stack_ndvi <- stack_band1
  for (j in 1:length(names(stack_band1))){
    stack_ndvi[[j]] = (stack_band4[[j]] - stack_band3[[j]])/(stack_band4[[j]] + stack_band3[[j]])
    names(stack_ndvi)[j] <- paste0(str_sub(names(stack_band1)[j],1, 10), "_ndvi")
  }
  
  
  plot_name <- paste0(name_studysite, "_allbands.png")
  png(plot_name, width = 4000, height = 3000, res = 300)
  plot(stack_all_bands, nc = 4)
  dev.off()
  
  plot_name <- paste0(name_studysite, "_band1.png")
  png(plot_name, width = 4000, height = 3000, res = 300)
  levelplot(stack_band1, nc = 4)
  dev.off()
  
  plot_name <- paste0(name_studysite, "_band2.png")
  png(plot_name, width = 4000, height = 3000, res = 300)
  levelplot(stack_band2, nc = 4)
  dev.off()
  
  plot_name <- paste0(name_studysite, "_band3.png")
  png(plot_name, width = 4000, height = 3000, res = 300)
  levelplot(stack_band3, nc = 4)
  dev.off()
  
  plot_name <- paste0(name_studysite, "_band4.png")
  png(plot_name, width = 4000, height = 3000, res = 300)
  levelplot(stack_band4, nc = 4)
  dev.off()
  
  plot_name <- paste0(name_studysite, "_ndvi.png")
  png(plot_name, width = 4000, height = 3000, res = 300)
  levelplot(stack_ndvi, nc = 4)
  dev.off()
  
  # Perfoem an unsupervised classification based on time series of ndvi (kmeans clustering) with 1 to 20 clusters
  input_class <- data.frame()
  input_class <- as.data.frame(getValues(stack_ndvi))
  withinss_df <- data.frame(matrix(ncol = 2, nrow = 0))
  x <- c("n_clus", "value")
  colnames(withinss_df) <- x
  for (i in 20:30){
    set.seed(99)
    kmncluster <- kmeans(na.omit(input_class), centers = i, iter.max = 500, nstart = 5, algorithm="Lloyd")
    withinss_df[i,] <- c(i, kmncluster$tot.withinss)
    print(paste0(i, " clusters done"))
  }
  
  
  # plot total within cluster sum of squares to select best number of clusters
  clus_raster <- stack_ndvi[[1]]
  for (i in 1:length(kmncluster_10$cluster)){
    values(clus_raster)[i] <- kmncluster_10$cluster[i]
    proc = i*100/length(kmncluster_10$cluster)
    print(paste0("Pixel ", i, " van ",length(kmncluster_10$cluster), " = ", proc," procent"))
  }
  
  
  
}
  