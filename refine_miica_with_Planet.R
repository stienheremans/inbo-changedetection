library(ggplot2)
library(raster)
library(RStoolbox)
library(rasterVis)
library(gridExtra)
library(grid)
library(stringr)
library(dplyr)

dirs <- list.dirs("Q:/Projects/PRJ_RemSen/Change detection 2018/change-detection files/data/Sen2_data/begin May")
dirs <- dirs[ grepl("BE", dirs)]

for (n in dirs){
  file_miica <- list.files(n, "miica_ind.Rdata$", full.names = TRUE, recursive = TRUE)
  load(file_miica)
  start_loc <- str_locate(n, "BE")[1]
  end_loc <- nchar(n)
  name_studysite <- str_sub(n,start_loc ,end_loc)
  print(paste0("Starting studysite ", name_studysite))
  
  Sen2_im <- raster(list.files(n, pattern = "20M.*.tif$", full = T)[1])
  
  
  dir_planet <- paste0("Q:/Projects/PRJ_RemSen/Change detection 2018/inbo-downloadsubimages_python/outputs/Planet_data/", name_studysite)
  planet_stack_loc <- list.files(dir_planet, pattern = "NDVI_stack.*.RData$",full = T)
  planet_stack <- load(planet_stack_loc)
  planet_proj <- projectRaster(stack_ndvi, Sen2_im)
  
  # Unsupervised classification based on time series of ndvi
  input_class <- data.frame()
  input_class <- as.data.frame(getValues(planet_proj))
  set.seed(99)
  
  clus_withinss <- data.frame(matrix(ncol = 2, nrow = 0))
  x <- c("n_clus", "withinss")
  colnames(clus_withinss) <- x
  for (i in 25){
    kmncluster <- kmeans(na.omit(input_class), centers = i, iter.max = 500, nstart = 5, algorithm="Lloyd")
    clus_withinss[i,1] <- i
    clus_withinss[i,2] <- kmncluster$tot.withinss
    print(paste0("With ", i, " clusters done"))
  }
  kmncluster_final <- kmncluster
  
  # Give cluster value to each cell
  knr <- planet_proj[[1]]
  knr <- reclassify(knr, cbind(-Inf, 10, NA), right=FALSE)
  for (p in 1:length(as.integer(names(kmncluster_final$cluster)))){
    m <- as.integer(names(kmncluster_final$cluster)[p])
    values(knr)[m] <- as.integer(kmncluster_final$cluster[[p]])
    print(p)
    print(m)
  }
  name_clus <- paste0("Q:/Projects/PRJ_RemSen/Change detection 2018/inbo-changedetection/data/sen2_data/begin May/", name_studysite, "/", name_studysite, "_Planet_NDVI_clusters.tif")
  writeRaster(knr, name_clus, overwrite=TRUE)
  
  # Stack clusters with miica rasters (per year combination)
  for (j in 1: length(miica_list_ind[[1]])){
    to_year <- names(miica_list_ind[[1]])[j]
    list_miica <- list()
    
    for (k in 1: length(miica_list_ind)){
      list_miica[k] <- miica_list_ind[[k]][[j]]
    }
    list_miica[k+1] <- knr
    stack_miica <- stack(list_miica)
    names(stack_miica) <- c(names(miica_list_ind), "clus")
    miica_clus <- data.frame()
    miica_clus <- as.data.frame(getValues(stack_miica))
    miica_clus_noNA <- na.omit(miica_clus)
    miica_clus_noNA$pix_nr <- row.names(miica_clus_noNA)
    # Calculate z scores for each indicator
    for (l in 1:4){
      miica_clus_noNA[6+l] <- ave(miica_clus_noNA[l], miica_clus_noNA$clus, FUN=scale)
      name_ind <- names(miica_clus_noNA)[l]
      ind_outliers <- miica_clus_noNA %>% filter(.[[6+l]] > 2 | .[[6+l]] < -2)
      rast_outliers <- stack_miica[[1]]
      rast_outliers <- reclassify(rast_outliers, cbind(-Inf, 10, NA), right=FALSE)
      for (o in 1:dim(ind_outliers)[1]){
        pix <- as.integer(ind_outliers$pix_nr[o])
        values(rast_outliers)[pix] <- ind_outliers[o, 6+l]
      }
      name_ras <- paste0("Q:/Projects/PRJ_RemSen/Change detection 2018/change-detection files/data/Sen2_data/begin May/", name_studysite, "/outliers_", to_year, "_", name_ind, "_", name_studysite)
      writeRaster(rast_outliers, name_ras, format = "GTiff", overwrite=TRUE)
    }
  }
  print(paste0("Ended studysite ", name_studysite))
}

