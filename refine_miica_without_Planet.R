library(ggplot2)
library(raster)
library(RStoolbox)
library(rasterVis)
library(gridExtra)
library(grid)
library(stringr)
library(dplyr)

dirs <- list.dirs("Q:/Projects/PRJ_RemSen/Change detection 2018/change-detection files/data/Sen2_data/begin May")
dirs <- dirs[ grepl("BE2500001-25", dirs)]

for (n in dirs){
  file_miica <- list.files(n, "miica_ind.Rdata$", full.names = TRUE, recursive = TRUE)
  load(file_miica)
  start_loc <- str_locate(n, "BE")[1]
  end_loc <- nchar(n)
  name_studysite <- str_sub(n,start_loc ,end_loc)
  
  
  # Stack miica rasters (per year combination)
  for (j in 1: length(miica_list_ind[[1]])){
    to_year <- names(miica_list_ind[[1]])[j]
    list_miica <- list()
    
    for (k in 1: length(miica_list_ind)){
      list_miica[k] <- miica_list_ind[[k]][[j]]
    }
    stack_miica <- stack(list_miica)
    names(stack_miica) <- names(miica_list_ind)
    miica_only <- data.frame()
    miica_only <- as.data.frame(getValues(stack_miica))
    miica_only_noNA <- na.omit(miica_only)
    miica_only_noNA$pix_nr <- row.names(miica_only_noNA)
    # Calculate z scores for each indicator
    for (l in 1:4){
      miica_only_noNA[5+l] <- as.numeric(scale(miica_only_noNA[l]))
      name_ind <- names(miica_only_noNA)[l]
      ind_outliers <- miica_only_noNA %>% filter(.[[5+l]] > 2 | .[[5+l]] < -2)
      rast_outliers <- stack_miica[[1]]
      rast_outliers <- reclassify(rast_outliers, cbind(-Inf, 10, NA), right=FALSE)
      for (o in 1:dim(ind_outliers)[1]){
        pix <- as.integer(ind_outliers$pix_nr[o])
        print(pix)
        values(rast_outliers)[pix] <- ind_outliers[o, 5+l]
      }
      name_ras <- paste0("Q:/Projects/PRJ_RemSen/Change detection 2018/change-detection files/data/Sen2_data/begin May/", name_studysite, "/outliers_noPlanet_", to_year, "_", name_ind, "_", name_studysite)
      writeRaster(rast_outliers, name_ras, format = "GTiff", overwrite=TRUE)
    }
  }
}

