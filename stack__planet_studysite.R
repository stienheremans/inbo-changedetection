stack_studysite <- function(name_studysite,shp_studysite){
  
  library(raster)
  library(sp)
  library(rgdal)
  library(stringr)
  
  studysite <- readOGR(shp_studysite)
  dir <- paste0("./data/Planet images/", name_studysite, "/analytic")
  
  list_planet <- list()
  names_list <- list()
  
  list_planet_files <- list.files(dir, pattern = NULL, full.names = TRUE, recursive = TRUE)
  
}
  