
library(raster)
library(sp)
library(rgdal)

# Get shapefile of SBZs in UTM31N
file_SBZ <- "../inbo-downloadsubimages_python/inputs/shp_allSBZ/SBZ-Hdeel_diss_UES_UFS_UTM31N.shp"
shp_SBZ <- readOGR(file_SBZ)



for (n in 1:length(shp_SBZ)){
  # Keep name and code of SBZ (subarea)
  studysite <- shp_SBZ[n,]
  SBZ_code = shp_SBZ[n,]$DEELGEBIED
  SBZ_name = shp_SBZ[n,]$FIRST_NAAM
  
  dirs <- list.dirs("Q:/Projects/PRJ_RemSen/Change detection 2018/change-detection files/data/Sen2_data")
  dirs <- dirs[ grepl(SBZ_code, dirs)]
  dirs <- dirs[ -grep("leaflet", dirs)]
  sen_2016 <- stack()
  sen_2017 <- stack()
  for (o in dirs){
    sen_file <- load(list.files(o, pattern = "stacks.Rdata$", full.names = TRUE, recursive = TRUE))
    sen_stack <- final_list$`20m_stack`
    sen_2016 <- stack(sen_2016, sen_stack[[1]][[1:9]]) # exclude cloud mask and shadow mask
    sen_2017 <- stack(sen_2017, sen_stack[[2]][[1:9]])
  }
 
  
}


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
  
}
