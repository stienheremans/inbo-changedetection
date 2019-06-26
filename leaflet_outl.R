# In this file, a leaflet is created with the outliers for each MIICA indicator, the sum of the absolute values, the number indicators with outlier values and the type of indicators with outliers


library(leaflet)
library(htmlwidgets)
library(htmltools)
library(stringr)
library(raster)
library(ggmap)
library(leafsync)
library(magrittr)
library(devtools)
library(dplyr)

dirs <- list.dirs("Q:/Projects/PRJ_RemSen/Change detection 2018/change-detection files/data/Sen2_data/begin May")
dirs <- dirs[ grepl("BE", dirs)]

save_tags <- function (tags, file, selfcontained = F, libdir = "./lib") 
{
  if (is.null(libdir)) {
    libdir <- paste(tools::file_path_sans_ext(basename(file)), 
                    "_files", sep = "")
  }
  htmltools::save_html(tags, file = file, libdir = libdir)
  if (selfcontained) {
    if (!htmlwidgets:::pandoc_available()) {
      stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n", 
           "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
    }
    htmlwidgets:::pandoc_self_contained_html(file, file)
    unlink(libdir, recursive = TRUE)
  }
  return(file)
}

for (n in dirs[1:length(dirs)]){
  # import all necessary files (4 miica indicators for each year, outliers for each indicator without Planet for each year)
  files <- list.files(n, full.names = T)
  files_outliers <- files[grepl("outliers_no.*tif$", files)]
  
  # fixed map layout
  tag.map.title <- tags$style(HTML("
                                   .leaflet-control.map-title { 
                                   transform: translate(-50%,20%);
                                   position: fixed !important;
                                   left: 50%;
                                   text-align: center;
                                   padding-left: 10px; 
                                   padding-right: 10px; 
                                   background: rgba(255,255,255,0.75);
                                   font-weight: bold;
                                   font-size: 28px;
                                   }
                                   "))
  
  
  map <- leaflet() %>%
    addProviderTiles('Esri.WorldImagery', group = "basemap")
  
  
  ind_outl <- list()
  
  
  for (m in 1:(length(files_outliers)/4)){
    #create rasters with specifications of miica outlier rasters but all values equal to 0
    rast_numb <- raster(files_outliers[1])
    rast_numb <- reclassify(rast_numb, cbind(-20, 20, NA), right = TRUE)
    rast_type <- rast_sum <- rast_numb <- reclassify(rast_numb, cbind(NA, NA, 0), right = TRUE)
    bin = 0
    for (p in 1:4){
      o = (m-1)*4+ p
      year_loc <- str_locate(files_outliers[o], "Planet")[1]
      year <- str_sub(files_outliers[o],year_loc + 7 , year_loc + 14)
      ind_loc_start <- year_loc + 16
      ind_loc_end <- str_locate(files_outliers[o], "_BE")[1]
      ind <- str_sub(files_outliers[o],ind_loc_start, ind_loc_end - 1)
      studysite_loc_start <- ind_loc_end + 1
      studysite_loc_end <- str_locate(files_outliers[o], ".tif$")[1]
      studysite <- str_sub(files_outliers[o],studysite_loc_start , studysite_loc_end-1)
      rast_outl <- raster(files_outliers[o])
      proj_WGS84 <- CRS("+proj=longlat +datum=WGS84")
      rast_WGS84 <- projectRaster(rast_outl, crs = proj_WGS84)
      
      rast_outl_1 <-  reclassify(rast_outl, cbind(-Inf, Inf, 1), right = TRUE)
      rast_outl_1 <-  reclassify(rast_outl_1, cbind(NA, NA, 0), right = TRUE)
      rast_outl_2 <-  reclassify(rast_outl, cbind(NA, NA, 0), right = TRUE)
      rast_numb <- rast_numb + rast_outl_1
      rast_sum <- rast_sum + sqrt(rast_outl_2*rast_outl_2)
      rast_type <- rast_type + (10^bin)*rast_outl_1
      
      ind_outl <- c(ind_outl, paste0(ind, year))
      if (!is.infinite(rast_WGS84@data@min) &  !is.infinite(rast_WGS84@data@max)){
        pal <- colorNumeric(c("royalblue", "yellow", "red"), values(rast_WGS84),
                           na.color = "transparent")
        
        map <- map %>%
          addRasterImage(rast_WGS84, colors = pal, opacity = 1, group = paste0(ind, year)) %>%
          addLegend("bottomleft", pal = pal, values = values(rast_WGS84),
                    title = paste0(ind, " ", year),
                    opacity = 0.8, group =  paste0(ind, year))
        }
      
      bin = bin + 1
      }
    
    rast_numb <- reclassify(rast_numb, cbind(-1, 0, NA), right = TRUE)
    rast_sum <- reclassify(rast_sum, cbind(-1, 0, NA), right = TRUE)
    rast_type <- reclassify(rast_type, cbind(-1, 0, NA), right = TRUE)
    type_class <- c(1, 10, 11, 100, 101, 110, 111, 1000, 1001, 1010, 1011, 1100, 1101, 1110, 1111)
    rast_type <- reclassify(rast_type,cbind(type_class-0.5, type_class, 1:length(type_class)))
    
    col_scheme_type <- cbind(type_class, 1:length(type_class), c("hotpink", "purple","blue", "blue3", "green","green4", "seagreen", "yellow", "orange", "red","violetred2", "darkred", "brown", "salmon", "grey56" ))
    
    print(col_scheme_type)
    
    proj_WGS84 <- CRS("+proj=longlat +datum=WGS84")
    rast_numb_WGS84 <- projectRaster(rast_numb, crs = proj_WGS84)
    rast_sum_WGS84 <- projectRaster(rast_sum, crs = proj_WGS84)
    rast_type_WGS84 <- projectRaster(rast_type, crs = proj_WGS84)
    
    if (!is.infinite(rast_numb_WGS84@data@min) &  !is.infinite(rast_numb_WGS84@data@max)){
      pal2 <- colorNumeric(c("red", "yellow", "green", "darkgreen"), values(rast_numb_WGS84),
                           na.color = "transparent")
      pal3 <- colorNumeric(c("red", "yellow", "green", "darkgreen"), values(rast_sum_WGS84),
                           na.color = "transparent")
      
      
      pal4 <- colorNumeric(c("hotpink", "purple","blue", "blue3", "green","green4", "seagreen", "yellow", "orange", "red","violetred2", "darkred", "brown", "salmon", "grey56", "black"),values(rast_type_WGS84),
                           na.color = "transparent")
      
      map <- map %>%
        addRasterImage(rast_numb_WGS84, colors = pal2, opacity = 1, group = paste0("numb",year)) %>%
        addLegend("bottomleft", pal = pal2, values = values(rast_numb_WGS84),
                  title = paste0("number outliers ", year),
                  opacity = 0.8, group = paste0("numb", year))
      
      map <- map %>%
        addRasterImage(rast_sum_WGS84, colors = pal3, opacity = 1, group = paste0("sum", year)) %>%
        addLegend("bottomleft", pal = pal3, values = values(rast_sum_WGS84),
                  title = paste0("sum of z values outliers ", year),
                  opacity = 0.8, group = paste0("sum", year))
      
      map <- map %>%
        addRasterImage(rast_type_WGS84, colors = pal4, opacity = 1, group = paste0("type", year)) %>%
        addLegend("bottomleft", pal = pal4, values = unique(rast_type_WGS84),
                  title = paste0("sum of z values outliers ", year),
                  opacity = 0.8, group = paste0("type", year))
      
      ind_outl <- c(ind_outl, paste0("numb", year),  paste0("sum", year),  paste0("type", year))
      
      name_numb <- paste0(n, "/summary_noPlanet_", year,"_number_ind_",studysite )
      writeRaster(rast_numb, name_numb, format = "GTiff", overwrite=TRUE)
      
      name_sum <- paste0(n, "/summary_noPlanet_", year,"_sum_ind_",studysite )
      writeRaster(rast_sum, name_sum, format = "GTiff", overwrite=TRUE)
      
      name_type <- paste0(n, "/summary_noPlanet_", year,"_type_ind_",studysite )
      writeRaster(rast_type, name_type, format = "GTiff", overwrite=TRUE)
    }
    
  }
  
  groups_hide <- c(ind_outl)
  
  title_map <- tags$div(
    tag.map.title, HTML(paste0(studysite)))
  
  map <- map %>%
    addLayersControl(
      baseGroups = c("basemap"),
      overlayGroups = groups_hide,
      options = layersControlOptions(collapsed = FALSE), position = "bottomright") %>%
    hideGroup(groups_hide)
  
  map1 <- leaflet() %>% 
    addTiles() %>%
    addWMSTiles(
      "https://geoservices.informatievlaanderen.be/raadpleegdiensten/OMW/wms?",
      layers = "OMWRGB16VL",
      options = WMSTileOptions(format = "image/png", transparent = F)
    )
  
  map2 <- leaflet() %>% 
    addTiles() %>%
    addWMSTiles(
      "https://geoservices.informatievlaanderen.be/raadpleegdiensten/OMW/wms?",
      layers = "OMWRGB17VL",
      options = WMSTileOptions(format = "image/png", transparent = F)
    )
  map3 <- leaflet() %>% 
    addTiles() %>%
    addWMSTiles(
      "https://geoservices.informatievlaanderen.be/raadpleegdiensten/OMW/wms?",
      layers = "OMWRGB18VL",
      options = WMSTileOptions(format = "image/png", transparent = F)
    )
  
  
  leafsync::latticeView(map, map1, map2, map3, ncol = 2, sync = list(c(2, 2)), sync.cursor = TRUE, no.initial.sync = FALSE)
  maps <- leafsync::sync(map, map1, map2, map3)
  
  file_name <- paste0("Q:/Projects/PRJ_RemSen/Change detection 2018/change-detection files/data/Sen2_data/leaflets/begin May/ouliers_", studysite, ".html")

  save_tags(maps, file_name, selfcontained=F)
  
}
