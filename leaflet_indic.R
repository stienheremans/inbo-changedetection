# Make leaflet illustration of indicator rasters
library(raster)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(stringr)

dirs <- list.dirs("Q:/Projects/PRJ_RemSen/Change detection 2018/change-detection files/data/Sen2_data/begin May")
dirs <- dirs[ grepl("BE", dirs)]

for (n in dirs){
  
  # import all necessary files (4 miica indicators for each year, outliers for each indicator without Planet for each year)
  files <- list.files(n, full.names = T)
  files_indicators <- files[grepl("to.*tif$", files)]
  
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
  
  indics <- list()
  
  for (o in 1:length(files_indicators)){
    year_loc <- str_locate(files_indicators[o], ".tif")[1]
    year <- str_sub(files_indicators[o],year_loc - 20 , year_loc - 1)
    ind_loc_start <- str_locate_all(files_indicators[o], " ")[[1]][5,1]
    ind_loc_end <- str_locate_all(files_indicators[o], " ")[[1]][6,1]
    ind <- str_sub(files_indicators[o],ind_loc_start, ind_loc_end)
    studysite_loc <- str_locate_all(files_indicators[o], "/BE")[[1]][2,1]
    studysite <- str_sub(files_indicators[o],studysite_loc+1, ind_loc_start-1)
    rast_ind <- raster(files_indicators[o])
    proj_WGS84 <- CRS("+proj=longlat +datum=WGS84")
    rast_WGS84 <- projectRaster(rast_ind, crs = proj_WGS84) 
    indics <- c(indics, paste0(ind, year))
    pal <- colorNumeric(c("royalblue", "yellow", "red"), values(rast_WGS84),
                        na.color = "transparent")
    map <- map %>%
      addRasterImage(rast_WGS84, colors = pal, opacity = 1, group = paste0(ind, year)) %>%
      addLegend("bottomleft", pal = pal, values = values(rast_WGS84),
                         title = paste0(ind, year),
                         opacity = 0.8, group =  paste0(ind, year))
  }
  title_map <- tags$div(
    tag.map.title, HTML(paste0(studysite)))

  map <- map%>%
    addLayersControl(
      baseGroups = c("basemap"),
      overlayGroups = indics,
      options = layersControlOptions(collapsed = FALSE)) %>%
    addControl(title_map, position = "topleft", className="map-title") %>%
    hideGroup(indics)
  map
  file_name <- paste0("Q:/Projects/PRJ_RemSen/Change detection 2018/change-detection files/data/Sen2_data/leaflets/begin May/indicators_", studysite, ".html")
  saveWidget(map, file = file_name)
}

