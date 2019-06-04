library(ggplot2)
library(raster)
library(RStoolbox)
library(rasterVis)
library(gridExtra)
library(grid)
library(stringr)


# Get dirs in the Sen2_data folder where the layer stacks have been written by the stack_Sen2_allSBZ file
head_dir <- "Q:/Projects/PRJ_RemSen/Change detection 2018/change-detection files/data/Sen2_data/"
sub_dir <- "begin May"
total_dir <- paste0(head_dir, sub_dir)
dirs <- list.dirs(total_dir)
dirs <- dirs[ grepl("BE", dirs)]

for (n in dirs){
  file_stacks <- list.files(n, "stacks.Rdata$", full.names = TRUE, recursive = TRUE)
  start_loc <- str_locate(n, "BE")[1]
  end_loc <- nchar(n)
  name_studysite <- str_sub(n,start_loc ,end_loc)
  load(file_stacks)
  list_10m <- final_list[[1]]
  list_20m <- final_list[[2]]

  # Uit de opgeslagen raster stacks kunnen volgende indicatoren berekend worden (zie Jin et al. 2013):
    ## dNBR (band 4 en 12 voor Sen2 op 20m)
    ## dNDVI (band 4 en 8 voor Sen2 op 20m)
    ## CV (alle banden op 20m)
    ## RCVMAX (alle banden op 20m)


  dNBR_title <- paste0("dNBR of ", name_studysite)
  dNBR_plot <- paste0(n,"/dNBR of ", name_studysite, ".png")
  dNDVI_title <- paste0("dNDVI of ", name_studysite)
  dNDVI_plot <- paste0(n,"/dNDVI of ", name_studysite, ".png")
  CV_title <- paste0("CV of ", name_studysite)
  CV_plot <- paste0(n,"/CV of ", name_studysite, ".png")
  RCVMAX_title <- paste0("RCVMAX of ", name_studysite)
  RCVMAX_plot <- paste0(n,"/RCVMAX of ", name_studysite, ".png")

  
  myplots = list()
  dNBR <- list()
  for (i in 1:(length(list_20m)-1)){
    j = i + 1
    dNBR_it <- (((list_20m[[i]][[3]] - list_20m[[i]][[8]])/(list_20m[[i]][[3]] + list_20m[[i]][[8]]))  - ((list_20m[[j]][[3]] - list_20m[[j]][[8]])/(list_20m[[j]][[3]] + list_20m[[j]][[8]])))
    dNBR <- c(dNBR, dNBR_it)
    dates_dNBR <- paste0(names(list_20m)[i], " to ", names(list_20m)[j])
    name_dNBR <- paste0(n, "/", name_studysite, " dNBR ",  names(list_20m)[i], " to ", names(list_20m)[j])
    psub2 <- gplot(dNBR_it) + geom_tile(aes(fill = value)) +
      scale_fill_gradient2(low = 'darkred', high = 'darkgreen', mid = 'grey', na.value="white") +
    coord_equal() +  theme(axis.line=element_blank(),axis.text.x=element_blank(),
                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(), panel.background=element_blank(),panel.border=element_blank(),
                           panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank(),plot.background=element_blank(),plot.title =
                             element_text(size=10) ) + ggtitle(dates_dNBR) +
    labs(fill = "dNBR")
    myplots[[i]] <- psub2
    writeRaster(dNBR_it, name_dNBR, format = "GTiff", overwrite=TRUE)
  }
  names(dNBR) <- names(list_20m[2:length(list_20m)])
  p <-grid.arrange(grobs = myplots ,ncol = 2, heigths = rep(10, 3), top = textGrob(dNBR_title, gp=gpar(fontsize=15,font=1)))
  ggsave(p, file = dNBR_plot, height = 6, width = 12)

  myplots = list()
  dNDVI <- list()
  for (i in 1:(length(list_20m)-1)){
    j = i + 1
    dNDVI_it <- (((list_20m[[i]][[3]] - list_20m[[i]][[2]])/(list_20m[[i]][[3]] + list_20m[[i]][[2]]))  - ((list_20m[[j]][[3]] - list_20m[[j]][[2]])/(list_20m[[j]][[3]] + list_20m[[j]][[2]])))
    dNDVI <- c(dNDVI, dNDVI_it)
    dates_dNDVI <- paste0(names(list_20m)[i], " to ", names(list_20m)[j])
    name_dNDVI <- paste0(n, "/", name_studysite, " dNDVI ",  names(list_20m)[i], " to ", names(list_20m)[j])
    psub2 <- gplot(dNDVI_it) + geom_tile(aes(fill = value)) +
    scale_fill_gradient2(low = 'darkred', high = 'darkgreen', mid = 'grey', na.value="white") +
    coord_equal() +  theme(axis.line=element_blank(),axis.text.x=element_blank(),
                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(), panel.background=element_blank(),panel.border=element_blank(),
                           panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank(),plot.background=element_blank(),plot.title =
                             element_text(size=10) ) + ggtitle(dates_dNDVI) +
    labs(fill = "dNDVI")
    myplots[[i]] <- psub2
    writeRaster(dNDVI_it, name_dNDVI, format = "GTiff", overwrite=TRUE)
  }
  names(dNDVI) <- names(list_20m[2:length(list_20m)])
  p <-grid.arrange(grobs = myplots ,ncol = 2, heigths = rep(10, 3), top = textGrob(dNDVI_title, gp=gpar(fontsize=15,font=1)))
  ggsave(p, file = dNDVI_plot, height = 6, width = 12)

  myplots = list()
  CV <- list()
  for (i in 1:(length(list_20m)-1)){
    j = i + 1
    CV_it <- 0
    for (k in 1:9){
      CV_it <- CV_it + (list_20m[[i]][[k]] - list_20m[[j]][[k]])*2
    }
    CV <- c(CV, CV_it)
    dates_CV <- paste0(names(list_20m)[i], " to ", names(list_20m)[j])
    name_CV <- paste0(n, "/", name_studysite, " CV ",  names(list_20m)[i], " to ", names(list_20m)[j])
    psub2 <- gplot(CV_it) + geom_tile(aes(fill = value)) +
    scale_fill_gradient2(low = 'darkred', high = 'darkgreen', mid = 'grey', na.value="white") +
    coord_equal() +  theme(axis.line=element_blank(),axis.text.x=element_blank(),
                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(), panel.background=element_blank(),panel.border=element_blank(),
                           panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank(),plot.background=element_blank(),plot.title =
                             element_text(size=10) ) + ggtitle(dates_CV) +
    labs(fill = "CV")
    myplots[[i]] <- psub2
    writeRaster(CV_it, name_CV, format = "GTiff", overwrite=TRUE)
  }

  names(CV) <- names(list_20m[2:length(list_20m)])
  p <-grid.arrange(grobs = myplots ,ncol = 2, heigths = rep(10, 3), top = textGrob(CV_title, gp=gpar(fontsize=15,font=1)))
  ggsave(p, file = CV_plot, height = 6, width = 12)


  myplots = list()
  RCVMAX <- list()
  for (i in 1:(length(list_20m)-1)){
    j = i + 1
    RCVMAX_it <- 0
    for (k in 1:9){
      RCVMAX_it <- RCVMAX_it + ((list_20m[[i]][[k]] - list_20m[[j]][[k]])/max(list_20m[[i]][[k]],list_20m[[j]][[k]]))*2
  }
    RCVMAX <- c(RCVMAX, RCVMAX_it)
    dates_RCVMAX <- paste0(names(list_20m)[i], " to ", names(list_20m)[j])
    name_RCVMAX <- paste0(n, "/", name_studysite, " RCVMAX ",  names(list_20m)[i], " to ", names(list_20m)[j])
  psub2 <- gplot(RCVMAX_it) + geom_tile(aes(fill = value)) +
    scale_fill_gradient2(low = 'darkred', high = 'darkgreen', mid = 'grey', na.value="white") +
    coord_equal() +  theme(axis.line=element_blank(),axis.text.x=element_blank(),
                           axis.text.y=element_blank(),axis.ticks=element_blank(),
                           axis.title.x=element_blank(),
                           axis.title.y=element_blank(), panel.background=element_blank(),panel.border=element_blank(),
                           panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank(),plot.background=element_blank(),plot.title =
                             element_text(size=10) ) + ggtitle(dates_RCVMAX) +
    labs(fill = "RCVMAX")
    myplots[[i]] <- psub2
    writeRaster(RCVMAX_it, name_RCVMAX, format = "GTiff", overwrite=TRUE)
  }
  names(RCVMAX) <- names(list_20m[2:length(list_20m)])
  p <-grid.arrange(grobs = myplots ,ncol = 2, heigths = rep(10, 3), top = textGrob(RCVMAX_title, gp=gpar(fontsize=15,font=1)))
  ggsave(p, file = RCVMAX_plot, height = 6, width = 12)

  miica_list_ind <- list(dNBR, dNDVI, CV, RCVMAX)
  names(miica_list_ind) <- c("dNBR", "dNDVI", "CV", "RCVMAX")
  file_miica_list_ind <- paste0(n, "/",  name_studysite, "_miica_ind.Rdata")
  save(miica_list_ind, file = file_miica_list_ind)
  
}
