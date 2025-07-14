library(ggplot2)
library(dplyr)
library(sf)
library(stringr)
library(terra)
library(tidyterra)
library(patchwork)

polygon_extent <- read_sf("D:\\Postdoc_work\\UMD\\API_query\\Biomass_polygon1.geojson")

scenes_folders <- list.files('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data',
                     full.names = T)

clear_middle <- function(labels){
  new_labels <- rep('',length(labels))
  new_labels[1] <- labels[1]
  new_labels[length(labels)] <- labels[length(labels)]
  new_labels
}

clear_middle(1:5)

vis_sites <- function(path){
  site_code <- stringr::str_extract(path,'Onfarm_[A-Z]{3}') %>% 
    stringr::str_remove('Onfarm_')
  plots <- polygon_extent %>% filter(code==site_code)
  tiffs <- list.files(path, full.names = T, pattern = '*AnalyticMS_SR_8b_clip.tif')
  tiff_subset <- c(head(tiffs,1),tail(tiffs,1))
  first <- rast(tiff_subset[1])
  last <- rast(tiff_subset[2])
  
  p1 <- ggplot()+
    geom_spatraster(data=first %>% 
                      mutate(NDVI=(nir-red)/(nir+red)) ,aes(fill=NDVI),show.legend = F)+
    geom_sf(data=plots, fill=NA, linewidth=2, color='white')+
    scale_fill_viridis_c(limits=c(0,1))+
    scale_x_continuous(labels = clear_middle)+
    scale_y_continuous(labels = clear_middle)+
    labs(title=site_code, subtitle = str_extract(tiff_subset[1],pattern='20[0-9]{6}'),fill='NDVI')
  
  p2 <- ggplot()+
    geom_spatraster(data=last %>% 
                      mutate(NDVI=(nir-red)/(nir+red)) ,aes(fill=NDVI))+
    geom_sf(data=plots, fill=NA, linewidth=2, color='white')+
    scale_fill_viridis_c(limits=c(0,1))+
    scale_x_continuous(labels = clear_middle)+
    scale_y_continuous(labels = clear_middle)+
    labs(title=site_code,subtitle = str_extract(tiff_subset[2],pattern='20[0-9]{6}'),fill='NDVI')
  
  ggsave(
    filename =paste0('D:\\Postdoc_work\\UMD\\API_query\\Planet\\onfarm_planet_outputs\\',site_code,'.pdf'),
    plot=p1+p2,
    width = 8, height = 6
    )
  
  p1+p2
}

vis_sites(scenes_folders[1])

purrr::map(scenes_folders,vis_sites)











