library(dplyr)
library(terra)
library(sf)
library(exactextractr);library(stringr)
library(jsonlite)

# fields = st_read('D:\\Postdoc_work\\UMD\\API_query\\Biomass_polygon1.geojson') %>%
#   st_transform("epsg:32618") %>%
#   select(Field) %>%
#   st_buffer(dist= -10)

sites <- c('CFZ','CUU','CWZ', 'DDQ', 'DVH')

## some points and polygons donot match
fields = st_read('D:\\Postdoc_work\\UMD\\API_query\\Biomass_polygon1.geojson') %>% 
  dplyr::filter(code %in% sites)

# plot polygons to check if ok
plot(fields$geometry[1])
plot(fields$geometry[2], add=T)

# map the polygon using 'leaflet'package
library(leaflet)
leaflet() %>% 
  addTiles(urlTemplate = "https://tiles.stadiamaps.com/tiles/alidade_satellite/{z}/{x}/{y}{r}.jpg",
           attribution = '&copy; CNES, Distribution Airbus DS, © Airbus DS, © PlanetObserver (Contains Copernicus Data) | &copy; <a href="https://www.stadiamaps.com/" target="_blank">Stadia Maps</a> &copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors') %>% 
  addPolygons(data = st_geometry(fields))
  

pl.m <- list.files('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data\\8band_onfarm_planet_sites',
                   recursive = T, full.names = T,
                   pattern = '*metadata.json$')

pl.m_df <- pl.m %>% as.data.frame() %>% 
  mutate(code=stringr::str_extract(pl.m,"Onfarm_[A-Z]{3}"), 
         code=str_remove(code,"Onfarm_"))

pl.m_df_subset <- pl.m_df[pl.m_df$code=='CWZ',]
  
pl.m <- pl.m_df_subset$.[c(1:4, 6:227)]

#LOOP for reading in metadata into dataframe
meta.df <- data.frame()
for (m in 1:length(pl.m)){
  print(paste("Loop m at:", m))
  file = fromJSON(pl.m[m])
  id = file$id
  instrument = file$properties$instrument
  Date = file$properties$acquired
  satellite_id = file$properties$satellite_id
  strip_id = file$properties$strip_id
  quality_category = (file$properties$quality_category %>% as.character())
  clear_confidence_percent = (file$properties$clear_confidence_percent %>% as.numeric())
  clear_percent = (file$properties$clear_confidence_percent %>% as.numeric())
  sun_elevation = (file$properties$sun_elevation %>% as.numeric())
  sun_azimuth = (file$properties$sun_azimuth %>% as.numeric())
  view_angle = (file$properties$view_angle %>% as.numeric()) #aka off-nadir angle
  # Print the length of each variable
  print(paste("Length of instrument:", length(instrument)))
  print(paste("Length of Date:", length(Date)))
  print(paste("Length of satellite_id:", length(satellite_id)))
  print(paste("Length of strip_id:", length(strip_id)))
  print(paste("Length of clear_confidence_percent:", length(clear_confidence_percent)))
  print(paste("Length of clear_percent:", length(clear_percent)))
  print(paste("Length of sun_elevation:", length(sun_elevation)))
  print(paste("Length of sun_azimuth:", length(sun_azimuth)))
  print(paste("Length of view_angle:", length(view_angle)))
  df <- data.frame(id,
                   instrument,
                   Date,
                   satellite_id,
                   strip_id,
                   quality_category,
                   clear_confidence_percent,
                   clear_percent,
                   sun_elevation,
                   sun_azimuth,
                   view_angle)
  meta.df <- rbind(meta.df, df)
}
meta.df.final <- meta.df %>%
  mutate(
    Date2 = str_extract(Date,'202[34]{1}-[0-9]{2}-[0-9]{2}') %>% as.character() %>%
      str_remove_all('-')
  )
meta.df.final <-  meta.df[meta.df$clear_confidence_percent >94.999 & meta.df$quality_category =='standard',]

#LOOPING THROUGH THE SUBSET OF IMAGES
setwd('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data\\8band_onfarm_planet_sites\\Onfarm_CWZ')
base_dir = 'D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data\\8band_onfarm_planet_sites\\Onfarm_CWZ'
tiles = list.files('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data\\8band_onfarm_planet_sites\\Onfarm_CWZ')
                   
fields.output = fields$code %>% as.data.frame()
for(i in 1:length(tiles)){
  print(paste("Loop  i at:", i))
  # tile_dir = paste(base_dir,tiles[i],"PSScene", sep = "\\") # lists Tile directory
  # setwd(tile_dir)
  scenes = list.files(pattern = glob2rx("*8b_clip.tif"))
  scenes = subset(scenes,str_extract(
    scenes,'[0-9]{8}_[0-9]{6}_[0-9]{2}_[:graph:]{4}') %in% meta.df.final$id)
  scene.id = str_extract(
    scenes[i],'[0-9]{8}_[0-9]{6}_[0-9]{2}_[:graph:]{4}')
  for (j in 1:length(scenes)){
    print(paste("Loop j at:", j))
    mask.file = paste0(scenes[j],'_3B_udm2_clip.tif') %>% str_remove('_3B_AnalyticMS_SR_8b_clip.tif')
    rast.stk = rast(scenes[j])
    rast.stk$NDVI = (rast.stk$nir-rast.stk$red) / (rast.stk$nir+rast.stk$red)
    set.names(rast.stk,
              c(
                paste0(str_extract(scenes[j],'[0-9]{8}_[0-9]{6}_[0-9]{2}_[:graph:]{4}'),'_cb'),
                paste0(str_extract(scenes[j],'[0-9]{8}_[0-9]{6}_[0-9]{2}_[:graph:]{4}'),'_blue'),
                paste0(str_extract(scenes[j],'[0-9]{8}_[0-9]{6}_[0-9]{2}_[:graph:]{4}'),'_greeni'),
                paste0(str_extract(scenes[j],'[0-9]{8}_[0-9]{6}_[0-9]{2}_[:graph:]{4}'),'_green'),
                paste0(str_extract(scenes[j],'[0-9]{8}_[0-9]{6}_[0-9]{2}_[:graph:]{4}'),'_yellow'),
                paste0(str_extract(scenes[j],'[0-9]{8}_[0-9]{6}_[0-9]{2}_[:graph:]{4}'),'_red'),
                paste0(str_extract(scenes[j],'[0-9]{8}_[0-9]{6}_[0-9]{2}_[:graph:]{4}'),'_rededge'),
                paste0(str_extract(scenes[j],'[0-9]{8}_[0-9]{6}_[0-9]{2}_[:graph:]{4}'),'_nir'),
                paste0(str_extract(scenes[j],'[0-9]{8}_[0-9]{6}_[0-9]{2}_[:graph:]{4}'),'_ndvi')
              ))
    udm.mask = rast(mask.file) %>% subset(1) #resource for UDM2 cloud mask https://developers.planet.com/docs/data/udm-2/
    udm.mask[udm.mask != 1] <- NA
    stk.mask = mask(rast.stk, mask = udm.mask) # mask by clear band
    #use exactextractr package to extract mean reflectance for each buffered field polygon
    pl.ext = exact_extract(stk.mask,
                           fields[fields$code=='CWZ',] %>% st_transform(., crs = crs(rast.stk)),
                           'median')
    fields_extract = cbind(fields$code,
                           pl.ext)
    fields.output = cbind(fields.output,pl.ext)
    gc()
    tmpFiles(remove = TRUE)
  }
  gc()
}
fields.output.final <- fields.output %>%
  dplyr::rename('identifier' = '.')
setwd('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data\\PSA Time Series')
write.csv(fields.output.final,'fields_output_final.csv',row.names = FALSE)

#READ PLANET DATA AND SELECT ONLY NDVI AND THIS WILL BE USED FOR PLOTTING
data.final <- read.csv(paste0(
  getwd(),'\\fields_output_final.csv'
))
data.final.2 <- pivot_longer(data.final,cols=c(2:784)) %>%
  mutate(
    scene = str_extract(name,'[0-9]{8}_[0-9]{6}_[0-9]{2}_[:graph:]{4}'),
    sensor = str_remove(scene,'[0-9]{8}_[0-9]{6}_[0-9]{2}_'),
    band = str_remove(name,'median.[0-9]{8}_[0-9]{6}_[0-9]{2}_[:graph:]{4}_'),
    band = ifelse(band == 'cb','coastal blue',band),
    date = str_extract(scene,'[0-9]{8}') %>% ymd()
  )
data.ndvi <- subset(data.final.2,band=='ndvi')
field.id <-  data.ndvi$identifier %>% unique()
2:08
#PLOTTING
library(ggplot2);library(ggtext)
#field.id.sub <- subset(data.ndvi,identifier %in% field.id[1:2])
for(i in 1:length(field.id)){
  print(paste("Loop  i at:", i))
  field.id.sub = subset(data.ndvi,identifier %in% field.id[i])
  p <- ggplot(field.id.sub,
              aes(x=date, y=value))+
    geom_point(alpha=0.6, size=2)+
    scale_y_continuous(limits = c(0,1),
                       breaks=seq(0,1,by=.1))+
    scale_x_date(date_breaks = '2 months')+
    geom_line(aes(x=date, y=value)) +
    geom_smooth()+
    labs(x = "Date", y = "NDVI") +
    ggtitle(paste0('Planetscope NDVI Values\n',
                   ' Field ',unique(field.id.sub$identifier)))+
    theme_minimal()+
    theme(plot.title = element_text(size=18, hjust=0.5, face = "bold"),
          axis.text.x=element_text(size=15,angle = 45, hjust = 1),
          axis.text.y=element_text(size=15),
          axis.title.x=element_markdown(size=18),
          axis.title.y = element_markdown(size=18),
          legend.text= element_text(size =15))
  ggsave(path = 'C:\\PSA\\Remote Sensing Team\\Projects\\BARC Time Series\\BARC Time Series 2023-2024\\OUTPUTS',
         filename = paste0(field.id[i],'_ndvi_',
                           str_extract(Sys.time(),'[0-9]{4}-[0-9]{2}-[0-9]{2}'),
                           '.png'),
         plot = p, width=15, height = 8, bg= 'white')
}







