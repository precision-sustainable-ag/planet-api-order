library(jsonlite)
library(stringr)
library(dplyr)

#list all metadata files with the path
pl.m <- list.files(path= 'D:\\Remote Sensing Team\\PENNSYLVANIA\\IMAGERY\\Planet', #name path where all files reside
                   recursive = T, full.names = T,
                   pattern = '*metadata.json$')

#Loop through list of metadata files to create a dataframe
meta.df <- data.frame()

for (m in 1:length(pl.m)){
  print(paste("Loop m at:", m))
  file = fromJSON(pl.m[m])
  
  id = file$id #the scene id
  
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

#Filters scenes based on clear confidence % and stanard quality category
meta.df.final <-  meta.df[meta.df$clear_confidence_percent >94.999 & meta.df$quality_category =='standard',]
