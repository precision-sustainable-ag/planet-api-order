folder <- list.dirs('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data', full.names = F, recursive = F)
sub_folder <- list.dirs('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data\\Sites_8band', full.names = F, recursive = T)

setdiff(folder, sub_folder)
intersect(folder, sub_folder)
file.path('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data',
          intersect(folder, sub_folder)) %>%
  unlink(recursive = TRUE)

