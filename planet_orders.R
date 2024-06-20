library(httr)
library(jsonlite)
library(sf)
library(lubridate)
library(magrittr)
library(dplyr)
library(exiftoolr) #reads in EXIF metadata from downloaded Planet image files

##create a file called 'secrets.R' and in it define a character
##object containing your Planet API key
source('secrets.R') 

##read in a .geojson or .shp file containing your AOI boundary
raw.extent <- read_sf('C:\\PSA\\Remote Sensing Team\\PA\\Cumberland\\cbp_cumberland_lulc_classes_81_82_87_88_manual_extent.geojson') %>% 
  st_geometry() %>% 
  st_union() %>% 
  write_sf('cumberland_flattened.geojson')

##read in file output from raw.extent object
extent <- fromJSON('C:\\Users\\Christopher.Hidalgo\\Downloads\\cumberland_flattened.geojson',
                   simplifyVector = F) %>% 
  .$features %>%
  .[[1]] %>% 
  .$geometry 


extent.filter <- list(type='GeometryFilter',
                      field_name='geometry',
                      config = extent) %>% 
  jsonlite::toJSON(auto_unbox = T)

##QUERYING####

##specify the date range
dates <- c('2024-04-21','2024-05-01') %>% 
  ymd() %>% 
  as_datetime() %>% 
  format_ISO8601(usetz = 'Z') %>% 
  set_names(c('gte','lte')) %>% 
  as.list()

date.filter <- list(type='DateRangeFilter',field_name='acquired',
                    config = dates) %>% 
  jsonlite::toJSON(auto_unbox = T)

##acceptable cloud cover range from 0-60%
data.search.template <- '{
  "item_types":["PSScene"],
  "filter":{
    "type":"AndFilter",
    "config":[
        ${date.filter}$,
        ${extent.filter}$,
        {
            "type":"RangeFilter",
            "config":{
               "gte":0,
               "lte":0.6
            },
            "field_name":"cloud_cover"
         }
    ]
  }
}'

request <-  glue::glue(data.search.template,
                       .open= '${',
                       .close = '}$')

search.results <-  POST(url='https://api.planet.com/data/v1/quick-search',
                        body = as.character(request),
                        authenticate(planet.api.key,
                                     ''), 
                        content_type_json()
)

search.results.c <- content(search.results)

##ORDERING####

products.json <- purrr::map_chr(search.results.c$features,
                                'id') %>% 
  list(item_ids=.,item_type='PSScene',
       product_bundle='analytic_8b_sr_udm2') %>% 
  toJSON(auto_unbox = T,pretty = T)

##name your order accordingly
product.order.name <- 'Cumberland_240421-30'

product.order.template <- '{
  "name":"${product.order.name}$",
  "source_type":"scenes",
  "products":[
    ${products.json}$
  ],
  "tools":[
    {
      "clip":{
        "aoi":${toJSON(extent,auto_unbox=T)}$
      }
    }
  ]
}
'

## paste into product.order.template if zip out is desired.
# "delivery":{
#   "archive_type":"zip",
#   "single_archive":true,
#   "archive_filename":"${product.order.name}$"
# },

order.request <- glue::glue(
  product.order.template,
  .open ='${',
  .close = '}$'
)

order.pending <- POST(url='https://api.planet.com/compute/ops/orders/v2',
                      body = as.character(order.request),
                      authenticate(planet.api.key,
                                   ''), 
                      content_type_json()
)

order.id <-  content(order.pending)[['id']]

order.status <- httr::GET(url='https://api.planet.com/compute/ops/orders/v2/7744bf3a-ca33-4cc0-b4ac-564a0f9275a5',
                          authenticate(planet.api.key,
                                       ''),
                          content_type_json()
)

content(order.status)[['last_message']]

# order.download <- httr::GET(url=content(order.status)[['_links']][['results']][[1]][['location']],
#                           authenticate(planet.api.key,
#                                        ''),
#                           content_type_json()
# )

##executes download
downloads <- purrr::imap(
  content(order.status)[['_links']][['results']],
  ~{
    dest = file.path('C:\\PSA\\Remote Sensing Team\\Projects\\Planet Orders',
                     basename(.x$name))
    if(file.exists(dest)){
      warning('File exists,skipping: ',basename(.x$name),
              immediate. = T,
              call. = F)
      return(NULL)}
    message(.y, ' ', basename(.x$name))
    httr::GET(url=.x$location,
              authenticate(planet.api.key,
                           ''),
              content_type_json(),
              write_disk(dest)
    )
  },.progress=T
)

##list all downloaded scenes
scenes <- list.files('C:\\PSA\\Remote Sensing Team\\Projects\\Planet Orders',
                     full.names = T)

##reads in all EXIF metadata.
scenes.exif <- exiftoolr::exif_read(scenes)

##checks file sizes 
scenes.exif$FileSize %>% summary()
