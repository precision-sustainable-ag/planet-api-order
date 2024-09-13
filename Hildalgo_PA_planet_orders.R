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
raw.extent <- read_sf('C:\\PSA\\Remote Sensing Team\\PA\\Cumberland\\cbp_cumberland_lulc_classes_81_82_87_88_manual_extent.geojson') %>% #choose the AOI file, must not have too many vertices.
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

##acceptable cloud cover range from 0-60%; only "standard" images vs. "test"
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
         },
{"type": "StringInFilter",
        "field_name": "quality_category",
        "config": ["standard"]
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

search.results.c <- content(search.results) #results cap at 250, aim to get it less than 250 by constraining the date range in object dates

##ORDERING####

products <- c('analytic_8b_sr_udm2','analytic_sr_udm2') #8-band or 4-band
products.json <- purrr::map_chr(search.results.c$features,
                                'id') %>% 
  list(item_ids=.,item_type='PSScene',
       product_bundle=products[1]) %>% #surface reflectance, 8 band. For 4-band or other products, visit https://developers.planet.com/apis/orders/product-bundles-reference/
  toJSON(auto_unbox = T,pretty = T)

##names your order, the date range will be generated accordingly
product.order.name <- paste0(
  'Cumberland ', #change according to your AOI
  dates$gte %>% as.character() %>% str_remove('T00:00:00Z') %>% str_remove_all('-'),
  '-',
  dates$lte %>% as.character() %>% str_remove('T00:00:00Z') %>% str_remove_all('-'))

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

#ONLY EXECUTE order.pending ONCE!!!!!!! Comment this out between orders.
order.pending <- POST(url='https://api.planet.com/compute/ops/orders/v2',
                      body = as.character(order.request),
                      authenticate(planet.api.key,
                                   ''), 
                      content_type_json()
)

order.id <-  content(order.pending)[['id']]

order.status <- httr::GET(url=
                            paste0('https://api.planet.com/compute/ops/orders/v2/',
                                   'order.id'), #change back to order.id if referencing a specific order
                          authenticate(planet.api.key,
                                       ''),
                          content_type_json()
)

content(order.status)[['last_message']] #if code is not in the 200s, something is wrong. recheck your objects.

# order.download <- httr::GET(url=content(order.status)[['_links']][['results']][[1]][['location']],
#                           authenticate(planet.api.key,
#                                        ''),
#                           content_type_json()
# )

##executes download
downloads <- purrr::imap(
  content(order.status)[['_links']][['results']],
  ~{
    dest = file.path(paste0(
      'C:\\PSA\\Remote Sensing Team\\Projects\\Planet Orders\\', #change to where you want the images to download
      product.order.name),#change back to product.order.name
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
scenes <- list.files(path=paste0(
  'C:\\PSA\\Remote Sensing Team\\Projects\\Planet Orders\\',#change to where you want the images to download
  product.order.name),
  pattern = '*8b_clip.tif',
  full.names = T)

##reads in all EXIF metadata.
scenes.exif <- exiftoolr::exif_read(scenes)

##checks file sizes 
scenes.exif$FileSize %>% summary()
