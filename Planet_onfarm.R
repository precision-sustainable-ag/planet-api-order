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

## listing the sites where we could not download planet images initially
PSA_negate <- list.files("D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data\\Sites_8band") %>% 
  stringr::str_extract('_[A-Z]{3}') %>% stringr::str_remove('_') %>% na.omit()  

PSA_negate_4band <- list.files("D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data") %>% 
  stringr::str_extract('_[A-Z]{3}') %>% stringr::str_remove('_') %>% na.omit()

PSA_negate <- c(PSA_negate, PSA_negate_4band)

## directly importing polygon shape/geojson file from local drive

polygon_extent <- read_sf("D:\\Postdoc_work\\UMD\\API_query\\Biomass_polygon1.geojson") %>% 
  dplyr::filter(!code %in% c(PSA_negate)) #filter out the sites that we have data already

#checking date range
range(polygon_extent$cover_planting, na.rm = T) ###missing planting_date to address
range(polygon_extent$cc_harvest_date, na.rm = T)


# map the polygon using 'leaflet'package
library(leaflet)
leaflet() %>% 
  addTiles(urlTemplate = "https://tiles.stadiamaps.com/tiles/alidade_satellite/{z}/{x}/{y}{r}.jpg",
           attribution = '&copy; CNES, Distribution Airbus DS, © Airbus DS, © PlanetObserver (Contains Copernicus Data) | &copy; <a href="https://www.stadiamaps.com/" target="_blank">Stadia Maps</a> &copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors') %>% 
  addPolygons(data = st_geometry(polygon_extent))


#create the bounding boxes for polygon wise data query to Planet, buffer 50m

polygon_bboxes <- polygon_extent %>% group_by(code, cc_harvest_date, cover_planting) %>% 
  summarise(
    geometry=st_as_sfc(st_bbox(st_combine(geometry)))
  ) %>% 
  mutate(geometry=st_buffer(geometry,50)) %>% 
  mutate(geometry=st_as_sfc(st_bbox(geometry)))



## next steps is to write a function and call the extent and date chunks together from the polygon_extent
#  data frame and then use filter for each rows

make.geometry.filter <- function(geom){
  coords <- st_coordinates(geom)[,1:2] %>% 
    jsonlite::toJSON(pretty = TRUE)
  template <- '{
   "type":"GeometryFilter",
   "field_name":"geometry",
   "config":{
      "type":"Polygon",
      "coordinates":[
      ${coords}$
      
      ]
   }
  }
  '
  glue::glue(template,.open = '${', .close = '}$')
  
}

#check function
make.geometry.filter(polygon_extent$geometry[1])


## make a function for the dates
make.date.filter <- function(start,end){
  
  start <- as.character(start)
  end <- as.character(end)
  template <- '{
   "type":"DateRangeFilter",
   "field_name":"acquired",
   "config":{
      "gt":"${start}$T00:00:00Z",
      "lte":"${end}$T00:00:00Z"
   }
}'
  
  glue::glue(template,.open = '${', .close = '}$')
  
}


# make a function for ordering data using geometry and dates
make.order <- function(geometry, date){
  data.search.template <- '{
  "item_types":["PSScene"],
  "filter":{
    "type":"AndFilter",
    "config":[
        ${date}$,
        ${geometry}$,
        {
            "type":"RangeFilter",
            "config":{
               "gte":0,
               "lte":0.6
            },
            "field_name":"cloud_cover"
         },
        {
            "type": "StringInFilter",
            "field_name": "quality_category",
            "config": ["standard"]
       }
    ]
  }
}'
  
  glue::glue(data.search.template,
             .open= '${',
             .close = '}$')
}

#### for future order use the below template-----CHRIS edited----
##acceptable cloud cover range from 0-60%; only "standard" images vs. "test"
##the last part. this will ignore test images. standard is what we want since it passes their quality checks.

# data.search.template <- '{
#   "item_types":["PSScene"],
#   "filter":{
#     "type":"AndFilter",
#     "config":[
#         ${date.filter}$,
#         ${extent.filter}$,
#         {
#             "type":"RangeFilter",
#             "config":{
#                "gte":0,
#                "lte":0.6
#             },
#             "field_name":"cloud_cover"
#          },
# {"type": "StringInFilter",
#         "field_name": "quality_category",
#         "config": ["standard"]
#       }
#     ]
#   }
# }'
# 



polygon.orders <- polygon_bboxes %>%
  filter(!is.na(as.Date(cover_planting)), cover_planting!='') %>% 
  rowwise() %>% 
  mutate(geomfilter=make.geometry.filter(geometry),
         datefilter=make.date.filter(cover_planting, cc_harvest_date),
         order=make.order(geomfilter, datefilter))

# Check site codes
polygon.orders$code

# check date filter
make.date.filter(polygon_bboxes$cover_planting[1],polygon_bboxes$cc_harvest_date[1])

# check order detail
polygon.orders$order[1]

# bad date sites
bad_dates_sites <- polygon.orders %>% 
  filter(cover_planting > cc_harvest_date|is.na(cover_planting)|is.na(cc_harvest_date)) %>% 
  pull(code)
polygon.orders <- polygon.orders %>% filter(!(code %in% bad_dates_sites))

# Check site codes
polygon.orders$code

##QUERYING####
# order_request <- purrr::map(
#   polygon.orders$order[1],
#   ~ POST(url='https://api.planet.com/data/v1/quick-search',
#          body = as.character(.x),
#          authenticate(planet.api.key,
#                       ''), 
#          content_type_json()
#   )
# )
# 
# order_request_features <- purrr::map(
#   order_request,
#   ~content(.x)$features %>% 
#     purrr::map_chr('id')
# )

get_product_from_search <- function(order){
  search_request <- POST(url='https://api.planet.com/data/v1/quick-search',
                         body = as.character(order),
                         authenticate(planet.api.key,
                                      ''), 
                         content_type_json()
  )
  search_results <- content(search_request)
  
  search_ids <- search_results$features %>% 
    purrr::map_chr('id')
  
  while (!is.null(search_results[["_links"]][["_next"]])) {
    message("Getting next URL: ")
    search_request <- GET(url=search_results[["_links"]][["_next"]],
                          body = as.character(order),
                          authenticate(planet.api.key,
                                       ''), 
                          content_type_json()
    )
    search_results <- content(search_request)
    
    search_ids <- c(search_ids,search_results$features %>% 
                      purrr::map_chr('id'))
  }
  search_ids
}




order_list <- list()


for (rn in 1:nrow(polygon.orders)) {
  
  code <- polygon.orders$code[rn]
  message('code: ',code)
  order <- polygon.orders$order[rn]
  geomfilter <- polygon.orders$geomfilter[rn]
  
  search_id <- get_product_from_search(order)
  message("Ordering products: ", length(search_id))
  
  clip_extent <- geomfilter %>% 
    jsonlite::fromJSON() %>% 
    .$config %>% 
    jsonlite::toJSON(auto_unbox = T) 
  
  product.order.template <- '{
  "name":"${product.order.name}$",
  "source_type":"scenes",
  "products":[
    ${search_id.json}$
  ],
  "tools":[
    {
      "clip":{
        "aoi":${clip_extent}$
      }
    }
  ]
}'
  
##"product_bundle": "analytic_8b_sr_udm2"  # 8band imagery---previously used
##"product_bundle": "analytic_sr_udm2"  # 4band imagery---currently used
  items_template <- '{
  "item_ids": ["${.x}$"],
  "item_type": "PSScene",
  "product_bundle": "analytic_sr_udm2"
}'
  
  order_response <- purrr::map(
    search_id,
    ~{
      search_id.json=glue::glue(
        items_template,
        .open ='${',
        .close = '}$'
      )
      product.order.name <- paste0('4band_Onfarm_',code,'_',.x)
      message(product.order.name)
      order.request <- glue::glue(
        product.order.template,
        .open ='${',
        .close = '}$'
      )
      order.request
      POST(url='https://api.planet.com/compute/ops/orders/v2',
           body = as.character(order.request),
           authenticate(planet.api.key,
                        ''),
           content_type_json()
      )
    }
  )
  
  order_list[[rn]] <- order_response
  
}

saveRDS(order_list, 'order_list_4band.rds')

order_list <- readRDS('order_list_4band.rds')

purrr::map_depth(order_list, 2, httr::status_code) %>% 
  purrr::map(unlist) %>% 
  purrr::keep(~any(.x==202))

order_list[[1]][[1]]



get_order_status <- function(orders){
  purrr::map(
    orders,
    ~tibble(
      link = .x[['_links']][['_self']],
      id = .x$id,
      last_message = .x$last_message,
      name = .x$name
    )
  ) %>% 
    bind_rows()
}


get_all_orders <- function(){
  order_statuses <- RETRY("GET", url='https://api.planet.com/compute/ops/orders/v2',
                        authenticate(planet.api.key,
                                     ''))
  order_response <- content(order_statuses)
  orders <- data.frame()
  if(length(order_response$orders)){
    orders <- bind_rows(orders, get_order_status(order_response$orders))
  }
  while(!is.null(order_response[['_links']][['next']])){
    message(order_response[['_links']][['next']])
    order_statuses <- RETRY("GET", url=order_response[['_links']][['next']],
                          authenticate(planet.api.key,
                                       ''))
    order_response <- content(order_statuses)
    
    if(length(order_response$orders)){
      orders <- bind_rows(orders, get_order_status(order_response$orders))
    }
  }
  orders
}


od <- get_all_orders()

# check and filter NA in order names
od %>% filter(is.na(name))

to_be_downloaded <- od %>% filter(last_message=='Manifest delivery completed') %>% 
  filter(stringr::str_detect(name,pattern='4band_Onfarm_[A-Z]{3}')) %>% 
  mutate(folder_name=stringr::str_extract(name,pattern='4band_Onfarm_[A-Z]{3}')) %>% 
  filter(!is.na(folder_name))

# check the names of the order sites to be downloaded and if there is NA in names of folder
unique(stringr::str_extract(od$name,pattern='4band_Onfarm_[A-Z]{3}'))

library(ggplot2)
polygon.orders %>% 
  anti_join(to_be_downloaded %>% 
              mutate(code = stringr::str_extract(name,'[A-Z]{3}'))) %>% 
  select(-geomfilter,-datefilter, -order) %>% 
  ungroup() %>% 
  slice(1) %>%
  mutate(area= st_area(geometry)) %>% 
  ggplot()+geom_sf()



for (rn in 1:nrow(to_be_downloaded)) {
  products_request <- GET(url=to_be_downloaded$link[rn],
                          authenticate(planet.api.key,
                                       ''))
  
  valid <- httr::status_code(products_request)>=200 & httr::status_code(products_request)<=299
  
  if (!valid) {to_be_downloaded[rn,] %>% 
      #mutate(error=as.character(content(products_request))) %>% 
      readr::write_csv('download_error.csv', append = T)
  }
  
  if(valid){
    ##executes download
    downloads <- purrr::imap(
      content(products_request)[['_links']][['results']],
      ~{
        dir.create(file.path('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data',
                             to_be_downloaded$folder_name[rn]))
        
        dest = file.path('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data',
                         to_be_downloaded$folder_name[rn],
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
  }
}




##list all downloaded scenes
scenes <- list.files('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data',
                     full.names = T)

stringr::str_extract(scenes,'_[A-Z]{3}') %>% 
  stringr::str_remove('_')

bad_dates_sites


##reads in all EXIF metadata.
scenes.exif <- exiftoolr::exif_read(scenes)

##checks file sizes 
scenes.exif$FileSize %>% summary()
