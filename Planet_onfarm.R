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



## directly importing polygon shape/geojson file from local drive
polygon_extent <- read_sf("D:\\Postdoc_work\\UMD\\API_query\\Biomass_polygon1.geojson")

# map the polygon using 'leaflet'package
library(leaflet)
leaflet() %>% 
  addTiles(urlTemplate = "https://tiles.stadiamaps.com/tiles/alidade_satellite/{z}/{x}/{y}{r}.jpg",
           attribution = '&copy; CNES, Distribution Airbus DS, © Airbus DS, © PlanetObserver (Contains Copernicus Data) | &copy; <a href="https://www.stadiamaps.com/" target="_blank">Stadia Maps</a> &copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> &copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors') %>% 
  addPolygons(data = st_geometry(polygon_extent))


#create the bounding boxes for polygon wise data query to Planet

polygon_bboxes <- polygon_extent %>% group_by(code, cc_harvest_date, cover_planting) %>% 
  summarise(geometry=st_as_sfc(st_bbox(st_combine(geometry))))



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
         }
    ]
  }
}'
  
  glue::glue(data.search.template,
             .open= '${',
             .close = '}$')
}



polygon.orders <- polygon_bboxes %>% rowwise() %>% 
  mutate(geomfilter=make.geometry.filter(geometry),
         datefilter=make.date.filter(cover_planting, cc_harvest_date),
         order=make.order(geomfilter, datefilter))

# check date filter
make.date.filter(polygon_bboxes$cover_planting[1],polygon_bboxes$cc_harvest_date[1])

# check order detail
polygon.orders$order[1]

# bad date sites
bad_dates_sites <- polygon.orders %>% 
  filter(cover_planting > cc_harvest_date|is.na(cover_planting)|is.na(cc_harvest_date)) %>% 
  pull(code)
polygon.orders <- polygon.orders %>% filter(!(code %in% bad_dates_sites))

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



order_list <- list()


for (rn in 1:nrow(polygon.orders)) {
  
  code <- polygon.orders$code[rn]
  message('code: ',code)
  order <- polygon.orders$order[rn]
  geomfilter <- polygon.orders$geomfilter[rn]
  
  search_request <- POST(url='https://api.planet.com/data/v1/quick-search',
                         body = as.character(order),
                         authenticate(planet.api.key,
                                      ''), 
                         content_type_json()
  )
  
  search_id.json <- content(search_request)$features %>% 
    purrr::map_chr('id')%>% 
    list(item_ids=.,item_type='PSScene',
         product_bundle='analytic_8b_sr_udm2') %>% 
    toJSON(auto_unbox = T,pretty = T)
  
  product.order.name <- paste0('Onfarm_',code) 
  
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
  
  order.request <- glue::glue(
    product.order.template,
    .open ='${',
    .close = '}$'
  )
  
  order_response <- POST(url='https://api.planet.com/compute/ops/orders/v2',
                         body = as.character(order.request),
                         authenticate(planet.api.key,
                                      ''), 
                         content_type_json()
  )
  order_list[[rn]] <- order_response
  
}


order_statuses <- GET(url='https://api.planet.com/compute/ops/orders/v2',
                      
                      authenticate(planet.api.key,
                                   ''))


order_stat <- GET(url='https://api.planet.com/compute/ops/stats/orders/v2',
                  
                  authenticate(planet.api.key,
                               ''))


X <- content(order_statuses)
Y <- content(order_stat)

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
  order_statuses <- GET(url='https://api.planet.com/compute/ops/orders/v2',
                        
                        authenticate(planet.api.key,
                                     ''))
  order_response <- content(order_statuses)
  orders <- data.frame()
  if(length(order_response$orders)){
    orders <- bind_rows(orders, get_order_status(order_response$orders))
  }
  while(!is.null(order_response[['_links']][['next']])){
    message(order_response[['_links']][['next']])
    order_statuses <- GET(url=order_response[['_links']][['next']],
                          
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

to_be_downloaded <- od %>% filter(last_message=='Manifest delivery completed')

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
  ##executes download
  downloads <- purrr::imap(
    content(products_request)[['_links']][['results']],
    ~{
      dir.create(file.path('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data',
                           to_be_downloaded$name[rn]))
      
      dest = file.path('D:\\Postdoc_work\\UMD\\API_query\\Planet\\Onfarm_planet_data',
                       to_be_downloaded$name[rn],
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
