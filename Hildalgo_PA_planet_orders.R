library(httr)
library(jsonlite)
library(tidyverse)
library(glue)
library(curl)
library(purrr)
library(progress)
library(sf)
library(lubridate)
library(magrittr)
library(dplyr)
library(exiftoolr) ##reads in EXIF metadata from downloaded Planet image files. optional if using Delivery Option #2.

##create a file called 'secrets.R' and in it define a character
##object containing your Planet API key
source('secrets.R') 

product.name <- '' ##give your order a name. this will be followed by a date range later.

##read in a .geojson or .shp file containing your AOI boundary. must be in CRS WGS84 (EPSG 4326)
extent <- fromJSON(aoi,
                   simplifyVector = F) %>%
  .$features %>%
  .[[1]] %>%
  .$geometry


extent.filter <- list(type='GeometryFilter',
                      field_name='geometry',
                      config = extent) %>% 
  jsonlite::toJSON(auto_unbox = T)

##specify the date range. Example: date ending as 2024-05-01 will query scenes until 00:00:00 hour at the latest, so the query actually ends on 2024-04-30
dates <- c('2024-04-01','2024-05-01') %>% 
  ymd() %>% 
  as_datetime() %>% 
  format_ISO8601(usetz = 'Z') %>% 
  set_names(c('gte','lte')) %>% 
  as.list()

##QUERYING####

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
);search.results$status_code ##displays status code. If code is in the 200s it is good, if it is in the 400 it is an errors. If it error'd,


search.results.c <- content(search.results); search.results.c[[2]] %>% length() #results cap at 250, aim to get it less than 250 by constraining the date range in object dates

##ORDERING####

products <- c('analytic_8b_sr_udm2','analytic_sr_udm2') #8-band or 4-band
products.json <- purrr::map_chr(search.results.c$features,
                                'id') %>% 
  list(item_ids=.,item_type='PSScene',
       product_bundle=products[1]) %>% #surface reflectance, 8 band. For 4-band or other products, visit https://developers.planet.com/apis/orders/product-bundles-reference/
  toJSON(auto_unbox = T,pretty = T)

##names your order, the date range will be generated accordingly
product.order.name <- paste0(
  product.name, #change according to your AOI
  dates$gte %>% as.character() %>% str_remove('T00:00:00Z') %>% str_remove_all('-'),
  '-',
  dates$lte %>% as.character() %>% str_remove('T00:00:00Z') %>% str_remove_all('-'))

# DELIVERY OPTION 1 - USE IF ZIPPED DELIVERY IS DESIRED####

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
  ],
   "delivery":{
  "archive_type":"zip",
  "single_archive":true,
  "archive_filename":"{{name}}.zip"
   }}'

# DELIVERY OPTION 2 - USE IF DOWNLOADING BY SINGLE FILES IS DESIRED.####
# product.order.template <- '{
#   "name":"${product.order.name}$",
#   "source_type":"scenes",
#   "products":[
#     ${products.json}$
#   ],
#   "tools":[
#     {
#       "clip":{
#         "aoi":${toJSON(extent,auto_unbox=T)}$
#       }
#     }
#   ]
# }
# '
'

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

##DELIVERY OPTION 1 - DOWNLOADING ZIPPED FILES FROM A LIST OF ORDERS---------------------------------------------------

auth = authenticate(planet.api.key, "")

download_root = "" 
get_all_orders = function() {
  orders = list()
  next_url = "https://api.planet.com/compute/ops/orders/v2?page_size=100"
  while (!is.null(next_url)) {
    resp = GET(next_url, auth)
    stop_for_status(resp)
    content_data = content(resp, as = "parsed", type = "application/json")
    page_orders = content_data$orders
    clean_page = map_dfr(page_orders, function(order) {
      tibble(
        id = order$id,
        name = order$name,
        state = order$state,
        created = order$created,
        updated = order$last_modified,
        items_count = length(order$products),
        url = order$`_links`$self
      )
    })
    orders = bind_rows(orders, clean_page)
    message(glue("Fetched {nrow(clean_page)} orders from page. Total so far: {nrow(orders)}"))
    next_url = content_data$`_links`$`_next`
  }
  return(orders)
}


#check the authentications work with one curl ID--------------------------------


test_order_id = "a54969d1-a906-4b68-88c0-663234b2fa58"

order_url = paste0("https://api.planet.com/compute/ops/orders/v2/", test_order_id)
resp = GET(order_url, authenticate(planet.api.key, "", type = "basic"))
stop_for_status(resp)
order_json = content(resp, "text", encoding = "UTF-8")
order_data = fromJSON(order_json, flatten = TRUE)

# Check results URLs
file_urls = order_data$`_links`$results$location
print(file_urls)
file_url = file_urls[1]

curl::curl_download(
  url = file_url,
  destfile = "testfile.zip",
  handle = curl::new_handle(httpauth = 1, userpwd = paste0(planet.api.key, ":"))
)


# Begin processing orders in batch----------------------------------------------

setwd(download_root)

#call order names
all_orders = get_all_orders()
dim(all_orders) #check to make sure it's not jus tthe first 20

#set progress bar
pb = progress_bar$new(
  total = nrow(all_orders),
  format = "  downloading [:bar] :percent eta: :eta"
)

for(i in all_orders$id){
  pb$tick()
  order_url = paste0("https://api.planet.com/compute/ops/orders/v2/", i)
  resp = GET(order_url, authenticate(planet.api.key, "", type = "basic"))
  stop_for_status(resp)
  order_json = content(resp, "text", encoding = "UTF-8")
  order_data = fromJSON(order_json, flatten = TRUE)
  id = all_orders[c(1:2),] %>%
    dplyr::filter(id == i) %>%
    pull(name)
  #set name of folders to order name place in org script
  fn = paname = fn = paste0(name, ".zip")
  # Check results URLs
  file_urls = order_data$`_links`$results$location
  print(file_urls)
  file_url = file_urls[1]
  #Download quietly without printing http information (not working) 
  #TODO: fix for quiet download
  suppressMessages(
    suppressWarnings(
      curl::curl_download(
        url = file_url,
        destfile = fn,
        handle = curl::new_handle(httpauth = 1, userpwd = paste0(planet.api.key, ":"))
      )
    )
  )
}

##DELIVERY OPTION 2 - START DOWNLOADING FILES FROM A SINGLE order ID####
downloads <- purrr::imap(
  content(order.status)[['_links']][['results']],
  ~{
    dest = file.path(paste0(
      'C:\\PSA\\Remote Sensing Team\\Projects\\Planet Orders\\Downloads\\',#change to where you want the images to download
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


