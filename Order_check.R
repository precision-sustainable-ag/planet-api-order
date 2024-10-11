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
