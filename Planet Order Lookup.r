library(httr);library(magrittr);source('secrets.R') 

#The objective of this code is to retrieve the order ID and its associated order name.
#This is useful for when the need to download a specific file from the Planet
#GUI arises, especially if the account has hundreds/thousands of orders in its history.
#When you find the desired order ID, simply append it to the URL 
#https://www.planet.com/account/#/orders/

# Set up your API key
api_key <- '' #you may set api_key up in your secrets.R file.
url <- 'https://api.planet.com/compute/ops/orders/v2'

# Function to fetch orders
fetch_orders <- function(url, api_key) {
  response <- GET(url, add_headers(Authorization = paste('api-key', api_key)))

  if (status_code(response) != 200) {
    stop(paste("Failed to retrieve orders:", status_code(response)))
  }
  
  content(response, "parsed")
}

# Fetch and print all orders

url_next <- url
order.list = data.frame()
repeat {
  orders <- fetch_orders(url_next, api_key)
  
  # Debugging: Print the orders fetched
  print(orders)
for (i in 1:length(orders$orders)) {
  print(paste("Order ID:", orders$orders[[i]]$id))
  order.id = orders$orders[[i]]$id
  order.name = orders$orders[[i]]$name
  df = data.frame(order.id,order.name)
  order.list = rbind(order.list,df)
}
  # Check if there's a next page
  if (!is.null(orders$`_links`$`next`)) {
    url_next <- orders$`_links`$`next`
    Sys.sleep(1) #delay for 60s
  } else {
    break
  }
}

View(order.list)
