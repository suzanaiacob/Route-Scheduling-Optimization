routes <- read.csv("~/Documents/MIT Code/Analytics Lab/routes.csv")

routes$fullAdress =  paste(routes$Address,routes$City,
                           routes$State)

locations = as.data.frame(routes$fullAdress)
names(locations) = c("address")
locations$address = as.character(locations$address)
locations = as.data.frame(locations, stringsAsFactor = FALSE)


places = geocode(locations$address)
routes$lat = places$lat
routes$lon = places$lon

min(routes$lon)
max(routes$lon)

min(routes$lat)
max(routes$lat)

boston_coords <- c(left   = -71.61, 
                   bottom = 41.84, 
                   right  = -70.65, 
                   top    = 42.6)
basemap <- get_map(location = boston_coords,
                   maptype = 'terrain')

route_2_1 = routes %>% filter(Route==2 & (Stop==28 | Stop == 6))
route_2_2 = routes %>% filter(Route==2 & (Stop==6 | Stop == 40))
route_2_3 = routes %>% filter(Route==2 & (Stop==40 | Stop == 12))

route_7 = routes %>% filter(Route==7)

ggmap(basemap) + 
  geom_point(aes(x = lon, y = lat), 
             data = routes, 
             size = 2,
             col="steelblue")


write.csv(routes, "routes_coord.csv")
  
  
# current route processing
september_routes =  read.csv("2019-07 to 2019-09/2019-09_RouteVisits.csv")
model_stops =  read.csv("narrow_stops2.csv")



old_routes$routeStartDateTime
str(old_routes$routeStartDateTime)

routes$AddressCode = routes2$`Address Code`[match(routes$Address, routes2$Address)]


old_routes = september_routes %>% filter(addressCode %in% routes$AddressCode)
old_routes$scheduledPickUpTime = as.POSIXct(old_routes$scheduledPickUpTime ,format="%a, %b %d,%Y %I:%M %p",tz=Sys.timezone())

old_routes$pickuptime = as.POSIXct(old_routes$pickuptime ,format="%a, %b %d,%Y %I:%M %p",tz=Sys.timezone())

old_routes$date = as.Date(old_routes$pickuptime)
old_routes$time <- format(old_routes$pickuptime,"%H:%M:%S")

old_routes$RtKindCode = NULL
old_routes$shiftactive = NULL
old_routes$RSR = NULL
old_routes$orderKindCode = NULL
old_routes$taskKindCode = NULL
old_routes$orderNumber1 = NULL
old_routes$contactName1 = NULL
old_routes$clientactive = NULL
old_routes$ServiceLevel = NULL

write.csv(old_routes, "old_routes2.csv")

