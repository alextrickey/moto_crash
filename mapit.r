
#Pulling Lat/Long from Google Maps
#http://stackoverflow.com/questions/32504880
geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lat,
             x$results[[1]]$geometry$location$lng)
  } else {
    out <- c(NA, NA)
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}
