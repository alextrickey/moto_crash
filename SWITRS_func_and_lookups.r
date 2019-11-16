#SWITRS Functions and Lookups

# Function to Format Dates
date_format <- function(date) {
  if (is.na(date)) date <- NA
  else date <- as.Date(as.character(date), "%Y%m%d")
  date
}

# Function to Get the Season
# Source: http://stackoverflow.com/questions/9500114
get_season <- function(dates) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox

  # The year is arbitary, but set to match season dates
  d <- as.Date(as.character(dates), "%Y%m%d")
  d <- as.Date(strftime(d, format = "2012-%m-%d"))

  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

#Function to Recode Fields that Use Blanks to Indicate "No"
na_as_no <- function(yes_and_nas) {
  sapply(yes_and_nas, function (x) (if (is.na(x)) "N" else "Y"))
}

# Combine/Correct Variables using Two Fields with Same Code Set
#   e.g. weather_1 and weather_2 contain the same kind of codes
#   Split into one column for each code
merge_and_recode <- function(v1, v2, codes) {
  response <- rep(0, length(v1))
  for (i in 1:length(v1)) {
    if ( (!is.na(v1[i]) & v1[i] %in% codes) |
         (!is.na(v2[i]) & v2[i] %in% codes)) {
      response[i] <- "Y"
    } else if (is.na(v1[i]) & is.na(v2[i])) {
      response[i] <- NA
    } else {
      response[i] <- "N"
    }
  }
  response
}

#Lookup Tables
collision_severity_lookup <- c(
  "1" = "Fatal",
  "2" = "Injury (Severe)",
  "3" = "Injury (Other Visible)",
  "4" = "Injury (Complaint of Pain)",
  "0" = "PDO (property damage only)"
)

day_lookup <- c("1" = "M",
                "2" = "Tu",
                "3" = "W",
                "4" = "Th",
                "5" = "F",
                "6" = "Sa",
                "7" = "Su")

violation_lookup <- c(
  "01" = "Driving or Bicycling Under the Influence of Alcohol or Drug",
  "02" = "Impeding Traffic",
  "03" = "Unsafe Speed",
  "04" = "Miscellaneous", #"Following Too Closely" -- only 5 cases
  "05" = "Wrong Side of Road",
  "06" = "Improper Passing",
  "07" = "Unsafe Lane Change",
  "08" = "Improper Turning",
  "09" = "Automobile Right of Way",
  "10" = "Pedestrian Right of Way",
  "11" = "Pedestrian Violation",
  "12" = "Traffic Signals and Signs",
  "13" = "Hazardous Parking",
  "14" = "Lights",
  "15" = "Miscellaneous", #"Brakes" -- only 1 case
  "16" = "Miscellaneous", #"Other Equipment" -- only 1 case
  "17" = "Other Hazardous Violation",
  "18" = "Other Than Driver (or Pedestrian)",
  "19" = "Code 19",
  "20" = "Code 20",
  "21" = "Unsafe Starting or Backing",
  "22" = "Other Improper Driving",
  "23" = "Pedestrian or 'Other' Under the Influence of Alcohol or Drug",
  "24" = "Fell Asleep",
  "00" = "Unknown"
)

weather_lookup <- c(
  "weather_clear"  = "A",
  "weather_cloudy" = "B",
  "weather_rainy"  = "C",
  "weather_snowy"  = "D",
  "weather_foggy"  = "E",
  "weather_other"  = "F",
  "weather_windy"  = "G"
)

collision_type_lookup <- c(
  "A" = "Head-On",
  "B" = "Sideswipe",
  "C" = "Rear End",
  "D" = "Broadside",
  "E" = "Hit Object",
  "F" = "Overturned",
  "G" = "Vehicle/Pedestrian",
  "H" = "Other")

control_dev_lookup <- c(
  "A" = "Functioning",
  "B" = "Not Functioning",
  "C" = "Obscured",
  "D" = "None")

mviw_lookup <- c(
  "A" = "Non-Collision",
  "B" = "Pedestrian",
  "C" = "Other Motor Vehicle",
  "D" = "Motor Vehicle on Other Roadway",
  "E" = "Parked Motor Vehicle",
  "F" = "Train",
  "G" = "Bicycle",
  "H" = "Animal",
  "I" = "Fixed Object",
  "J" = "Other Object"
)

road_surface_lookup <- c(
  "A" = "Dry",
  "B" = "Wet",
  "C" = "Snowy or Icy",
  "D" = "Slippery (Muddy, Oily, etc.)"
)

light_lookup <- c(
  "A" = "Daylight",
  "B" = "Dusk or Dawn",
  "C" = "Dark - Street Lights",
  "D" = "Dark - No Street Lights",
  "E" = "Dark - No Street Lights"
  #E = "Dark - Street Lights Not Functioning" very rare
  #combining with code D
)
