
require(dplyr)
require(ggmap)


#https://data.lacity.org/A-Livable-and-Sustainable-City/Road-Surface-Condition-Map/d9rz-k88a
pci = tbl_df(read.csv('~/Desktop/moto_crash/road_conditions_2017.csv',
               stringsAsFactors = F,
               #quote = "\"",
               header=T))

#Look at Variability of Ratings Within Streets
pci %>% 
  group_by(ST_NAME) %>% 
  summarise(n_obs=n(), 
            mean_pci = mean(PCI), 
            min_pci=min(PCI), 
            max_pci=max(PCI)) %>% 
  arrange(desc(n_obs))

#SWITRS Bike, Pedestrian and Motorcycle Collision Data
collision = tbl_df(read.csv('~/Desktop/moto_crash/switrs/CollisionRecords.txt',
                     header=T, 
                     strip.white = T, 
                     na.strings = c("NA","","-"))) 

#Process/Select Bike Data
moto_col = collision %>%
  filter(MOTORCYCLE_ACCIDENT == 'Y') %>%
  mutate(cross_street = paste(PRIMARY_RD,'and',SECONDARY_RD,'Los Angeles'))


#geocode(moto_col$cross_street)

glimpse(moto_col)



# Format Dates
date_format <- function(date) {
  if (is.na(date)) date = NA
  else date = as.Date(as.character(date),'%Y%m%d')
  date
}
moto_col$PROC_DATE = sapply(moto_col$PROC_DATE,date_format)
moto_col$COLLISION_DATE = sapply(moto_col$COLLISION_DATE,date_format)



# Get Lat and Long

glimpse(moto_col)
summary(moto_col)

"
#Variables to Select
CASE_ID, #?
COLLISION_DATE, 
# Pull Out Season (Temperature)
COLLISION_TIME, 
#mutate?? rush_hour_am (7am-9am),
#         rush_hour_pm (3pm-7pm),
#         night_traffic (11pm-5am)
DAY_OF_THE_WEEK,
#as.factor
DIRECTION, SIDE_OF_HWY,
#both have many nas
INTERSECTION,
#as.factor
WEATHER_1,WEATHER_2 
#transform
STATE_HIGHWAY_INDICATOR
#4 NAs fill in manually?
PCF_VIOL_CATEGORY
TYPE_OF_COLLISION
#134 NULLs
MVIW #motor vehicle involved with
ROAD_SURFACE
#recode ... B, C, D = not dry
ROAD_COND_1, ROAD_COND_2
#transform, possible merge with road surface
"

