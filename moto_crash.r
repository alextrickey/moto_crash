
setwd('~/Desktop/moto_crash')

require(dplyr)
require(ggmap)
source('SWITRS_func_and_lookups.r')

# #Not In Use For Now.. needs work to link data
# #https://data.lacity.org/A-Livable-and-Sustainable-City/Road-Surface-Condition-Map/d9rz-k88a
# pci = tbl_df(read.csv('~/Desktop/moto_crash/road_conditions_2017.csv',
#                stringsAsFactors = F,
#                #quote = "\"",
#                header=T))
# 
# #Look at Variability of Ratings Within Streets
# pci %>% 
#   group_by(ST_NAME) %>% 
#   summarise(n_obs=n(), 
#             mean_pci = mean(PCI), 
#             min_pci=min(PCI), 
#             max_pci=max(PCI)) %>% 
#   arrange(desc(n_obs))

#SWITRS Bike, Pedestrian and Motorcycle Data
#Collision Table
collision = tbl_df(read.csv('~/Desktop/moto_crash/switrs/CollisionRecords.txt',
                     header=T, 
                     strip.white = T, 
                     na.strings = c("NA","","-"))) 
# #Not In Use For Now.. data quality issues
# #(At Fault) Party Table
# party = tbl_df(read.csv('~/Desktop/moto_crash/switrs/PartyRecords.txt',
#                             header=T, 
#                             strip.white = T, 
#                             na.strings = c("NA","","-"))) 
# #Victim Table
# victim = tbl_df(read.csv('~/Desktop/moto_crash/switrs/VictimRecords.txt',
#                         header=T, 
#                         strip.white = T, 
#                         na.strings = c("NA","","-"))) 


#Count Rows with NAs in Interesting Features
collision %>%
  filter(MOTORCYCLE_ACCIDENT == 'Y') %>%
  filter(is.na(INTERSECTION) |
           (is.na(WEATHER_1) & is.na(WEATHER_2)) | 
           (is.na(STATE_HWY_IND)) | 
           (is.na(TYPE_OF_COLLISION)) | 
           (is.na(MVIW)) | 
           (is.na(ROAD_COND_1) & is.na(ROAD_COND_2)) | 
           (is.na(LIGHTING))
  ) %>%
  summarise(rows_with_NAs = n())#344 out of 10533 (3.27%)

#Select Relevant Data
moto_dat = collision %>%
  filter(MOTORCYCLE_ACCIDENT == 'Y') %>%
  select(CASE_ID, COLLISION_DATE, ACCIDENT_YEAR, COLLISION_TIME, DAY_OF_WEEK,
         INTERSECTION, WEATHER_1, WEATHER_2, STATE_HWY_IND, 
         PCF_VIOL_CATEGORY, TYPE_OF_COLLISION, MVIW, ROAD_SURFACE,
         ROAD_COND_1, ROAD_COND_2, LIGHTING, CONTROL_DEVICE, #exclude?
         ALCOHOL_INVOLVED, TRUCK_ACCIDENT)

#Look at Primary Collision Factor (PCF) Categories
moto_dat %>% 
  mutate(vio = violation_lookup[PCF_VIOL_CATEGORY]) %>% 
  group_by(vio) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


#Some Quirky Transformations
moto_dat$COLLISION_DATE = sapply(moto_dat$COLLISION_DATE,date_format)
moto_dat$season = as.factor(sapply(moto_dat$COLLISION_DATE,getSeason))

#Some Less Quirky Transformations
moto_dat = moto_dat %>%
  #Calculate Some Time-Related Variables
  mutate(rush_hour_am = as.factor(
              (COLLISION_TIME >= 700 & COLLISION_TIME <= 900)), #7-9am
         rush_hour_pm = as.factor(
              (COLLISION_TIME >= 1500 & COLLISION_TIME <= 1900)), #3-7pm
         late_night = as.factor(
              (COLLISION_TIME >= 2300 | COLLISION_TIME <= 500)), #11pm-5am
         day_of_week = as.factor(day_lookup[DAY_OF_WEEK])
  ) %>%
  #Transform/Recode Weather Columns
  mutate(weather_clear  = merge_and_recode(WEATHER_1,WEATHER_2,'A'),
         weather_cloudy = merge_and_recode(WEATHER_1,WEATHER_2,'B'),
         weather_rainy  = merge_and_recode(WEATHER_1,WEATHER_2,'C'),
         weather_snowy  = merge_and_recode(WEATHER_1,WEATHER_2,'D'),
         weather_foggy  = merge_and_recode(WEATHER_1,WEATHER_2,'E'),
         weather_other  = merge_and_recode(WEATHER_1,WEATHER_2,'F'), 
         weather_windy  = merge_and_recode(WEATHER_1,WEATHER_2,'G')
  ) %>%
  #Transform Road, Highway Data
  mutate(
    intersection = as.factor(INTERSECTION),
    state_hwy_ind = as.factor(STATE_HWY_IND),
    road_surface = as.factor(
        road_surface_lookup[ROAD_SURFACE]), #recode: B,C,D = not dry
    lighting = as.factor(light_lookup[LIGHTING]),
    control_dev = as.factor(control_dev_lookup[CONTROL_DEVICE])
  ) %>%
  #Transform Road Condition
  mutate(rc_holes_ruts       = merge_and_recode(ROAD_COND_1,ROAD_COND_2,'A'), 
         rc_loose_material   = merge_and_recode(ROAD_COND_1,ROAD_COND_2,'B'),
         rc_obstruction      = merge_and_recode(ROAD_COND_1,ROAD_COND_2,'C'),
         rc_construction     = merge_and_recode(ROAD_COND_1,ROAD_COND_2,'D'),
         rc_reduced_width    = merge_and_recode(ROAD_COND_1,ROAD_COND_2,'E'),
         rc_flooding         = merge_and_recode(ROAD_COND_1,ROAD_COND_2,'F'), 
         rc_other_condition  = merge_and_recode(ROAD_COND_1,ROAD_COND_2,'G'),
         rc_normal_condition = merge_and_recode(ROAD_COND_1,ROAD_COND_2,'H')
  ) %>%  
  #Transform Collision Data
  mutate(
    pcf_viol_category = as.factor(violation_lookup[PCF_VIOL_CATEGORY]),
    type_of_collision = as.factor(collision_type_lookup[TYPE_OF_COLLISION]),
    mviw = as.factor(mviw_lookup[MVIW]), #motor vehicle involved with
    alcohol_involved = as.factor(na_as_no(ALCOHOL_INVOLVED)),
    truck_accident = as.factor(na_as_no(TRUCK_ACCIDENT))
  )

CHP_VEHTYPE_AT_FAULT (merge with STWD_VEHTYPE_AT_FAULT?)
mutate(cross_street = paste(PRIMARY_RD,'and',SECONDARY_RD,'Los Angeles'))
  select(
    #Time Related
    COLLISION_YEAR, season, rush_hour_am, rush_hour_pm, late_night, day_of_week,
    



#geocode(moto_dat$cross_street)




