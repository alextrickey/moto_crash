
setwd('~/Desktop/moto_crash')

require(dplyr)
require(missForest)
#require(ggmap)
source('SWITRS_func_and_lookups.r')

#SWITRS Bike, Pedestrian and Motorcycle Data
#Collision Table
collision = tbl_df(read.csv('~/Desktop/moto_crash/switrs/CollisionRecords.txt',
                     header=T, 
                     strip.white = T, 
                     na.strings = c("NA","","-"))) 

# ## Additional data sources not currently in use
# ##    (Further work needed to clean/link the datasets)
#
# #SWITRS Party Table - (Contains records for victims as well)
# party = tbl_df(read.csv('~/Desktop/moto_crash/switrs/PartyRecords.txt',
#                             header=T, 
#                             strip.white = T, 
#                             na.strings = c("NA","","-"))) 
# 
# #SWITRS Victim Table 
# victim = tbl_df(read.csv('~/Desktop/moto_crash/switrs/VictimRecords.txt',
#                         header=T, 
#                         strip.white = T, 
#                         na.strings = c("NA","","-"))) 
#
# # Road Quality Data
# #https://data.lacity.org/A-Livable-and-Sustainable-City/Road-Surface-Condition-Map/d9rz-k88a
# pci = tbl_df(read.csv('~/Desktop/moto_crash/road_conditions_2017.csv',
#                stringsAsFactors = F,
#                #quote = "\"",
#                header=T))

#Select Relevant Data
moto_dat = collision %>%
  filter(MOTORCYCLE_ACCIDENT == 'Y') %>%
  select(CASE_ID, COUNT_MC_KILLED, COUNT_MC_INJURED, COLLISION_SEVERITY,
         COLLISION_DATE, ACCIDENT_YEAR, COLLISION_TIME, DAY_OF_WEEK,
         INTERSECTION, WEATHER_1, WEATHER_2, STATE_HWY_IND, 
         PCF_VIOL_CATEGORY, TYPE_OF_COLLISION, MVIW, ROAD_SURFACE,
         ROAD_COND_1, ROAD_COND_2, LIGHTING, CONTROL_DEVICE, #exclude?
         ALCOHOL_INVOLVED, TRUCK_ACCIDENT, PRIMARY_RD, SECONDARY_RD)
         #Add later? -- CHP_VEHTYPE_AT_FAULT / STWD_VEHTYPE_AT_FAULT

#Look at Primary Collision Factor (PCF) Categories
moto_dat %>% 
  mutate(vio = violation_lookup[PCF_VIOL_CATEGORY]) %>% 
  group_by(vio) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))


#Some Quirky Transformations
moto_dat$collision_date = sapply(moto_dat$COLLISION_DATE,date_format)
moto_dat$season = as.factor(sapply(moto_dat$COLLISION_DATE,getSeason))

#Some Less Quirky Transformations
moto_dat = 
  moto_dat %>%
  #Potential DVs
  mutate(
        mc_killed = as.factor(ifelse(COUNT_MC_KILLED>0, 'Y', 'N')),
        mc_injured = as.factor(ifelse(COUNT_MC_INJURED>0, 'Y', 'N')),
        collision_severity = as.factor(
                  collision_severity_lookup[as.factor(COLLISION_SEVERITY)]),
        severe = as.factor(
                  ifelse(collision_severity %in% c('Fatal','Injury (Severe)'),
                         'Y','N'))
  ) %>%
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
  mutate(w_clear  = as.factor(merge_and_recode(WEATHER_1,WEATHER_2,'A')),
         w_cloudy = as.factor(merge_and_recode(WEATHER_1,WEATHER_2,'B')),
         w_rainy  = as.factor(merge_and_recode(WEATHER_1,WEATHER_2,'C'))
         #These are too infrequent... exclude
         #w_snowy  = as.factor(merge_and_recode(WEATHER_1,WEATHER_2,'D')),
         #w_foggy  = as.factor(merge_and_recode(WEATHER_1,WEATHER_2,'E')),
         #w_other  = as.factor(merge_and_recode(WEATHER_1,WEATHER_2,'F')), 
         #w_windy  = as.factor(merge_and_recode(WEATHER_1,WEATHER_2,'G'))
  ) %>%
  #Transform Road, Highway Data
  mutate(
    intersection = as.factor(INTERSECTION),
    state_hwy_ind = as.factor(STATE_HWY_IND),
    road_not_dry = as.factor(
        road_surface_lookup[ROAD_SURFACE] %in% 
          c("Slippery (Muddy, Oily, etc.)","Snowy or Icy","Wet")),
    lighting = as.factor(light_lookup[LIGHTING])
    #control_dev = as.factor(control_dev_lookup[CONTROL_DEVICE])
    #exclude - only 22 non-functioning, 9 obsured, otherwise working/irrelevant
  ) %>%
  #Transform Road Condition
  mutate(road_condition_issue = as.factor(
              #includes 
              #   holes & ruts (A), loose material (B), obstruction (C), 
              #   construction (D), reduced road width (E), 
              #   and "other" (G) issues 
              #excludes
              #   flooding (never occurred in data) -- code F
              #   normal condition (not informative) -- code H
              merge_and_recode(ROAD_COND_1,ROAD_COND_2,
                               codes =c('A','B','C','D','E','G')))
  ) %>%  
  #Transform Collision Data
  mutate(
    pcf_viol_category = as.factor(violation_lookup[PCF_VIOL_CATEGORY]),
    type_of_collision = as.factor(collision_type_lookup[TYPE_OF_COLLISION]),
    mviw = as.factor(mviw_lookup[MVIW]), #motor vehicle involved with
    alcohol_involved = as.factor(na_as_no(ALCOHOL_INVOLVED)),
    truck_accident = as.factor(na_as_no(TRUCK_ACCIDENT))
  ) %>%
  #Convert primary and secondary roads to cross streets 
  #(later lookup geocodes with: geocode(moto_dat$cross_street)
  mutate(
    cross_street = paste(PRIMARY_RD,'and',SECONDARY_RD,'Los Angeles')
  ) %>%
  select(
    #CASE_ID, cross_street,
    #DVs
    mc_killed, mc_injured, collision_severity, severe,
    #When:
    ACCIDENT_YEAR, season, rush_hour_am, rush_hour_pm, late_night, day_of_week,
    #Environment Conditions:
    starts_with('w_'), road_not_dry, road_condition_issue, lighting, 
    #Collision Details:
    intersection, state_hwy_ind, 
    type_of_collision, pcf_viol_category, mviw, alcohol_involved, 
    truck_accident
  )

View(moto_dat)
summary(moto_dat)

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
  summarise(rows_with_NAs = n())
#Affected Data: 
# 344 rows out of 10533 (3.27%)
# 9 columns with NAs (7 distinct)

#Use Random Forests to Impute Missing Values
set.seed(1047)
moto_dat.imp = missForest(as.data.frame(moto_dat),variablewise = TRUE)

#Out of Bag Error for Imputed Columns
moto_dat.imp$OOBerror

#Get Data
#moto_dat.imp$Ximp
