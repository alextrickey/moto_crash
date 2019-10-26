require(dplyr)
require(missForest)
source('SWITRS_func_and_lookups.r')

#SWITRS -- Collision Data
#http://iswitrs.chp.ca.gov/Reports/jsp/userLogin.jsp
#   Data Requested:
#     Bike, Pedestrian and Motorcycle Data
#     Dates Jan 2012 - Feb 2017
#Collision Table
collision = tbl_df(read.csv('switrs/CollisionRecords.txt',
                     header=TRUE,
                     strip.white = TRUE,
                     na.strings = c("NA","","-")))

# #SWITRS -- Collision Party Data
# party = tbl_df(read.csv('switrs/PartyRecords.txt',
#                             header=T,
#                             strip.white = T,
#                             na.strings = c("NA","","-")))
#
# #SWITRS -- Collision Victim Data
# victim = tbl_df(read.csv('switrs/VictimRecords.txt',
#                         header=T,
#                         strip.white = T,
#                         na.strings = c("NA","","-")))
#
# #LA Public Works -- Road Surface Condition
# #https://data.lacity.org/A-Livable-and-Sustainable-City/Road-Surface-Condition-Map/d9rz-k88a
# pci = tbl_df(read.csv('lacity/road_conditions_2017.csv',
#                stringsAsFactors = F,
#                #quote = "\"",
#                header=T))

#Select Relevant Data
moto_dat = collision %>%
  filter(MOTORCYCLE_ACCIDENT == 'Y') %>%
  select(
         #unique id for collision
         CASE_ID,
         #outcomes
         COUNT_MC_KILLED, COUNT_MC_INJURED, COLLISION_SEVERITY,
         #when
         COLLISION_DATE, ACCIDENT_YEAR, COLLISION_TIME, DAY_OF_WEEK,
         #where
         INTERSECTION, STATE_HWY_IND, PRIMARY_RD, SECONDARY_RD,
         #environment/conditions
         WEATHER_1, WEATHER_2, ROAD_SURFACE, ROAD_COND_1, ROAD_COND_2, LIGHTING,
         #accident/collision details
         PCF_VIOL_CATEGORY, HIT_AND_RUN, TYPE_OF_COLLISION, MVIW,
         ALCOHOL_INVOLVED, TRUCK_ACCIDENT,
         #covariates (impact collision-level severity measure)
         PEDESTRIAN_ACCIDENT, BICYCLE_ACCIDENT
         )

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
  mutate(accident_year = ACCIDENT_YEAR,
         day_of_week = as.factor(day_lookup[DAY_OF_WEEK]),
         rush_hour = as.factor(
              #check for morning rush hour
              ifelse(
                  (COLLISION_TIME >= 700 & COLLISION_TIME <= 900 #7-9am
                  & day_of_week %in% c('M','Tu','W','Th','F')
                  ), 'AM',
              #check for evening rush hour
              ifelse(
                  (COLLISION_TIME >= 1500 & COLLISION_TIME <= 1900 #3-7pm
                  & day_of_week %in% c('M','Tu','W','Th','F')
                  ), 'PM','N'))
              ),
         late_night = as.factor(ifelse(
              (COLLISION_TIME >= 2300 | COLLISION_TIME <= 500), #11pm-5am
              'Y','N')),
         hours_since_midnight = floor(COLLISION_TIME/100) #hours
                                + (COLLISION_TIME %% 100)/60 #minutes (decimal)
  ) %>%
  #Transform/Recode Weather Columns [not mutually exclusive]
  mutate(w_clear  = as.factor(merge_and_recode(WEATHER_1,WEATHER_2,'A')),
         w_cloudy = as.factor(merge_and_recode(WEATHER_1,WEATHER_2,'B')),
         w_rainy  = as.factor(merge_and_recode(WEATHER_1,WEATHER_2,'C')),
         #Merge infrequent weather types [snowy, foggy, other, windy]
         w_other  = as.factor(merge_and_recode(
                      WEATHER_1,WEATHER_2,codes =c('D','E','F','G')))
  ) %>%
  #Transform Road, Highway Data
  mutate(
    intersection = as.factor(INTERSECTION),
    state_hwy_ind = as.factor(STATE_HWY_IND),
    road_not_dry = as.factor(
        ifelse(road_surface_lookup[ROAD_SURFACE] %in%
            c("Slippery (Muddy, Oily, etc.)","Snowy or Icy","Wet"),
            'Y','N')),
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
    truck_accident = as.factor(na_as_no(TRUCK_ACCIDENT)),
    pedestrian_accident = as.factor(na_as_no(PEDESTRIAN_ACCIDENT)),
    bicycle_accident = as.factor(na_as_no(BICYCLE_ACCIDENT))
  ) %>%
  #Convert primary and secondary roads to cross streets
  #(later lookup geocodes with: geocode(moto_dat$cross_street)
  mutate(
    cross_street = paste(PRIMARY_RD,'and',SECONDARY_RD,'Los Angeles')
  ) %>%
  select(
    #unique id for collision
    CASE_ID,
    #outcomes
    mc_killed, mc_injured, collision_severity, severe,
    #when
    accident_year, hours_since_midnight,
    season, rush_hour, late_night, day_of_week,
    #where
    intersection, state_hwy_ind, #cross_streets,
    #environment/conditions
    starts_with('w_'), #weather variables
    road_not_dry, road_condition_issue, lighting,
    #accident/collision details
    type_of_collision, pcf_viol_category, mviw, alcohol_involved,
    truck_accident,
    #covariates (impact collision-level severity measure)
    pedestrian_accident, bicycle_accident
  )

#View(moto_dat)
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
moto_dat.imp = missForest(as.data.frame(moto_dat[,-1]),variablewise = TRUE)

#Out of Bag Error for Imputed Columns
data.frame('total_NAs' = apply(sapply(moto_dat[,-1],is.na),2,sum),
           'OOB_Error'=moto_dat.imp$OOBerror) #MSE/PFC

#Save Dataset Variations
write.csv(moto_dat,
          file = "processed_data/moto_dat_with_NA.csv",
          row.names = FALSE)
write.csv(cbind(moto_dat[,1],moto_dat.imp$ximp),
          file = "processed_data/moto_dat_imputed_NA.csv",
          row.names = FALSE)
write.csv(na.omit(moto_dat),
          file = "processed_data/moto_dat_dropped_NA.csv",
          row.names = FALSE)
