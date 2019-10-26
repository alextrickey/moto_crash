
#Load and Start H2O
require(h2o)
h2o.init(nthreads=-1, max_mem_size = "8G")

########################
## Load and Prep Data ##
########################

#Load Data into H2O Frame
moto_dat <- h2o.importFile("processed_data/moto_dat_imputed_NA.csv",
                           destination_frame = 'moto_dat')
#Alternative: "processed_data/moto_dat_dropped_NA.csv"

#Split Samples
splits <- h2o.splitFrame(
                data = moto_dat,
                ratios = c(0.7, 0.15),
                destination_frames = c("train.hex", "valid.hex", "test.hex"),
                seed = 314)
train <- splits[[1]] #70%
valid <- splits[[2]] #15%
test  <- splits[[3]] #15%

#Identify Features
outcomes = c("mc_killed","mc_injured","collision_severity","severe")
features_glm = setdiff(names(moto_dat),
                       c('CASE_ID','hours_since_midnight',outcomes))
features = setdiff(names(moto_dat),
                       c('CASE_ID','rush_hour','late_night',outcomes))


#######################
## Out-of-Box Models ##
#######################

#(Almost) Default GLM
glm_baseline <- h2o.glm(x = features_glm,
                   y = "severe",
                   training_frame = train,
                   model_id = "glm_baseline",
                   nfolds = 5,
                   lambda_search = TRUE,
                   family = "binomial")
glm_perf <- h2o.performance(model = glm_baseline,newdata = valid)
glm_perf
#Mean Per-Class Error:  0.3226529
#AUC:  0.7294094

#Default Random Forest
rf_baseline <- h2o.randomForest(x = features,
                            y = 'severe',
                            training_frame = train,
                            model_id = "rf_baseline",
                            seed = 1067)
rf_perf1 <- h2o.performance(model = rf_baseline,newdata = valid)
rf_perf1
#Mean Per-Class Error:  0.3337867
#AUC:  0.7132602


#Default GBM
gbm_baseline <- h2o.gbm(x = features,
                   y = "severe",
                   training_frame = train,
                   model_id = "gbm_baseline",
                   seed = 1067)
gbm_perf1 <- h2o.performance(model = gbm_baseline,newdata = valid)
gbm_perf1
#Mean Per-Class Error:  0.3219454
#AUC:  0.7314944

#############
## Try DRF ##
#############

drf_params <- list(max_depth = seq(5, 15, 1),
                   min_rows = 1:10,
                   ntrees = seq(10,150,10),
                   balance_classes = TRUE,
                   class_sampling_factors = list(
                     c(1,1),
                     #c(0.4,1),c(0.6,1),c(0.8,1),#under sample
                     c(1,1.2),c(1,1.4),c(1,1.6)#over sample
                   ))
search_criteria <- list(strategy = "RandomDiscrete",
                        max_runtime_secs = 360,
                        #early stopping
                        stopping_rounds = 10,
                        stopping_metric = "auc",
                        stopping_tolerance = 0.001)

drf_grid <- h2o.grid("randomForest", x = features, y = "severe",
                     grid_id = "drf_grid",
                     training_frame = train,
                     validation_frame = valid,
                     seed = 307,
                     hyper_params = drf_params,
                     search_criteria = search_criteria)

drf_gridperf_auc <- h2o.getGrid(grid_id = "drf_grid",
                            sort_by = "auc",
                            decreasing = TRUE)
drf_gridperf_auc


# Look at "best" model
tuned_drf_id <- drf_gridperf_auc@model_ids[[1]]
tuned_drf <- h2o.getModel(tuned_drf_id)
tuned_drf_perf_valid <- h2o.performance(model = tuned_drf,
                                        newdata = valid)
tuned_drf_perf_valid
#Mean Per-Class Error:  0.320215
#AUC:  0.7352626

#############
## Try GBM ##
#############

gbm_params <- list(learn_rate = seq(0.01, 0.1, 0.01),
                   max_depth = seq(2, 10, 1),
                   ntrees = seq(10,150,10),
                   balance_classes = TRUE,
                   class_sampling_factors = list(
                        c(1,1),
                        #c(0.4,1),c(0.6,1),c(0.8,1),#under sample
                        c(1,1.2),c(1,1.4),c(1,1.6)#over sample
                   ))
search_criteria <- list(strategy = "RandomDiscrete",
                        max_runtime_secs = 360)

gbm_grid <- h2o.grid("gbm", x = features, y = "severe",
                     grid_id = "gbm_grid",
                     training_frame = train,
                     validation_frame = valid,
                     #used for early stopping:
                     score_tree_interval = 5,
                     stopping_rounds = 3,
                     stopping_metric = "AUC",
                     stopping_tolerance = 0.0005,
                     seed = 307,
                     hyper_params = gbm_params,
                     search_criteria = search_criteria)

gbm_gridperf <- h2o.getGrid(grid_id = "gbm_grid",
                            sort_by = "auc",
                            decreasing = TRUE)
gbm_gridperf
#Mean Per-Class Error:  0.3124102
#AUC:  0.7412931

# Look at "best" model
tuned_gbm_id <- gbm_gridperf@model_ids[[1]]
tuned_gbm <- h2o.getModel(tuned_gbm_id)
tuned_gbm_perf_valid <- h2o.performance(model = tuned_gbm,
                                  newdata = valid)
tuned_gbm_perf_valid

##########
## Test ##
##########

#Look at performance in Test Set
tuned_gbm_perf_test <- h2o.performance(model = tuned_gbm,
                                        newdata = test)
tuned_gbm_perf_test
#Mean Per-Class Error:  0.3157915
#AUC:  0.7566492
#gbm_grid_model_549


####################
## Visualizations ##
####################
require(dplyr)
require(ggplot2)

# Plot GLM Coefficients
glm_coef = h2o.varimp(glm_baseline)
glm_coef_df = data.frame(glm_coef$names,glm_coef$coefficients,glm_coef$sign)
names(glm_coef_df) <-c('Variable.Level','Coefficient','Sign')
glm_coef_df = glm_coef_df[complete.cases(glm_coef_df),]
glm_coef_df <- transform(glm_coef_df, Variable.Level = reorder(Variable.Level,
                                                        Coefficient))

p = ggplot(glm_coef_df[1:20,],aes(Variable.Level,Coefficient,fill=Sign))
p + geom_bar(stat = "identity") + coord_flip() + theme_minimal()


# Plot Variable Importances
gbm_imp = h2o.varimp(tuned_gbm)
gbm_imp_df = data.frame(gbm_imp$variable,gbm_imp$scaled_importance)
names(gbm_imp_df) <-c('Variable','Scaled_Importance')
gbm_imp_df <- transform(gbm_imp_df, Variable = reorder(Variable, Scaled_Importance))

p = ggplot(gbm_imp_df,aes(Variable,Scaled_Importance))
p + geom_bar(stat = "identity") + coord_flip() + theme_minimal()

# Accident Severity by Day of Week and Time of Day?
moto_dat_df = tbl_df(as.data.frame(moto_dat))

#MV Involved With
p = ggplot(moto_dat_df,aes(mviw,fill=severe))
p + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Violation Category
p = ggplot(moto_dat_df,aes(pcf_viol_category,fill=severe))
p + geom_bar()

#Type of Collision
p = ggplot(moto_dat_df,aes(type_of_collision,fill=severe))
p + geom_bar() + coord_flip()

#Highway
p = ggplot(moto_dat_df,aes(state_hwy_ind,fill=severe))
p + geom_bar()

#Day and Time
moto_dat_df$day_of_week = factor(as.character(moto_dat_df$day_of_week),
                                    levels=c('M','Tu','W','Th','F','Sa','Su'))
p = ggplot(moto_dat_df,aes(hours_since_midnight,fill=severe))
p + geom_histogram(binwidth=1) + facet_grid(day_of_week~state_hwy_ind)
