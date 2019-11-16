
#Load and Start H2O
require(h2o)
h2o.init(nthreads = -1, max_mem_size = "8G")

########################
## Load and Prep Data ##
########################

#Load Data into H2O Frame
moto_dat <- h2o.importFile("processed_data/moto_dat_imputed_NA.csv",
                           destination_frame = "moto_dat")
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
outcomes <- c("mc_killed", "mc_injured", "collision_severity", "severe")
features_glm <- setdiff(names(moto_dat),
                       c("CASE_ID", "time_of_day", outcomes))
features <- setdiff(names(moto_dat),
                       c("CASE_ID", "rush_hour", "late_night", outcomes))


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
glm_perf <- h2o.performance(model = glm_baseline, newdata = valid)
glm_perf
#Mean Per-Class Error:  0.3366404
#AUC:  0.7294094


#Default Random Forest
rf_baseline <- h2o.randomForest(x = features,
                            y = "severe",
                            training_frame = train,
                            model_id = "rf_baseline",
                            seed = 1067)
rf_perf1 <- h2o.performance(model = rf_baseline, newdata = valid)
rf_perf1
#Mean Per-Class Error:  0.3179449
#AUC:  0.7137967


#Default GBM
gbm_baseline <- h2o.gbm(x = features,
                   y = "severe",
                   training_frame = train,
                   model_id = "gbm_baseline",
                   seed = 1067)
gbm_perf1 <- h2o.performance(model = gbm_baseline, newdata = valid)
gbm_perf1
#Mean Per-Class Error:  0.3106437
#AUC:  0.7334899


#############
## Try DRF ##
#############

drf_params <- list(max_depth = seq(5, 15, 1),
                   min_rows = 1:10,
                   ntrees = seq(10, 150, 10),
                   balance_classes = TRUE,
                   class_sampling_factors = list(
                     c(1, 1), c(1, 1.2), c(1, 1.4), c(1, 1.6) #over sample
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
#Mean Per-Class Error:  0.3146081
#AUC:  0.732125


#############
## Try GBM ##
#############

gbm_params <- list(learn_rate = seq(0.01, 0.1, 0.01),
                   max_depth = seq(2, 10, 1),
                   ntrees = seq(10, 150, 10),
                   balance_classes = TRUE,
                   class_sampling_factors = list(
                        c(1, 1),
                        c(1, 1.2), c(1, 1.4), c(1, 1.6)#over sample
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
#Mean Per-Class Error:  0.3170021
#AUC:  0.7400444

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
glm_perf_test <- h2o.performance(model = glm_baseline, newdata = test)
glm_perf_test

tuned_gbm_perf_test <- h2o.performance(model = tuned_gbm,
                                        newdata = test)
tuned_gbm_perf_test
#Mean Per-Class Error:  0.3369814
#AUC:  0.7584865
#gbm_grid_model_549


####################
## Visualizations ##
####################
require(dplyr)
require(ggplot2)


# Plot GLM Coefficients
glm_coef <- h2o.coef(glm_baseline)
glm_coef <- data.frame(variable = names(glm_coef), coef = glm_coef)
glm_coef$sign <- as.factor(sign(glm_coef$coef))

glm_imp <- h2o.varimp(glm_baseline)
glm_imp <- transform(glm_imp, variable = reorder(variable, scaled_importance))
glm_imp <- merge(glm_imp, glm_coef, by = "variable", all.x = TRUE)

p <- ggplot(glm_imp[order(-glm_imp$scaled_importance), ][1:20, ],
           aes(variable, scaled_importance, fill = sign))
p + geom_bar(stat = "identity") + coord_flip() + theme_minimal()


# Plot Variable Importances
gbm_imp <- h2o.varimp(tuned_gbm)
gbm_imp <- transform(gbm_imp, variable = reorder(variable, scaled_importance))
p <- ggplot(gbm_imp, aes(variable, scaled_importance))
p + geom_bar(stat = "identity") + coord_flip() + theme_minimal()


# Accident Severity
moto_dat_df <- tbl_df(as.data.frame(moto_dat))

#MV Involved With
p <- ggplot(moto_dat_df, aes(mviw, fill = severe))
p + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Violation Category
p <- ggplot(moto_dat_df, aes(pcf_viol_category, fill = severe))
p + geom_bar() + coord_flip() + theme_minimal()

#Type of Collision
p <- ggplot(moto_dat_df, aes(type_of_collision, fill = severe))
p + geom_bar() + coord_flip()

#Highway
p <- ggplot(moto_dat_df, aes(state_hwy_ind, fill = severe))
p + geom_bar()

#Day and Time
moto_dat_df$day_of_week <- factor(as.character(moto_dat_df$day_of_week),
                              levels = c("M", "Tu", "W", "Th", "F", "Sa", "Su"))
p <- ggplot(moto_dat_df, aes(time_of_day, fill = severe))
p + geom_histogram(binwidth = 1) + facet_grid(day_of_week ~ state_hwy_ind)
