

library(caret)
library(rsample)
library(tidyverse)
library(arrow)
library(parallel)
library(mlbench)
library(doParallel)
#xBlock or xHurry

cluster<-makeCluster(detectCores() - 1)
registerDoParallel(cluster)


df<-read_parquet("final_df.parquet")



source("R/bdb_load_pff_scouting_data.R")

pff<-bdb_load_pff_data() %>%
  dplyr::select(-pff_role,-pff_positionLinedUp,-pff_beatenByDefender,
                -pff_hitAllowed,-pff_hurryAllowed,-pff_sackAllowed,-pff_nflIdBlockedPlayer,
                -pff_backFieldBlock,-pff_BlockedPlayerLinedUpPosition,-pff_blockType) %>%
  dplyr::mutate(pressure = rowSums(.[4:6],na.rm = T)) %>%
  dplyr::select(-pff_hit,-pff_hurry,-pff_sack)



df<-df %>%
  dplyr::left_join(pff,by=c("gameId" = "gameId",
                            "playId" = "playId",
                            "nflId" = "nflId")) %>%
  dplyr::select(-week,-assigned,-Blocked)


###### expected block
df<-df %>%
  dplyr::mutate(
    left_side_sep_from_snap = ifelse(is.na(left_side_sep_from_snap),0,left_side_sep_from_snap),
    right_side_sep_from_snap = ifelse(is.na(right_side_sep_from_snap),0,right_side_sep_from_snap),
    forward_sep_from_snap = ifelse(is.na(forward_sep_from_snap),0,forward_sep_from_snap),
    forward_sep_dis_time = ifelse(is.na(forward_sep_dis_time),0,forward_sep_dis_time),
    backward_sep_from_snap = ifelse(is.na(backward_sep_from_snap),0,backward_sep_from_snap),
    closest_down_edge_separation = ifelse(is.na(closest_down_edge_separation),0,closest_down_edge_separation),
    closest_up_edge_separation = ifelse(is.na(closest_up_edge_separation),0,closest_up_edge_separation),
    closest_dt_separation = ifelse(is.na(closest_dt_separation),0,closest_dt_separation),
    snap_dis_from_los = ifelse(is.na(snap_dis_from_los),0,snap_dis_from_los),
    separation_from_los_x = ifelse(is.na(separation_from_los_x),0,separation_from_los_x),
    
  ) %>%
  dplyr::group_by(gameId,playId,nflId) %>%
  dplyr::mutate(
    dis_los_x_max = ifelse(is.na(dis_los_x_max),0,dis_los_x_max),
    frame_cross_los = ifelse(is.na(frame_cross_los),0,frame_cross_los),
    cross_los_bool = ifelse(is.na(cross_los_bool),0,cross_los_bool)
  ) %>%
  ungroup()

df %>%
  group_by(pressure) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))

#### imbalanced target


############ split
### Tackler prediction model
vars<-df[,c(1:58)]
pred<-df %>% select(pressure)

data_model<-cbind(vars,pred)

data_model<-na.omit(data_model)


nrow(data_model) / nrow(df)

data_model<-data_model %>%
  mutate(
    pressure = as.factor(pressure),
    pressure = ifelse(pressure == "1","A","B"),
    pff_positionLinedUp = as.factor(pff_positionLinedUp),
    offenseFormation = as.factor(offenseFormation),
    personnelO = as.factor(personnelO),
    personnelD = as.factor(personnelD),
    pff_passCoverage = as.factor(pff_passCoverage),
    pff_passCoverageType = as.factor(pff_passCoverageType)
  ) %>%
  dplyr::select(-pff_passCoverage,-pff_passCoverageType,-personnelD,-personnelO,-offenseFormation,
                -pff_positionLinedUp,-sum_snap_dis_from_los,-dis_los_x_max,
                -backward_sep_dis_time,-forward_sep_dis_time,-right_side_dis_time,
                -left_side_dis_time,-pre_play_dis,-player_dir_diff,-closest_dt_separation,
                -closest_down_edge_separation,-closest_up_edge_separation,-absoluteYardlineNumber,
                -closest_pass_block_player_dir_diff,
                -LT_pct,-RT_pct,-LG_pct,-RG_pct,-C_pct,-down,-yardsToGo,
                -backward_sep_from_dis,-left_side_sep_from_dis,-right_side_sep_from_dis,-forward_sep_from_dis,
                -separation_from_snap_position,-closest_pass_block_guard_separation,-closest_pass_block_tackle_separation) 


#split <- initial_split(data_model,group = week)

training_set<- data_model %>%
  dplyr::filter(gameId >= summary(data_model$gameId)[2]) %>%
  select(-gameId,-playId,-nflId)

training_set_ids<- data_model %>%
  dplyr::filter(gameId >= summary(data_model$gameId)[2]) %>%
  select(gameId,playId,nflId)


#training_set_ids <- training(split) %>% select(gameId, playId, nflId)


testing_set<- data_model %>%
  dplyr::filter(gameId < summary(data_model$gameId)[2]) %>%
  select(-gameId,-playId,-nflId)

testing_set_ids <- data_model %>%
  dplyr::filter(gameId < summary(data_model$gameId)[2]) %>%
  select(gameId,playId,nflId)

#testing_set <- testing(split) %>% select(-gameId, -playId, -nflId)
#testing_set_ids <- testing(split) %>% select(gameId, playId, nflId)

nrow(training_set) / nrow(data_model)


summary(data_model)

glm<-glm(as.factor(pressure) ~ .,data=training_set,family=binomial())

summary(glm)

imp<-varImp(glm)

testing_set$test_pred<-predict(glm,testing_set,type="response")

testing_set %>%
  ggplot(aes(x=test_pred)) +
  geom_density()

testing_set$test_pred<-ifelse(testing_set$test_pred < 0.75,"A","B")
testing_set<-testing_set %>%
  mutate(test_pred = as.factor(test_pred),
         pressure = as.factor(pressure))

table(testing_set$test_pred,testing_set$pressure)

postResample(testing_set$test_pred,testing_set$pressure)

confusionMatrix(testing_set$pressure, testing_set$test_pred,mode="everything")

###### suspect it is good at identifying a block vs. a non blocked

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE)

training_set<-training_set %>%
  mutate(
         pressure = as.factor(pressure))

testing_set<-testing_set %>%
  mutate(
         pressure = as.factor(pressure))



##### add class weights 
# class weights 
k <- 0.5
classWeights <- ifelse(training_set$pressure == "A",
                       (1/table(training_set$pressure)[1]) * k,
                       (1/table(training_set$pressure)[2]) * (1-k))


gbm_wt<-caret::train(as.factor(pressure) ~ .,
                  data=training_set,
                  method="gbm",
                  verbose = TRUE,
                  metric = "ROC",
                  trControl = fitControl,
                  weights = classWeights)

testing_set$test_predgbm_wt<-predict(gbm_wt,testing_set,type="raw")

postResample(testing_set$test_predgbm_wt,testing_set$pressure)

confusionMatrix(testing_set$pressure, testing_set$test_predgbm_wt,mode="everything")

saveRDS(gbm_wt,"ep_gbm_wt_model.rds")


### down sampling
#fitControl <- trainControl(## 10-fold CV
#  method = "repeatedcv",
#  number = 10,
#  repeats = 5,
#  summaryFunction = twoClassSummary,
#  classProbs = TRUE,
#  sampling = "down")


#gbm_wt_down<-caret::train(as.factor(pressure) ~ .,
#                          data=training_set,
#                          method="gbm",
#                          verbose = TRUE,
#                          metric = "ROC",
#                          trControl = fitControl,
#                          weights = classWeights)

#testing_set$test_predgbm_wt_down<-predict(gbm_wt_down,testing_set,type="raw")

#postResample(testing_set$test_predgbm_wt_down,testing_set$pressure)

#confusionMatrix(testing_set$pressure, testing_set$test_predgbm_wt_down,mode="everything")


treeBag_wt<-caret::train(as.factor(pressure) ~ .,
                          data=training_set,
                          method="treebag",
                        verbose = TRUE,
                          metric = "ROC",
                          trControl = fitControl,
                          weights = classWeights)

testing_set$test_predtreebg<-predict(treeBag_wt,testing_set,type="raw")

postResample(testing_set$test_predtreebg,testing_set$pressure)

confusionMatrix(testing_set$pressure, testing_set$test_predtreebg,mode="everything")

saveRDS(treeBag_wt,"ep_treeBag_wt_model.rds")



svmRadial<-caret::train(as.factor(pressure) ~ .,
                         data=training_set,
                         method="svmRadial",
                         verbose = TRUE,
                         metric = "ROC",
                         trControl = fitControl,
                         weights = classWeights)

testing_set$test_svm<-predict(svmRadial,testing_set,type="raw")

postResample(testing_set$test_svm,testing_set$pressure)

confusionMatrix(testing_set$pressure, testing_set$test_svm,mode="everything")


#### bart

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 4,
  #repeats = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  allowParallel = TRUE)

ada<-caret::train(as.factor(pressure) ~ .,
                        data=training_set,
                        method="ada",
                        verbose = TRUE,
                        metric = "ROC",
                        trControl = fitControl,
                        weights = classWeights)







testing_set$test_ada<-predict(ada,testing_set,type="raw")

postResample(testing_set$test_ada,testing_set$pressure)

confusionMatrix(testing_set$pressure, testing_set$test_ada,mode="everything")

gbm_wt<-readRDS("ep_gbm_wt_model.rds")
treeBag_wt<-readRDS("ep_treeBag_wt_model.rds")

testing_set$test_gbm_wt<-predict(gbm_wt,testing_set,type="raw")

postResample(testing_set$test_gbm_wt,testing_set$pressure)

confusionMatrix(testing_set$pressure, testing_set$test_gbm_wt,mode="everything")


testing_set$test_treeBag<-predict(treeBag_wt,testing_set,type="raw")
postResample(testing_set$test_treeBag,testing_set$pressure)
confusionMatrix(testing_set$pressure, testing_set$test_treeBag,mode="everything")


treeBag_wt_sens<-caret::train(as.factor(pressure) ~ .,
                         data=training_set,
                         method="treebag",
                         verbose = TRUE,
                         metric = "Sens",
                         trControl = fitControl,
                         weights = classWeights)

testing_set$test_treeBag_sens<-predict(treeBag_wt_sens,testing_set,type="raw")
postResample(testing_set$test_treeBag_sens,testing_set$pressure)
confusionMatrix(testing_set$pressure, testing_set$test_treeBag_sens,mode="everything")


rf_sense<-caret::train(as.factor(pressure) ~ .,
                              data=training_set,
                              method="ranger",
                              verbose = TRUE,
                              metric = "Sens",
                              trControl = fitControl,
                              weights = classWeights)

testing_set$test_rf_sense<-predict(rf_sense,testing_set,type="raw")
postResample(testing_set$test_rf_sense,testing_set$pressure)
confusionMatrix(testing_set$pressure, testing_set$test_rf_sense,mode="everything")

saveRDS(rf_sense,"ep_random_forest_opt_sens_model.rds")


ep_model_list <- list(
  #GBM = gbm,
  #RF = rf,
  GBM_WT = gbm_wt,
  treeBag_wt,
  svm = svmRadial
  )


ep_resamples <- resamples(ep_model_list)
summary(ep_resamples)
bwplot(ep_resamples)

