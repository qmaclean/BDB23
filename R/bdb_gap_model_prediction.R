
library(tidyverse)
library(arrow)
library(caret)
library(nnet)
library(keras)
library(tensorflow)
library(shapr)
library(shapviz)
library(treeshap)
library(edarf)
library("DALEX")

#remotes::install_github("slundberg/shap")

#devtools::install_github("mayer79/shapviz")

#devtools::install_github('ModelOriented/treeshap')

#devtools::install_github("zmjones/edarf", subdir = "pkg")


source("R/bdb_load_pff_scouting_data.R")

wk1<-read_parquet("data_processed/wk_final/wk1_final.parquet")
wk2<-read_parquet("data_processed/wk_final/wk2_final.parquet")
wk3<-read_parquet("data_processed/wk_final/wk3_final.parquet")
wk4<-read_parquet("data_processed/wk_final/wk4_final.parquet")
wk5<-read_parquet("data_processed/wk_final/wk5_final.parquet")
wk6<-read_parquet("data_processed/wk_final/wk6_final.parquet")
wk7<-read_parquet("data_processed/wk_final/wk7_final.parquet")
wk8<-read_parquet("data_processed/wk_final/wk8_final.parquet")

df<-rbind(wk1,wk2,wk3,wk4,wk5,wk6,wk7,wk7,wk8)
rm(wk1,wk2,wk3,wk4,wk5,wk6,wk7,wk8)

##### traininign
train_labels<-read_csv("data/charted_data/manual_chart_data_train.csv")

train_labels<-train_labels %>%
  dplyr::filter(complete.cases(Gap))

train<-df %>%
  dplyr::inner_join(train_labels,by=c("gameId" = "gameId",
                                     "playId" = "playId",
                                     "nflId" = "nflId")) %>%
  dplyr::select(-pff_positionLinedUp.y,-rush_direction) %>%
  dplyr::rename(pff_positionLinedUp = pff_positionLinedUp.x) %>%
  dplyr::mutate(target_var = paste0(Gap,Hole),
         target_var = as.factor(target_var),
         pff_positionLinedUp = as.factor(pff_positionLinedUp),
         offenseFormation = as.factor(offenseFormation),
         personnelO = as.factor(personnelO)) %>%
  dplyr::select(-Gap,-Hole,-week,-pff_role,-jerseyNumber) %>%
  dplyr::mutate(
    left_side_sep_from_snap = ifelse(is.na(left_side_sep_from_snap),0,left_side_sep_from_snap),
    right_side_sep_from_snap = ifelse(is.na(right_side_sep_from_snap),0,right_side_sep_from_snap),
    forward_sep_from_snap = ifelse(is.na(forward_sep_from_snap),0,forward_sep_from_snap),
    forward_sep_dis_time = ifelse(is.na(forward_sep_dis_time),0,forward_sep_dis_time),
    backward_sep_from_snap = ifelse(is.na(backward_sep_from_snap),0,backward_sep_from_snap),
    closest_down_edge_separation = ifelse(is.na(closest_down_edge_separation),0,closest_down_edge_separation),
    closest_up_edge_separation = ifelse(is.na(closest_up_edge_separation),0,closest_up_edge_separation),
    closest_dt_separation = ifelse(is.na(closest_dt_separation),0,closest_dt_separation)
    
  ) %>%
  dplyr::group_by(gameId,playId,nflId) %>%
  dplyr::mutate(
    dis_los_x_max = ifelse(is.na(dis_los_x_max),0,dis_los_x_max),
    frame_cross_los = ifelse(is.na(frame_cross_los),0,frame_cross_los),
    cross_los_bool = ifelse(is.na(cross_los_bool),0,cross_los_bool)
  ) %>%
  ungroup()

assignments<-read.csv("data/charted_data/gap_assignments.csv") %>%
  dplyr::select(-n)

train<-train %>%
  dplyr::left_join(assignments,by=c(
    "pff_positionLinedUp" = "pff_positionLinedUp",
    "target_var" = "target_var"
  )) 
#%>%
#  mutate(target_var = paste0(target_var,assigned)) %>%
#  dplyr::select(-assigned)


#### may need to add a non-gap assignment?
 

train %>%
  group_by(target_var) %>%
  dplyr::summarize(n = n())

#assignments<-train %>%
#  group_by(pff_positionLinedUp,target_var) %>%
#  dplyr::summarize(n = n())

#write.csv(assignments,"gap_assignments.csv",row.names = F)


train_set<-train %>% dplyr::select(-gameId,-playId,-nflId,-pff_positionLinedUp,-pff_passCoverage,-pff_passCoverageType,
                                   -offenseFormation,-personnelO,-personnelD,-assigned,-down,-defendersInBox,
                                   -absoluteYardlineNumber,-yardsToGo,-snap_separation,-dis_los_x_max)
train_id<-train %>% dplyr::select(gameId,playId,nflId,pff_positionLinedUp,pff_passCoverage,pff_passCoverageType,
                                  offenseFormation,personnelO,personnelD,assigned,down,defendersInBox,
                                  absoluteYardlineNumber,yardsToGo,snap_separation,dis_los_x_max
                                  )

train_set$target_var<-factor(train_set$target_var)


##### test ########
test_labels<-read_csv("data/charted_data/manual_chart_data_test.csv")

test_labels<-test_labels %>%
  dplyr::filter(complete.cases(Gap))

test<-df %>%
  dplyr::inner_join(test_labels,by=c("gameId" = "gameId",
                                      "playId" = "playId",
                                      "nflId" = "nflId")) %>%
  dplyr::select(-pff_positionLinedUp.y,-rush_direction) %>%
  dplyr::rename(pff_positionLinedUp = pff_positionLinedUp.x) %>%
  dplyr::mutate(target_var = paste0(Gap,Hole),
                target_var = as.factor(target_var)) %>%
  dplyr::select(-Gap,-Hole,-week,-pff_role,-jerseyNumber) %>%
  dplyr::mutate(
    left_side_sep_from_snap = ifelse(is.na(left_side_sep_from_snap),0,left_side_sep_from_snap),
    right_side_sep_from_snap = ifelse(is.na(right_side_sep_from_snap),0,right_side_sep_from_snap),
    forward_sep_from_snap = ifelse(is.na(forward_sep_from_snap),0,forward_sep_from_snap),
    forward_sep_dis_time = ifelse(is.na(forward_sep_dis_time),0,forward_sep_dis_time),
    backward_sep_from_snap = ifelse(is.na(backward_sep_from_snap),0,backward_sep_from_snap),
    closest_down_edge_separation = ifelse(is.na(closest_down_edge_separation),0,closest_down_edge_separation),
    closest_up_edge_separation = ifelse(is.na(closest_up_edge_separation),0,closest_up_edge_separation),
    closest_dt_separation = ifelse(is.na(closest_dt_separation),0,closest_dt_separation)
    
  ) %>%
  dplyr::group_by(gameId,playId,nflId) %>%
  dplyr::mutate(
    dis_los_x_max = ifelse(is.na(dis_los_x_max),0,dis_los_x_max),
    frame_cross_los = ifelse(is.na(frame_cross_los),0,frame_cross_los),
    cross_los_bool = ifelse(is.na(cross_los_bool),0,cross_los_bool)
  ) %>%
  ungroup()

test<-test %>%
  dplyr::left_join(assignments,by=c(
    "pff_positionLinedUp" = "pff_positionLinedUp",
    "target_var" = "target_var"
  )) 
#%>%
#  mutate(target_var = paste0(target_var,assigned)) %>%
#  dplyr::select(-assigned)

test_set<-test %>% dplyr::select(-gameId,-playId,-nflId,-pff_positionLinedUp,-pff_passCoverage,-pff_passCoverageType,
                                 -offenseFormation,-personnelO,-personnelD,-assigned,-down,-defendersInBox,
                                 -absoluteYardlineNumber,-yardsToGo,-snap_separation,-dis_los_x_max)
test_id<-test %>% dplyr::select(gameId,playId,nflId,pff_positionLinedUp,pff_passCoverage,pff_passCoverageType,
                                offenseFormation,personnelO,personnelD,assigned,down,defendersInBox,
                                absoluteYardlineNumber,yardsToGo,snap_separation,dis_los_x_max)

test %>%
  group_by(target_var) %>%
  dplyr::summarize(n = n())

train %>%
  group_by(target_var) %>%
  dplyr::summarize(n = n())


test_set$target_var<-factor(test_set$target_var)



summary(train_set)
summary(test_set)

round(prop.table(table(train_set$target_var)), 2)


##################### GAP MODEL PREDICTION ###############################

#### base model
rpart_model = caret::train(target_var ~., data = train_set, method = "rpart2")

test_set$test_pred<-predict(rpart_model,test_set,type="raw")

#prediction <- predict(rpart_model, test_set[,-which(names(test_set)=="target_var")], type = "raw")

#postResample(pred=prediction,obs=test_set$target_var)

postResample(test_set$test_pred,test_set$target_var)

confusionMatrix(test_set$target_var, test_set$test_pred,mode="everything")

varImp<-varImp(rpart_model)$importance



table(test_set$test_pred,test_set$target_var)

#### multinomial n
nnet_model <- nnet::multinom(target_var ~., data = train_set)

test_set$test_predn<-predict(nnet_model,test_set)
postResample(pred=test_set$test_predn,obs=test_set$target_var)

netImp<-varImp(nnet_model)

confusionMatrix(test_set$target_var,test_set$test_predn,mode="everything")

### random forest
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  summaryFunction = multiClassSummary,
  classProbs = TRUE,
  allowParallel = TRUE)

rf_model<-caret::train(target_var ~ .,
          data = train_set,
          method = "ranger",
          trControl = fitControl,
          verbose = TRUE)


test_set$test_predrf<-predict(rf_model,test_set)
postResample(pred=test_set$test_predrf,obs=test_set$target_var)

confusionMatrix(test_set$target_var,test_set$test_predrf,mode="everything")

saveRDS(rf_model,"gap_rf_model.rds")

fitControl<-trainControl(
  method = "cv",
  #number = 10,
  #adaptive = list(min = 3,
  #                alpha = 0.05,
  #                method = "BT",
  #                complete = FALSE),
  search = "random",
  summaryFunction = multiClassSummary,
  classProbs = TRUE#,
  #allowParallel = TRUE
)


rf_opt_model<-caret::train(target_var ~ .,
                            data = train_set,
                            method = "ranger",
                            trControl = fitControl,
                            metric = "ROC",
                            #tuneGrid = gbmGrid,
                            verbose = TRUE)


rf_opt_model<-readRDS("model_objects/active/gap_rf_model_opt_final.rds")
test_set$test_predrf_opt<-predict(rf_opt_model,test_set)
postResample(pred=test_set$test_predrf_opt,obs=test_set$target_var)

confusionMatrix(test_set$target_var,test_set$test_predrf_opt,mode="everything")

#saveRDS(rf_opt_model,"gap_rf_model_opt_final.rds")




#### tune model 


##### gbm model
gbm_model<-caret::train(target_var ~ .,
                       data = train_set,
                       method = "gbm",
                       trControl = fitControl)


test_set$test_predgbm<-predict(gbm_model,test_set)
postResample(pred=test_set$test_predgbm,obs=test_set$target_var)

confusionMatrix(test_set$target_var,test_set$test_predgbm,mode="everything")


saveRDS(gbm_model,"gap_gbm_model.rds")

gbm_model$bestTune

gbmGrid <-  expand.grid(
                        n.trees = gbm_model$bestTune$n.trees,
                        n.minobsinnode = gbm_model$bestTune$n.minobsinnode,
                        interaction.depth = gbm_model$bestTune$interaction.depth,
                        shrinkage = gbm_model$bestTune$shrinkage)


###### Optimize model
set.seed(2020)
fitControl<-trainControl(
  method = "cv",
  #number = 10,
  #adaptive = list(min = 3,
  #                alpha = 0.05,
  #                method = "BT",
  #                complete = FALSE),
  search = "random",
  summaryFunction = multiClassSummary,
  classProbs = TRUE#,
  #allowParallel = TRUE
)


gbm_opt_model<-caret::train(target_var ~ .,
                            data = train_set,
                            method = "gbm",
                            trControl = fitControl,
                            metric = "ROC",
                           #tuneGrid = gbmGrid,
                           verbose = TRUE)


test_set$test_predgbm_opt<-predict(gbm_opt_model,test_set)
postResample(pred=test_set$test_predgbm_opt,obs=test_set$target_var)

confusionMatrix(test_set$target_var,test_set$test_predgbm,mode="everything")


##### Improve performance #####

##### NEURAL NETWORK ####
x_train_mat<-train_set %>%
  dplyr::select(-target_var) %>%
  as.matrix()

y_train_mat<-train_set %>%
  dplyr::select(target_var) %>%
  dplyr::mutate(target_var = as.numeric(
    case_when(
      target_var == "A1" ~ 0,
      target_var == "A2" ~ 1,
      target_var == "B3" ~ 2,
      target_var == "B4" ~ 3,
      target_var == "C5" ~ 4,
      target_var == "C6" ~ 5,
      target_var == "D7" ~ 6,
      target_var == "D8" ~ 7,
      TRUE ~ as.numeric(NA)
    )
  )
  )  %>%
  as.matrix()


callbacks<-list(
  callback_reduce_lr_on_plateau(
    monitor = "val_loss",
    factor = 0.1,
    patience = 10
  ),
  callback_early_stopping(
    monitor = "val_loss",
    patience = 20
  )
)



neural_net_model<-keras_model_sequential() 

neural_net_model %>%
  layer_dense(units = 32, activation = "relu",input_shape = ncol(x_train_mat)) %>% 
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 8, activation = "softmax")

neural_net_model %>%
  compile(
    optimizer = "adam",
    loss = "sparse_categorical_crossentropy",
    metrics = "accuracy"
  )

#neural_net_model %>%
#  layer_dense(units = 32,input_shape = ncol(x_train_mat),activation = 'relu') %>%
#  layer_dense(units = 16,activation = 'relu') %>%
#  layer_dense(units = 8,activation = 'softmax')

#neural_net_model() %>%
#  compile(optimizer = 'adam')

neural_net_model %>%
  fit(x_train_mat,
      y_train_mat,
      epochs = 40,
      batch_size = 2 ^ 3,
      validation_split = 0.2,
      callbacks = callbacks
      )


input_tensor<-layer_input(shape = ncol(x_train_mat))

output_tensor<-input_tensor %>%
  layer_dense(units = 32,activation = "relu") %>%
  layer_dense(units = 8,activation = "softmax") 

model<- keras_model(inputs = input_tensor,outputs = output_tensor)

model %>%
  compile(
    optimizer = 'rmsprop',
    loss = "sparse_categorical_crossentropy",
    metrics = c("accuracy")
  )

model %>%
  fit(x_train_mat,
      y_train_mat,
      batch_size = 2 ^ 3,
      epochs = 40,
      validation_split = 0.2
  )




