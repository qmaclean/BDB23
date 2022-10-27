

library(caret)
library(rsample)

#xBlock or xHurry

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

pff<-bdb_load_pff_data() %>%
  dplyr::select(-pff_role,-pff_positionLinedUp,-pff_beatenByDefender,
                -pff_hitAllowed,-pff_hurryAllowed,-pff_sackAllowed,-pff_nflIdBlockedPlayer,
                -pff_backFieldBlock,-pff_BlockedPlayerLinedUpPosition,-pff_blockType) %>%
  dplyr::mutate(pressure = rowSums(.[4:6],na.rm = T)) %>%
  dplyr::select(-pff_hit,-pff_hurry,-pff_sack)

df<-df %>%
  dplyr::left_join(pff,by=c("gameId" = "gameId",
                            "playId" = "playId",
                            "nflId" = "nflId"))


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
  group_by(Blocked) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n))

#### imbalanced target


############ split
### Tackler prediction model
vars<-df[,c(1:59)]
pred<-df %>% select(Blocked)

data_model<-cbind(vars,pred)

data_model<-na.omit(data_model)


nrow(data_model) / nrow(df)

data_model<-data_model %>%
  mutate(
         Blocked = ifelse(Blocked == "Yes","A","B"),
         Blocked = as.factor(Blocked),
         pff_positionLinedUp = as.factor(pff_positionLinedUp),
         offenseFormation = as.factor(offenseFormation),
         personnelO = as.factor(personnelO),
         personnelD = as.factor(personnelD),
         pff_passCoverage = as.factor(pff_passCoverage),
         pff_passCoverageType = as.factor(pff_passCoverageType)
         ) %>%
  dplyr::select(-sum_snap_dis_from_los,-pff_positionLinedUp,
                -yardsToGo,-absoluteYardlineNumber,-offenseFormation,
                -personnelO,-defendersInBox,-personnelD,-pff_passCoverage,-pff_passCoverageType,
                -cross_los_bool,-sum_dis,
                -left_side_sep_from_snap,-left_side_sep_from_dis,-left_side_dis_time,
                -right_side_sep_from_snap,-right_side_sep_from_dis,-right_side_dis_time,
                -forward_sep_from_dis,-forward_sep_from_snap,
                -backward_sep_from_snap,-backward_sep_from_dis,-backward_sep_dis_time,
                -player_dir_diff,-down,
                -dis_los_x_max,-frame_cross_los)


#split <- initial_split(data_model,group = week)

training_set<- data_model %>%
  dplyr::filter(gameId >= summary(data_model$gameId)[2]) %>%
  select(-gameId,-playId,-nflId,-week)

training_set_ids<- data_model %>%
  dplyr::filter(gameId >= summary(data_model$gameId)[2]) %>%
  select(gameId,playId,nflId,week)


#training_set_ids <- training(split) %>% select(gameId, playId, nflId)


testing_set<- data_model %>%
  dplyr::filter(gameId < summary(data_model$gameId)[2]) %>%
  select(-gameId,-playId,-nflId,-week)

testing_set_ids <- data_model %>%
  dplyr::filter(gameId < summary(data_model$gameId)[2]) %>%
  select(gameId,playId,nflId,week)

#testing_set <- testing(split) %>% select(-gameId, -playId, -nflId)
#testing_set_ids <- testing(split) %>% select(gameId, playId, nflId)

nrow(training_set) / nrow(data_model)


summary(data_model)

glm<-glm(Blocked ~ .,data=training_set,family=binomial())

summary(glm)

testing_set$test_pred<-predict(glm,testing_set,type="response")

testing_set %>%
  ggplot(aes(x=test_pred)) +
  geom_density()

testing_set$test_pred<-ifelse(testing_set$test_pred > 0.25,"B","A")

table(testing_set$test_pred,testing_set$Blocked)

postResample(testing_set$test_pred,testing_set$Blocked)

###### suspect it is good at identifying a block vs. a non blocked

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE)

### base model
gbm<-caret::train(Blocked ~ .,
                   data=training_set,
                   method="gbm",
                   verbose = TRUE,
                   metric = "ROC",
                   trControl = fitControl)

testing_set$test_predgbm<-predict(gbm,testing_set,type="raw")

postResample(testing_set$test_predgbm,testing_set$Blocked)

confusionMatrix(testing_set$Blocked, testing_set$test_predgbm,mode="everything")


summary(gbm)
#### might need to training_set & testing set for data leakage


check<-as.data.frame(training_set_ids$gameId %in% testing_set_ids$gameId) %>%
  rename(check = 1) %>%
  dplyr::filter(check == TRUE)

check<-as.data.frame(testing_set_ids$gameId %in% training_set_ids$gameId) %>%
  rename(check = 1) %>%
  dplyr::filter(check == TRUE)


### model comparison
rf<-caret::train(Blocked ~ .,
                  data=training_set,
                  method="ranger",
                  verbose = TRUE,
                  metric = "ROC",
                  trControl = fitControl)

testing_set$test_predrf<-predict(rf,testing_set,type="raw")

postResample(testing_set$test_predrf,testing_set$Blocked)

confusionMatrix(testing_set$Blocked, testing_set$test_predrf,mode="everything")

eb_model_list <- list(
                           GBM = gbm,
                           RF = rf)


eb_resamples <- resamples(eb_model_list)
summary(eb_resamples)
bwplot(eb_resamples)


saveRDS(rf,"expected_block_model_rf.rds")


### grid search 


