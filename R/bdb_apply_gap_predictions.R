

library(tidyverse)
library(arrow)
library(caret)


wk1<-read_parquet("wk1_final.parquet")
wk2<-read_parquet("wk2_final.parquet")
wk3<-read_parquet("wk3_final.parquet")
wk4<-read_parquet("wk4_final.parquet")
wk5<-read_parquet("wk5_final.parquet")
wk6<-read_parquet("wk6_final.parquet")
wk7<-read_parquet("wk7_final.parquet")
wk8<-read_parquet("wk8_final.parquet")

df<-rbind(wk1,wk2,wk3,wk4,wk5,wk6,wk7,wk7,wk8)
rm(wk1,wk2,wk3,wk4,wk5,wk6,wk7,wk8)

#### exclude tagged data
#### import model
gap_model<-readRDS("gap_rf_model_opt_final.rds")
### apply predictions to rest of data


##### 
train_labels<-read_csv("data/charted_data/manual_chart_data_train.csv")
train_labels<-train_labels %>%
  dplyr::filter(complete.cases(Gap))
test_labels<-read_csv("data/charted_data/manual_chart_data_test.csv")
test_labels<-test_labels %>%
  dplyr::filter(complete.cases(Gap))
labels<-rbind(train_labels,test_labels)


df_labelled<-df %>%
  dplyr::left_join(labels,by=c("gameId" = "gameId",
                               "playId" = "playId",
                               "nflId" = "nflId")) %>%
  dplyr::filter(complete.cases(Gap)) %>%
  dplyr::select(-pff_positionLinedUp.y,-rush_direction,-jerseyNumber) %>%
  dplyr::mutate(target_var = paste0(Gap,Hole)) %>%
  dplyr::select(-Gap,-Hole)


df_unlabelled<-df %>%
  dplyr::left_join(labels,by=c("gameId" = "gameId",
                               "playId" = "playId",
                               "nflId" = "nflId")) %>%
  dplyr::filter(!complete.cases(Gap)) %>%
  dplyr::mutate(
    snap_dis_from_los = ifelse(is.na(snap_dis_from_los),0,snap_dis_from_los),
    separation_from_snap_position = ifelse(is.na(separation_from_snap_position),0,separation_from_snap_position),
    separation_from_los_x = ifelse(is.na(separation_from_los_x),0,separation_from_los_x),
    left_side_sep_from_snap = ifelse(is.na(left_side_sep_from_snap),0,left_side_sep_from_snap),
    right_side_sep_from_snap = ifelse(is.na(right_side_sep_from_snap),0,right_side_sep_from_snap),
    forward_sep_from_snap = ifelse(is.na(forward_sep_from_snap),0,forward_sep_from_snap),
    forward_sep_dis_time = ifelse(is.na(forward_sep_dis_time),0,forward_sep_dis_time),
    backward_sep_from_snap = ifelse(is.na(backward_sep_from_snap),0,backward_sep_from_snap),
    closest_down_edge_separation = ifelse(is.na(closest_down_edge_separation),0,closest_down_edge_separation),
    closest_up_edge_separation = ifelse(is.na(closest_up_edge_separation),0,closest_up_edge_separation),
    closest_dt_separation = ifelse(is.na(closest_dt_separation),0,closest_dt_separation),
    qb_separation = ifelse(is.na(qb_separation),0,qb_separation),
    qb_dir_diff = ifelse(is.na(qb_dir_diff),0,qb_dir_diff)
  ) %>%
  dplyr::group_by(gameId,playId,nflId) %>%
  dplyr::mutate(
    dis_los_x_max = ifelse(is.na(dis_los_x_max),0,dis_los_x_max),
    frame_cross_los = ifelse(is.na(frame_cross_los),0,frame_cross_los),
    cross_los_bool = ifelse(is.na(cross_los_bool),0,cross_los_bool)
  ) %>%
  ungroup() %>%
  rename(pff_positionLinedUp = pff_positionLinedUp.x) %>%
  dplyr::select(-jerseyNumber,-Gap,-pff_positionLinedUp.y,-Hole,-rush_direction)





#### apply predictions
target_var<-predict(gap_model$finalModel,df_unlabelled,type="response")

length(target_var$predictions)

df_unlabelled$target_var<-target_var$predictions


df_unlabelled<-df_unlabelled %>%
  dplyr::mutate(
    A1_prob = target_var[,"A1"],
    A2_prob = target_var[,"A2"],
    B3_prob = target_var[,"B3"],
    B4_prob = target_var[,"B4"],
    C5_prob = target_var[,"C5"],
    C6_prob = target_var[,"C6"]
    ) %>%
  dplyr::select(-target_var) 
  
  max<-df_unlabelled %>%
  dplyr::select(A1_prob,A2_prob,B3_prob,B4_prob,C5_prob,C6_prob) %>%
    mutate(
      max = names(.)[max.col(.)]
    ) %>%
    dplyr::select(max)

df_unlabelled<-cbind(df_unlabelled,max)

df_unlabelled<-df_unlabelled %>%
  mutate(
    target_var = case_when(
      max == "A1_prob" ~ "A1",
      max == "A2_prob" ~ "A2",
      max == "B3_prob" ~ "B3",
      max == "B4_prob" ~ "B4",
      max == "C5_prob" ~ "C5",
      max == "C6_prob" ~ "C6",
      TRUE ~ as.character(NA)
    )
    ) %>%
  dplyr::select(-A1_prob,-A2_prob,-B3_prob,-B4_prob,-C5_prob,-C6_prob,-max)

colnames(df_unlabelled)<-colnames(df_labelled)
  
final_df<-rbind(df_labelled,df_unlabelled)

##### add assignments

assignments<-read.csv("gap_assignments.csv") %>%
  dplyr::select(-n)



round(prop.table(table(final_df$target_var)), 2)




final_df<-final_df %>%
  rename(pff_positionLinedUp = pff_positionLinedUp.x) %>%
  dplyr::left_join(assignments,by=c("pff_positionLinedUp" = "pff_positionLinedUp",
                                    "target_var" = "target_var"))

round(prop.table(table(final_df$target_var,final_df$assigned)), 2)



#### filter to identify types of clusters
write_parquet(final_df,"final_df.parquet")








