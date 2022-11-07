


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
    separation_from_snap_position = ifelse(is.na(separation_from_snap_position),0,separation_from_snap_position),
    qb_separation = ifelse(is.na(qb_separation),0,qb_separation),
    qb_dir_diff = ifelse(is.na(qb_dir_diff),0,qb_dir_diff),
    
  ) %>%
  dplyr::group_by(gameId,playId,nflId) %>%
  dplyr::mutate(
    dis_los_x_max = ifelse(is.na(dis_los_x_max),0,dis_los_x_max),
    frame_cross_los = ifelse(is.na(frame_cross_los),0,frame_cross_los),
    cross_los_bool = ifelse(is.na(cross_los_bool),0,cross_los_bool)
  ) %>%
  ungroup()

summary(df)

rf<-readRDS("model_objects/active/expected_block_model_rf.rds")

predictions<-predict(rf$finalModel,df,type="response")

df$xBlock_predictions<-predictions$predictions

df<-df %>%
  dplyr::mutate(
    A_prob = xBlock_predictions[,"A"],
    B_prob = xBlock_predictions[,"B"]
  ) %>%
  dplyr::select(-xBlock_predictions) 

max<-df %>%
  dplyr::select(A_prob,B_prob) %>%
  mutate(
    max = names(.)[max.col(.)]
  ) %>%
  dplyr::select(max)

# get class predictions? 

df<-cbind(df,max)

df<-df %>%
  mutate(
    xBlock = case_when(
      max == "A_prob" ~ "Yes",
      max == "B_prob" ~ "No",
      TRUE ~ as.character(NA)
    )
  ) %>%
  dplyr::select(-max) 



#pff<-pff %>%
#  dplyr::select(gameId,playId,nflId,)

#df<-df %>%
#  dplyr::left_join(pff,by=c("gameId" = "gameId",
#                            "playId" = "playId",
#                            "nflId" = "nflId"))

round(prop.table(table(df$xBlock,df$Blocked)), 2)

df<-df %>%
  dplyr::select(gameId,playId,nflId,Blocked,A_prob,B_prob,xBlock)


write_parquet(df,"expected_block_results.parquet")

### quick analyze of results
players<-read_csv("data/src/players.csv") %>%
  dplyr::select(nflId,displayName,officialPosition)

blocked_rate<-df %>%
  mutate(xBlocked = ifelse(xBlock == "Yes",1,0),
         Blocked = ifelse(Blocked == "Yes",1,0)) %>%
  dplyr::group_by(nflId) %>%
  dplyr::summarise( n = n(),
                  xBlock_rate = sum(xBlocked) / n(),
                   Block_rate = sum(Blocked) / n()) %>%
  dplyr::mutate(Block_RTOE = Block_rate - xBlock_rate) %>%
  dplyr::filter(n >= 100) %>%
  dplyr::left_join(players,by=c("nflId" = "nflId"))
  



  
### 98 % accuracy




