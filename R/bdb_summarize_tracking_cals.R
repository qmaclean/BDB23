

library(arrow)
tr<-read_parquet("data_processed/wk_add_metrics/wk1_add_metrics.parquet")



##### add a cut to the frame so within 2.5 seconds after the snap
### count over in_play 

### cross over bool filter out people who didn't cross the los? 
#how do you factor for people who were blocked off the Los



bdb_summarize_tracking_metrics<-function(df_tracking){
  
  require(tidyverse)
  
  start_time <- Sys.time()
  
  df_tracking<-df_tracking %>%
  filter(team1 == defensiveTeam & 
           same_player == 0) %>%
    mutate(
      RT = ifelse(closest_pass_block_position == 'RT',1,0),
      LT = ifelse(closest_pass_block_position == 'LT',1,0),
      RG = ifelse(closest_pass_block_position == 'RG',1,0),
      LG = ifelse(closest_pass_block_position == 'LG',1,0),
      C = ifelse(closest_pass_block_position == 'C',1,0)
    ) %>%
  group_by(gameId,playId,nflId1) %>%
  summarise(
    # mean
    mean_var_x = mean(var_x,na.rm = T),
    mean_var_y = mean(var_y,na.rm = T),
    mean_var_dir_x = mean(var_dir_x),
    mean_var_dir_y = mean(var_dir_y),
    mean_var_dir = mean(var_dir),
    mean_var_s_theta = mean(var_s_theta),
    mean_var_s = mean(var_s),
    snap_dis_from_los = mean(snap_dis_from_los,na.rm = T),
    sum_dis = mean(sum_dis),
    pre_play_dis = mean(pre_play_dis),
    snap_dis_from_los = mean(snap_dis_from_los,na.rm=T),
    separation = mean(separation,na.rm = T),
    snap_separation = mean(snap_separation,na.rm = T),
    separation_from_snap_position = mean(separation_from_snap_position,na.rm = T),
    separation_from_los_x = mean(separation_from_los_x,na.rm = T),
    left_side_sep_from_snap = mean(left_side_sep_from_snap,na.rm = T),
    left_side_sep_from_dis = sum(left_side_sep_from_dis,na.rm = T),
    left_side_dis_time = sum(left_side_dis_frames,na.rm = T) * 0.1,
    right_side_sep_from_snap = mean(right_side_sep_from_snap,na.rm = T),
    right_side_sep_from_dis = sum(right_side_sep_from_dis,na.rm = T),
    right_side_dis_time = sum(right_side_dis_frames,na.rm = T) * 0.1,
    forward_sep_from_snap = mean(forward_sep_from_snap,na.rm = T),
    forward_sep_from_dis = sum(forward_sep_from_dis,na.rm = T),
    forward_sep_dis_time = sum(forward_dis_frames,na.rm = T) * 0.1,
    backward_sep_from_snap = mean(backward_sep_from_snap,na.rm = T),
    backward_sep_from_dis = sum(backward_sep_from_dis,na.rm = T),
    backward_sep_dis_time = sum(backward_dis_frames,na.rm = T),
    closest_pass_block_separation = mean(closest_pass_block_separation,na.rm = T),
    closest_pass_block_tackle_separation = mean(closest_pass_block_tackle_separation,na.rm = T),
    closest_pass_block_guard_separation = mean(closest_pass_block_guard_separation,na.rm = T),
    qb_separation = mean(closest_qb_separation,na.rm = T),
    closest_down_edge_separation = mean(closest_down_edge_separation,na.rm = T),
    closest_up_edge_separation = mean(closest_up_edge_separation,na.rm = T),
    closest_dt_separation = mean(closest_dt_separation,na.rm = T),
    player_dir_diff = mean(player_dir_diff,na.rm = T),
    closest_pass_block_player_dir_diff = mean(closest_pass_block_player_dir_diff,na.rm = T),
    qb_dir_diff = mean(qb_dir_diff,na.rm = T),
    #dis_los_x_abs = mean(dis_los_x_abs,na.rm = T),
    dis_los_x_max = mean(dis_los_x_max,na.rm = T),
    frame_cross_los = mean(frame_cross_los,na.rm = T),
    ### sum
    sum_snap_dis_from_los = sum(snap_dis_from_los,na.rm = T),
    #### context variables
    cross_los_bool = mean(cross_los_bool,na.rm = T),
    ### position closest percentage?
    RT_pct = sum(RT,na.rm = T) / n_distinct(frameId),
    LT_pct = sum(LT,na.rm = T) / n_distinct(frameId),
    LG_pct = sum(LG,na.rm = T) / n_distinct(frameId),
    RG_pct = sum(RG,na.rm = T) / n_distinct(frameId),
    C_pct  = sum(C,na.rm = T) / n_distinct(frameId),
    pff_positionLinedUp = pff_positionLinedUp1[1]
    
    
  ) %>%
  ungroup() %>%
  dplyr::rename(nflId = nflId1) 
  
  
  
  plays_join<-bdb_load_plays() %>%
    dplyr::select(gameId,playId,down,yardsToGo,absoluteYardlineNumber,offenseFormation,
                  personnelO,defendersInBox,personnelD,pff_passCoverage,pff_passCoverageType)
  
  df_tracking<-df_tracking %>%
    dplyr::left_join(plays_join,by=c("gameId" = "gameId",
                                     "playId" = "playId"))
  
  
  games<-bdb_load_games() %>%
    dplyr::select(gameId,week)
  
  df_tracking<-df_tracking %>%
    dplyr::left_join(games,by=c("gameId"))
  
  end_time <- Sys.time()
  print(paste('(bdb_summarize_tracking_cals): Took', round(end_time - start_time, 2), 'minutes for week'))
  
  return(df_tracking)
  
  
}

summary_tr<-bdb_summarize_tracking_metrics(tr)


write_parquet(summary_tr,"wk8_final.parquet")

  
 
  
  