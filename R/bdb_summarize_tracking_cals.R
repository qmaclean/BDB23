


tr<read_parquet("wk1_add_metrics.parquet")



##### add a cut to the frame so within 2.5 seconds after the snap
### count over in_play 

### cross over bool filter out people who didn't cross the los? 
#how do you factor for people who were blocked off the Los



bdb_summarize_tracking_metrics<-function(df_tracking){
  
  require(tidyverse)
  
  start_time <- Sys.time()
  
  df_tracking<-df_tracking %>%
  filter(team1 == defensiveTeam & 
           same_player == 0,
         cross_los_bool == 0) %>%
  group_by(gameId,playId,nflId1) %>%
  summarise(
    # mean
    var_x = mean(var_x,na.rm = T),
    var_y = mean(var_y,na.rm = T),
    var_dir_x = mean(var_dir_x),
    var_dir_y = mean(var_dir_y),
    var_dir = mean(var_dir),
    var_s_theta = mean(var_s_theta),
    var_s = mean(var_s),
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
    closest_dt_separation = mean(closest_dt_separtion,na.rm = T),
    player_dir_diff = mean(player_dir_diff,na.rm = T),
    closest_pass_block_player_dir_diff = mean(closet_pass_block_player_dir_diff,na.rm = T),
    qb_dir_diff = mean(qb_dir_diff,na.rm = T),
    #dis_los_x_abs = mean(dis_los_x_abs,na.rm = T),
    dis_los_x_max = mean(dis_los_x_max,na.rm = T),
    frame_cross_los = mean(frame_cross_los,na.rm = T),
    
    
    ##### count percentage of closest pass block percentage per frame
    #closest_
    
    
    
  ) %>%
  ungroup() %>%
  dplyr::rename(nflId = nflId1)
  
  end_time <- Sys.time()
  print(paste('(bdb_summarize_tracking_cals): Took', round(end_time - start_time, 2), 'minutes for week'))
  
  return(df_tracking)
  
  
}

summary_tr<-bdb_summarize_tracking_metrics(tr)
  
  
  
  