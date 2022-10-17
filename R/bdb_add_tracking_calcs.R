

library(arrow)
library(sparklyr)

#tracking<-wk5

#sample<-tracking %>% filter(gameId=="2021100700",playId=="95")

## each frame is 0.1 seconds


bdb_create_vars_loop <- function(df_tracking){
  

  
  ##### start data loop
  df_tracking_list <- df_tracking %>%
    group_split(playId) # split data by week to save memory
  
  print('Starting Loop')
  
  
  
  require(tidyverse)
  require(zoo)
  
  df_tracking_vars <- {}
  for(i in 1:length(df_tracking_list)){
    start_time <- Sys.time()
    
    df_tracking_vars[[i]] <- df_tracking_list[[i]] %>%
      select(playId, gameId, frameId,
             possessionTeam,defensiveTeam, snap, pass_forward, in_play,dir_x, dir_y, s_theta, dir, o, s, dis, s_x,s_y, a,a_x,a_y, o_x,o_y,
             los_x, los_y,pre_play,in_play,post_play,qb_sack,
             snap_x,snap_y,pass_forward_x,pass_forward_y,nflId, x, y, pff_positionLinedUp,pff_role,team) %>%
      # join with play data
      inner_join(df_tracking_list[[i]] %>%
                   select(playId, gameId, frameId, nflId, x, y, team, pff_role,
                          pff_positionLinedUp, s_theta), by = c('playId', 'gameId', 'frameId')) %>% # join with tracking data to see interactions between players
      rename_at(vars(contains('.x')), function(x) gsub('\\.x', '1', x)) %>% # rename vars in df_tracking 1 to player 1
      rename_at(vars(contains('.y')), function(x) gsub('\\.y', '2', x)) %>% # rename vars in df_tracking 2 to player 2
      filter(team1 != 'football' & team2 != 'football',
             pff_role1 %in% c('Pass Block','Pass','Pass Rush') &
               pff_role2 %in% c('Pass Block','Pass','Pass Rush')) %>% # filter for specific pass rush (utilize coverage as variable)
      group_by(gameId, playId, nflId1, nflId2) %>%
      dplyr::mutate(
                    var_x = var(x1,na.rm = T), # variance of x coord
                    var_y = var(y1,na.rm = T), # variance of y coord
                    var_dir_x = var(dir_x,na.rm = T), # variance in velocity in x-direction
                    var_dir_y = var(dir_y,na.rm = T), # variance in velocity in y-direction
                    avg_dir_x = mean(dir_x, na.rm = TRUE), # avg of velocity in x-dir
                    avg_dir_y = mean(dir_y, na.rm = TRUE), # avg of velocity in y-dir
                    var_s_theta = var(s_theta1,na.rm = T), # variance in directional velo
                    avg_s_theta = mean(s_theta1, na.rm = TRUE), # avg directional velo
                    var_dir = var(dir), # variance of player direction
                    avg_dir = mean(dir, na.rm = TRUE), # avg player direction
                    var_s = var(s), # variance in speed
                    avg_s = mean(s, na.rm = TRUE), # avg speed
                    sum_dis = sum(dis),
                    pre_play_dis = sum(dis[pre_play == 1]),
                    ### get player orientation to another player; need to parse out QB orientation
                    # get atan2 in degrees
                    # https://math.stackexchange.com/questions/707673/find-angle-in-degrees-from-one-point-to-another-in-2d-space
                    player_orientation =  atan2((y2 - y1), (x2 - x1)) * (180 / pi),
                    player_orientation = (360 - player_orientation) + 90,
                    player_orientation = case_when(player_orientation < 0 ~ player_orientation + 360,
                                                   player_orientation > 360 ~ player_orientation - 360,
                                                   TRUE ~ player_orientation),
                    player_orientation_diff = abs(o - player_orientation),
                    player_orientation_diff = pmin(360 - player_orientation_diff,player_orientation_diff),
                    snap_dis_from_los = snap_x - los_x, 
                    separation = sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2),
                    avg_sep = mean(separation[in_play == 1], na.rm = TRUE),
                    snap_separation = ifelse(snap == 1,separation,NA),
                    separation_from_snap_position = sqrt((x1 - snap_x) ^ 2 + (y1 - snap_y) ^ 2),
                    separation_from_los_x = sqrt((x1 - los_x) ^ 2 + (y1 - los_y) ^ 2),
                    same_team = ifelse(team1 == team2,1,0),
                    same_player = ifelse(nflId1 == nflId2,1,0)) %>%
      ungroup()
        
    
    end_time <- Sys.time()
    print(paste('(bdb_create_vars_loop): Took', round(end_time - start_time, 2), 'minutes for play', i))
  }
  
  df_tracking_vars <- do.call('rbind', df_tracking_vars)
  
  return(df_tracking_vars)
}

#rm(tr)
#write_parquet(tr<-bdb_create_vars_loop(tracking),'wk1_loop.parquet')



####


### separation at snap
### orientation to position -> few frames after snap? 


### ngs is 10 frames per second

#write_csv(tr,"tracking2020_agg.csv")


### Stunts 
tr<-read_parquet("wk1_loop.parquet")


bdb_add_context_distance<-function(df_tracking){
  
  require(tidyverse)
  start_time <- Sys.time()
  
  df_tracking<-df_tracking %>%
    mutate(
      ### left dist
      left_side_sep_from_snap = ifelse(in_play == 1 & y1 > snap_y,sqrt((x1 - snap_x) ^ 2 + (y1 - snap_y) ^ 2),NA),
      left_side_sep_from_dis = ifelse(in_play == 1 & y1 > snap_y,dis,NA),
      left_side_dis_frames = ifelse(in_play == 1 & y1 > snap_y,1,0),
      ### right dist
      right_side_sep_from_snap = ifelse(in_play == 1 & y1 < snap_y,sqrt((x1 - snap_x) ^ 2 + (y1 - snap_y) ^ 2),NA),
      right_side_sep_from_dis = ifelse(in_play == 1 & y1 < snap_y,dis,NA),
      right_side_dis_frames = ifelse(in_play == 1 & y1 < snap_y,1,0),
      ### forward dist
      forward_sep_from_snap = ifelse(in_play == 1 & x1 > snap_y,sqrt((x1 - snap_x) ^ 2 + (y1 - snap_y) ^ 2),NA),
      forward_sep_from_dis = ifelse(in_play == 1 & x1 > snap_y,dis,NA),
      forward_dis_frames = ifelse(in_play == 1 & x1 > snap_y,1,0),
      ### backward dist
      backward_sep_from_snap = ifelse(in_play == 1 & x1 < snap_y,sqrt((x1 - snap_x) ^ 2 + (y1 - snap_y) ^ 2),NA),
      backward_sep_from_dis = ifelse(in_play == 1 & x1 < snap_y,dis,NA),
      backward_dis_frames = ifelse(in_play == 1 & x1 < snap_y,1,0),
    )
  
  end_time <- Sys.time()
  print(paste('(bdb_add_context_distance): Took', round(end_time - start_time, 2), 'minutes for week'))
  
  return(df_tracking)
  
  
}


tr<-bdb_add_context_distance(tr)
write_parquet(tr<-bdb_add_context_distance(tr),'wk1_add_metrics.parquet')



bdb_add_closest_pass_block_players_metrics<-function(df_tracking){
  
  require(tidyverse)
  start_time <- Sys.time()
  Tackles<-c("LT","RT")
  Guards<-c("LG","RG")
  #Center<-c("C")
  
  df_tracking<-df_tracking %>%
    group_by(gameId,playId,nflId1, frameId) %>%  
    mutate(
      ## closest pass block
      ### including pff_role2 would be possible data leakage
      closest_pass_block_player_id = nflId2[separation == min(separation[same_player == 0 & possessionTeam == team2 & pff_positionLinedUp2 %in% c("LT","RT","LG","RG","C")],na.rm = T)][1],
      closest_pass_block_position = pff_positionLinedUp2[separation == min(separation[same_player == 0 & possessionTeam == team2 & pff_positionLinedUp2 %in% c("LT","RT","LG","RG","C")],na.rm = T)][1],
      closest_pass_block_player_separation = min(separation[same_player == 0 & possessionTeam == team2 & nflId2 == closest_pass_block_player_id], na.rm = T),
      ### closest tackle 
      closest_pass_block_tackle_player_id = nflId2[separation == min(separation[same_player == 0 & possessionTeam == team2 & pff_positionLinedUp2 %in% Tackles],na.rm = T)][1],
      closest_pass_block_tackle_player_separation = min(separation[same_player == 0 & possessionTeam == team2 & nflId2 == closest_pass_block_tackle_player_id], na.rm = T),
      ### closest guards
      closest_pass_block_guard_player_id = nflId2[separation == min(separation[same_player == 0 & possessionTeam == team2 & pff_positionLinedUp2 %in% Guards],na.rm = T)][1],
      closest_pass_block_guard_player_separation = min(separation[same_player == 0 & possessionTeam == team2 & nflId2 == closest_pass_block_guard_player_id], na.rm = T),
      ### center separation; duplicative with los separation
      #closest_pass_block_center_player_id = nflId2[separation == min(separation[same_player == 0 & possessionTeam == team2& pff_positionLinedUp2 %in% Center],na.rm = T)][1],
      #closest_pass_block_center_player_separation = min(separation[same_player == 0 & possessionTeam == team2 & nflId2 == closest_pass_block_center_player_id], na.rm = T)
    ) %>%
    ungroup()
  
  end_time <- Sys.time()
  print(paste('(bdb_add_closest_pass_block_players_metrics): Took', round(end_time - start_time, 2), 'minutes for week'))
  
  return(df_tracking)
  
  
}


tr<-bdb_add_closest_pass_block_players_metrics(tr)
write_parquet(tr,'wk1_add_metrics.parquet')


####### separation to non-oline offensive players or skill position
bdb_add_qb_seperation_metrics<-function(df_tracking){
  
  require(tidyverse)
  start_time <- Sys.time()
  #### create position vectors for separation metrics


  
  df_tracking<-df_tracking %>%
    group_by(gameId,playId,nflId1, frameId) %>%  
    mutate(
   
      ## QB separation
      qb_player_id = nflId2[separation == min(separation[same_player == 0 & possessionTeam == team2 & pff_positionLinedUp2 == "QB"],na.rm = T)][1],
      qb_separation = min(separation[same_player == 0 & possessionTeam == team2 & pff_positionLinedUp2 == "QB"], na.rm = T),
    ) %>%
    ungroup()
  
  end_time <- Sys.time()
  print(paste('Took', round(end_time - start_time, 2), 'minutes for week'))
  
  return(df_tracking)
  
  
}


tr<-bdb_add_qb_seperation_metrics(tr)
write_parquet(tr,'wk1_add_metrics.parquet')


bdb_add_closest_pass_rush_players_metrics<-function(df_tracking){
  
  require(tidyverse)
  start_time <- Sys.time()
  
  Up_EDGE<-c("LEO","ROLB","REO","LOLB")
  Down_EDGE<-c("RE","LE")
  LB<-c("LILB","RLB","RILB","LLB","MLB")
  DTs<-c("DRT","DLT","NLT","NT","NRT")

  
  df_tracking<-df_tracking %>%
    group_by(gameId,playId,nflId1, frameId) %>%  
    mutate(
      ### closest pass rush
      closest_pass_rush_player_id = nflId2[separation == min(separation[same_player == 0 & possessionTeam == team2 & pff_positionLinedUp2 %in% c("RCB","SCBR","FS","LCB","SCBoL","SCBiL","FSR","SCBL","FSL","SSL","SS",
                                                                                                                                                 "SCBiR","SCBoR","SSR",
                                                                                                                                                 "LEO","ROLB","REO","LOLB",
                                                                                                                                                 "RE","LE","LILB","RLB","RILB","LLB","MLB",
                                                                                                                                                 "DRT","DLT","NLT","NT","NRT")],na.rm = T)][1],
      closest_pass_rush_position = pff_positionLinedUp2[separation == min(separation[same_player == 0 & possessionTeam == team2 & pff_positionLinedUp2 %in% c("RCB","SCBR","FS","LCB","SCBoL","SCBiL","FSR","SCBL","FSL","SSL","SS",
                                                                                                                                                              "SCBiR","SCBoR","SSR",
                                                                                                                                                              "LEO","ROLB","REO","LOLB",
                                                                                                                                                              "RE","LE","LILB","RLB","RILB","LLB","MLB",
                                                                                                                                                              "DRT","DLT","NLT","NT","NRT")],na.rm = T)][1],
      closest_pass_rush_player_separation = min(separation[same_player == 0 & possessionTeam == team2 & nflId2 == closest_pass_rush_player_id], na.rm = T),
    ) %>%
    ungroup()
  
  end_time <- Sys.time()
  print(paste('Took', round(end_time - start_time, 2), 'minutes for week'))
  
  return(df_tracking)
  
  
}


tr<-bdb_add_closest_pass_rush_players_metrics(tr)
write_parquet(tr,'wk1_add_metrics.parquet')



### add specific position separation
bdb_add_closest_pass_rush_specific_players_metrics<-function(df_tracking){
  
  require(tidyverse)
  start_time <- Sys.time()
  
  Up_EDGE<-c("LEO","ROLB","REO","LOLB")
  Down_EDGE<-c("RE","LE")
  LB<-c("LILB","RLB","RILB","LLB","MLB")
  DTs<-c("DRT","DLT","NLT","NT","NRT")
  
  
  df_tracking<-df_tracking %>%
    group_by(gameId,playId,nflId1, frameId) %>%  
    mutate(
      ### closest pass rush
      # Up Edge
      closest_upedge_player_id = nflId2[separation == min(separation[same_player == 0 & defensiveTeam == team2 & pff_positionLinedUp2 %in% Up_EDGE],na.rm = T)][1],
      closest_upedge_player_separation = min(separation[same_player == 0 & defensiveTeam == team2 & nflId2 == closest_upedge_player_id], na.rm = T),
      # Down EDGE 
      closest_down_edge_player_id = nflId2[separation == min(separation[same_player == 0 & defensiveTeam == team2 & pff_positionLinedUp2 %in% Down_EDGE],na.rm = T)][1],
      closest_down_edge_player_separation = min(separation[same_player == 0 & defensiveTeam == team2 & nflId2 == closest_down_edge_player_id], na.rm = T),
      # Up LBs                                                       
      #closest_lb_player_id = nflId2[separation == min(separation[same_player == 0 & defensiveTeam == team2 & pff_positionLinedUp2 %in% LB],na.rm = T)][1],
      #closest_lb_player_separation = min(separation[same_player == 0 & defensiveTeam == team2 & nflId2 == closest_lb_player_id], na.rm = T),
      # DTs
      closest_DTs_player_id = nflId2[separation == min(separation[same_player == 0 & defensiveTeam == team2 & pff_positionLinedUp2 %in% DTs],na.rm = T)][1],
      closest_DTs_player_separation = min(separation[same_player == 0 & defensiveTeam == team2 & nflId2 == closest_DTs_player_id], na.rm = T),
    ) %>%
    ungroup()
  
  end_time <- Sys.time()
  print(paste('Took', round(end_time - start_time, 2), 'minutes for week'))
  
  return(df_tracking)
  
  
}


##### get theta difference metrics #####
### get theta difference
bdb_add_closest_pass_rush_specific_players_metrics<-function(df_tracking){
  
  require(tidyverse)
  start_time <- Sys.time()
  
  df_tracking<-df_tracking %>%
    group_by(gameId,playId,nflId1, frameId) %>%  
    mutate(
    player_dir_diff = s_theta1 - s_theta2,
    closest_pass_block_player_dir_diff = s_theta1 - s_theta2[nflId2 == closest_pass_block_player_id],
    closest_pass_rush_play_dir_diff = s_theta1 - s_theta2[nflId2 == closest_pass_rush_player_id],
    qb_play_dir_diff = s_theta1 - s_theta2[nflId2 == qb_player_id]
    ) %>%
    ungroup()
    
  end_time <- Sys.time()
  print(paste('Took', round(end_time - start_time, 2), 'minutes for week'))

return(df_tracking)

}


############ OLD ###########




   
 
DBs<-c("RCB","SCBR","FS","LCB","SCBoL","SCBiL","FSR","SCBL","FSL","SSL","SS",
                "SCBiR","SCBoR","SSR")

        

        




