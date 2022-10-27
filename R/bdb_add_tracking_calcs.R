

library(arrow)
library(sparklyr)



## each frame is 0.1 seconds


bdb_create_vars_loop <- function(df_tracking){
  

    require(tidyverse)
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




### Stunts 
tr<-read_parquet("wk8_loop.parquet")



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
write_parquet(tr,'wk8_add_metrics.parquet')
tr<-read_parquet("wk8_add_metrics.parquet")

tr<-tr %>%
  dplyr::filter(post_play == 0,
                pre_play == 0)




bdb_add_closest_pass_block_player_metrics<-function(df_tracking){
  
  require(tidyverse)
  start_time <- Sys.time()
  
  df_tracking<-df_tracking %>%
  mutate(closest_pass_block_id = ifelse(same_player == 0 & possessionTeam == team2 & pff_positionLinedUp2 %in% c("LT","RT","LG","RG","C"),nflId2,NA))

  closest_pass_block<- df_tracking %>%
    dplyr::filter(complete.cases(closest_pass_block_id)) %>%
    dplyr::select(gameId,playId,nflId1,frameId,closest_pass_block_id,separation) %>%
    group_by(gameId,playId,nflId1, frameId) %>%
    dplyr::summarize(closest_pass_block_separation = min(separation,na.rm = T))
    
  df_tracking<-df_tracking %>%
  dplyr::left_join(closest_pass_block,by=c("gameId" = "gameId","playId" = "playId","frameId" = "frameId",
                                      "nflId1" = "nflId1"))
  df_tracking<-df_tracking %>%
  mutate(closest_pass_block_separation = ifelse(closest_pass_block_separation == separation,closest_pass_block_separation,NA),
         closest_pass_block_id = ifelse(closest_pass_block_separation == separation,closest_pass_block_id,NA),
         closest_pass_block_position = ifelse(closest_pass_block_separation == separation,pff_positionLinedUp2,NA)) %>%
  ungroup()

  rm(closest_pass_block)
  
  end_time <- Sys.time()
  print(paste('(bdb_add_closest_pass_block_player_metrics): Took', round(end_time - start_time, 2), 'minutes for week'))

  return(df_tracking)
  
}

tr<-bdb_add_closest_pass_block_player_metrics(tr)
write_parquet(tr,'wk8_add_metrics.parquet')
tr<-read_parquet('wk8_add_metrics.parquet')


bdb_add_closest_tackle_metrics<-function(df_tracking){
  
  require(tidyverse)
  start_time <- Sys.time()
  
  df_tracking<-df_tracking %>%
  mutate(closest_pass_block_tackle_id = ifelse(same_player == 0 & possessionTeam == team2 & pff_positionLinedUp2 %in% c("LT","RT"),nflId2,NA))  
         
closest_pass_block_t<- df_tracking %>%
  dplyr::filter(complete.cases(closest_pass_block_tackle_id)) %>%
  dplyr::select(gameId,playId,nflId1,frameId,closest_pass_block_tackle_id,separation) %>%
  group_by(gameId,playId,nflId1, frameId) %>%
  dplyr::summarize(closest_pass_block_tackle_separation = min(separation,na.rm = T))


df_tracking<-df_tracking %>%
  dplyr::left_join(closest_pass_block_t,by=c("gameId" = "gameId","playId" = "playId","frameId" = "frameId",
                                           "nflId1" = "nflId1"))

df_tracking<-df_tracking %>%
  mutate(closest_pass_block_tackle_separation = ifelse(closest_pass_block_tackle_separation == separation,closest_pass_block_tackle_separation,NA),
         closest_pass_block_tackle_id = ifelse(closest_pass_block_tackle_separation == separation,closest_pass_block_tackle_id,NA)) %>%
  ungroup()

rm(closest_pass_block_t)

end_time <- Sys.time()
print(paste('(closest_add_closest_tackle_metrics): Took', round(end_time - start_time, 2), 'minutes for week'))

return(df_tracking)
  

}

tr<-bdb_add_closest_tackle_metrics(tr)
write_parquet(tr,'wk8_add_metrics.parquet')
tr<-read_parquet('wk8_add_metrics.parquet')


bdb_add_closest_guard_metrics<-function(df_tracking){

  require(tidyverse)
  start_time <- Sys.time()  
df_tracking<-df_tracking %>%
  mutate(closest_pass_block_guard_id = ifelse(same_player == 0 & possessionTeam == team2 & pff_positionLinedUp2 %in% c("LG","RG"),nflId2,NA))  

closest_pass_block_g<- df_tracking %>%
  dplyr::filter(complete.cases(closest_pass_block_guard_id)) %>%
  dplyr::select(gameId,playId,nflId1,frameId,closest_pass_block_guard_id,separation) %>%
  group_by(gameId,playId,nflId1, frameId) %>%
  dplyr::summarize(closest_pass_block_guard_separation = min(separation,na.rm = T))

df_tracking<-df_tracking %>%
  dplyr::left_join(closest_pass_block_g,by=c("gameId" = "gameId","playId" = "playId","frameId" = "frameId",
                                             "nflId1" = "nflId1"))

df_tracking<-df_tracking %>%
  mutate(closest_pass_block_guard_separation = ifelse(closest_pass_block_guard_separation == separation,closest_pass_block_guard_separation,NA),
         closest_pass_block_guard_id = ifelse(closest_pass_block_guard_separation == separation,closest_pass_block_guard_id,NA)) %>%
  ungroup()

rm(closest_pass_block_g)

end_time <- Sys.time()
print(paste('(closest_add_closest_guard_metrics): Took', round(end_time - start_time, 2), 'minutes for week'))

return(df_tracking)

}

tr<-bdb_add_closest_guard_metrics(tr)
write_parquet(tr,'wk8_add_metrics.parquet')
tr<-read_parquet('wk8_add_metrics.parquet')

####### pass on center given los would be similar?

bdb_add_qb_seperation_metrics<-function(df_tracking){

  require(tidyverse)
  start_time <- Sys.time()
  
  df_tracking<-df_tracking %>%
  mutate(closest_qb_id = ifelse(same_player == 0 & possessionTeam == team2 & pff_positionLinedUp2 %in% c("QB"),nflId2,NA))  

closest_qb<- df_tracking %>%
  dplyr::filter(complete.cases(closest_qb_id)) %>%
  dplyr::select(gameId,playId,nflId1,frameId,closest_qb_id,separation) %>%
  group_by(gameId,playId,nflId1, frameId) %>%
  dplyr::summarize(closest_qb_separation = min(separation,na.rm = T))

df_tracking<-df_tracking %>%
  dplyr::left_join(closest_qb,by=c("gameId" = "gameId","playId" = "playId","frameId" = "frameId",
                                             "nflId1" = "nflId1"))

df_tracking<-df_tracking %>%
  mutate(closest_qb_separation = ifelse(closest_qb_separation == separation,closest_qb_separation,NA),
         closest_qb_id = ifelse(closest_qb_separation == separation,closest_qb_id,NA)) %>%
  ungroup()

rm(closest_qb)

end_time <- Sys.time()
print(paste('(bdb_add_qb_seperation_metrics): Took', round(end_time - start_time, 2), 'minutes for week'))

return(df_tracking)

}

tr<-bdb_add_qb_seperation_metrics(tr)
write_parquet(tr,'wk8_add_metrics.parquet')
tr<-read_parquet('wk8_add_metrics.parquet')


###### CLOSEST DOWN EDGE PLAYERS ###

bdb_add_closest_down_edge_metrics<-function(df_tracking){
  
  require(tidyverse)
  start_time <- Sys.time()
  
df_tracking<-df_tracking %>%
  mutate(closest_down_edge_player_id = ifelse(same_player == 0 & defensiveTeam == team2 & pff_positionLinedUp2 %in% c("RE","LE"),nflId2,NA))  

closest_down_edge<- df_tracking %>%
  dplyr::filter(complete.cases(closest_down_edge_player_id)) %>%
  dplyr::select(gameId,playId,nflId1,frameId,closest_down_edge_player_id,separation) %>%
  group_by(gameId,playId,nflId1, frameId) %>%
  dplyr::summarize(closest_down_edge_separation = min(separation,na.rm = T))

df_tracking<-df_tracking %>%
  dplyr::left_join(closest_down_edge,by=c("gameId" = "gameId","playId" = "playId","frameId" = "frameId",
                                   "nflId1" = "nflId1"))

df_tracking<-df_tracking %>%
  mutate(closest_down_edge_separation = ifelse(closest_down_edge_separation == separation,closest_down_edge_separation,NA),
         closest_down_edge_player_id = ifelse(closest_down_edge_separation == separation,closest_down_edge_player_id,NA)) %>%
  ungroup()

rm(closest_down_edge)

end_time <- Sys.time()
print(paste('(closest_add_closest_down_edge_metrics): Took', round(end_time - start_time, 2), 'minutes for week'))

return(df_tracking)

}

tr<-bdb_add_closest_down_edge_metrics(tr)
write_parquet(tr,'wk8_add_metrics.parquet')


####### BDB Calculate Up Edge Separation metrics
bdb_add_closest_up_edge_metrics<-function(df_tracking){

  require(tidyverse)
  start_time <- Sys.time()
  df_tracking<-df_tracking %>%
  mutate(closest_up_edge_player_id = ifelse(same_player == 0 & defensiveTeam == team2 & pff_positionLinedUp2 %in% c("LEO","ROLB","REO","LOLB"),nflId2,NA))  

closest_up_edge<- df_tracking %>%
  dplyr::filter(complete.cases(closest_up_edge_player_id)) %>%
  dplyr::select(gameId,playId,nflId1,frameId,closest_up_edge_player_id,separation) %>%
  group_by(gameId,playId,nflId1, frameId) %>%
  dplyr::summarize(closest_up_edge_separation = min(separation,na.rm = T))

df_tracking<-df_tracking %>%
  dplyr::left_join(closest_up_edge,by=c("gameId" = "gameId","playId" = "playId","frameId" = "frameId",
                                          "nflId1" = "nflId1"))

df_tracking<-df_tracking %>%
  mutate(closest_up_edge_separation = ifelse(closest_up_edge_separation == separation,closest_up_edge_separation,NA),
         closest_up_edge_player_id = ifelse(closest_up_edge_separation == separation,closest_up_edge_player_id,NA)) %>%
  ungroup()


rm(closest_up_edge)

end_time <- Sys.time()
print(paste('(closest_add_closest_up_edge_metrics): Took', round(end_time - start_time, 2), 'minutes for week'))

return(df_tracking)

}

tr<-bdb_add_closest_up_edge_metrics(tr)
write_parquet(tr,'wk8_add_metrics.parquet')


bdb_add_closest_dt_metrics<-function(df_tracking){

  require(tidyverse)
start_time <- Sys.time()

df_tracking<-df_tracking %>%
  mutate(closest_dt_player_id = ifelse(same_player == 0 & defensiveTeam == team2 & pff_positionLinedUp2 %in% c("DRT","DLT","NLT","NT","NRT"),nflId2,NA))  

closest_dt<- df_tracking %>%
  dplyr::filter(complete.cases(closest_dt_player_id)) %>%
  dplyr::select(gameId,playId,nflId1,frameId,closest_dt_player_id,separation) %>%
  group_by(gameId,playId,nflId1, frameId) %>%
  dplyr::summarize(closest_dt_separation = min(separation,na.rm = T))


df_tracking<-df_tracking %>%
  dplyr::left_join(closest_dt,by=c("gameId" = "gameId","playId" = "playId","frameId" = "frameId",
                                        "nflId1" = "nflId1"))

df_tracking<-df_tracking %>%
  mutate(closest_dt_separation = ifelse(closest_dt_separation == separation,closest_dt_separation,NA),
         closest_dt_player_id = ifelse(closest_dt_separation == separation,closest_dt_player_id,NA)) %>%
  ungroup()

rm(closest_dt)

end_time <- Sys.time()
print(paste('(closest_add_closest_dt_metrics): Took', round(end_time - start_time, 2), 'minutes for week'))

return(df_tracking)

}

tr<-bdb_add_closest_dt_metrics(tr)
write_parquet(tr,'wk8_add_metrics.parquet')


##### BDB Calculate Key Player orientation
bdb_add_player_orientation_metrics<-function(df_tracking){

  require(tidyverse)
  start_time <- Sys.time()  
  
df_tracking<-df_tracking %>%
  mutate(
    player_dir_diff = s_theta1 - s_theta2,
    closest_pass_block_player_dir_diff = ifelse(nflId2 == closest_pass_block_id,s_theta1 - s_theta2,NA),
    qb_dir_diff = ifelse(nflId2 == closest_qb_id,s_theta1 - s_theta2,NA)
  )

end_time <- Sys.time()
print(paste('(bdb_add_player_orientation_metrics): Took', round(end_time - start_time, 2), 'minutes for week'))

return(df_tracking)


}

tr<-bdb_add_player_orientation_metrics(tr)
write_parquet(tr,'wk8_add_metrics.parquet')

### BDB Time to Cross LOS
bdb_add_cross_los_metrics<-function(df_tracking){

######## IDENTIFY FRAME WHEN CROSSED LOS
  require(tidyverse)
  start_time <- Sys.time()  

df_tracking<-df_tracking %>%
  mutate(dis_los_x = los_x - x1)



min_los_separation<-df_tracking %>%
  dplyr::select(gameId,playId,nflId1,frameId,separation_from_los_x,x1,los_x) %>%
  dplyr::mutate(dis_los_x_abs = abs(los_x - x1),
                dis_los_x = los_x - x1) %>%
  dplyr::filter(dis_los_x <= 0) %>%
  dplyr::group_by(gameId,playId,nflId1) %>%
  dplyr::summarize(separation_from_los_x_min = min(separation_from_los_x),
                   dis_los_x_max = max(dis_los_x,na.rm = T)) %>%
  dplyr::inner_join(df_tracking,by=c("gameId" = "gameId","playId" = "playId","nflId1" = "nflId1","dis_los_x_max" = "dis_los_x")) %>%
  dplyr::select(gameId,playId,nflId1,frameId,dis_los_x_max) %>%
  dplyr::distinct() %>%
  dplyr::rename(frame_cross_los = frameId) %>%
  ungroup()

df_tracking<-df_tracking %>%
  dplyr::left_join(min_los_separation,by=c("gameId" = "gameId",
                                          "playId" = "playId",
                                          "nflId1" = "nflId1"))



df_tracking<-df_tracking %>%
  dplyr::mutate(cross_los_bool = ifelse(frameId >= frame_cross_los,1,0))

end_time <- Sys.time()
print(paste('(bdb_add_cross_los_metrics): Took', round(end_time - start_time, 2), 'minutes for week'))

return(df_tracking)

}

tr<-bdb_add_cross_los_metrics(tr)
write_parquet(tr,'wk8_add_metrics.parquet')


##### add frames
        
#### really cool plot to build argument
min_los_separation %>%
  ggplot() +
  aes(y = -dis_los_x_max,x = frame_cross_los) +
  geom_jitter(alpha = 0.1) +
  theme_minimal() +
  labs(x = "frame (since snap)",
       y = "Distance Past Line of Scrimmage",
       title = "Time into Snap to which a defender passes Line of scrimmage") 


### normal distribution as expected




