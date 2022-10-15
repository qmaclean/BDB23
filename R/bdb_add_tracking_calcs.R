
#library(tidyverse)
#tracking<-read.csv("tracking2020_prepped.csv")

#parallelly::availableCores()
#future::plan("multisession")



sample<-tracking %>% filter(gameId=="2021100700",playId=="95")

## each frame is 0.1 seconds


bdb_get_vars_line <- function(df_tracking){
  
  #### create position vectors for separation metrics
  Guards<-c("LG","RG")
  Tackles<-c("LT","RT")
  Center<-c("C")
  QB<-c("QB")
  TEs<-c("TE-L","TE-oR","TE-R","TE-oL","TE-iL","TE-iR")
  Backs<-c("HB-R","HB","HB-L","FB-L","FB","FB-R")
  WRs<-c("LWR","RWR","SLWR","SRoWR","SRiWR","SRWR","SLoWR","SLiWR")
  DBs<-c("RCB","SCBR","FS","LCB","SCBoL","SCBiL","FSR","SCBL","FSL","SSL","SS",
         "SCBiR","SCBoR","SSR")
  Up_EDGE<-c("LEO","ROLB","REO","LOLB")
  Down_EDGE<-c("RE","LE")
  LB<-c("LILB","RLB","RILB","LLB","MLB")
  DTs<-c("DRT","DLT","NLT","NT","NRT")
  
  ##### start data loop
  df_tracking_list <- df_tracking %>%
    group_split(gameId) # split data by week to save memory
  
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
             snap_x,snap_y,nflId, x, y, pff_positionLinedUp,pff_role,team) %>%
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
      dplyr::mutate(pass_forward_x = ifelse(pass_forward == 1,x1,NA),
                    pass_forward_y = ifelse(pass_forward == 1,y1,NA),
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
                    backwards_dis = sum(dis[x1 > snap_x & in_play == 1]),
                    forward_dis = sum(dis[x1 < snap_x & in_play == 1]),
                    ### get player orientation to another player; need to parse out QB orientation
                    # get atan2 in degrees
                    # https://math.stackexchange.com/questions/707673/find-angle-in-degrees-from-one-point-to-another-in-2d-space
                    player_orientation =  atan2((y2 - y1), (x2 - x1)) * (180 / pi),
                    player_orientation = (360 - player_orientation) + 90,
                    player_orientation = case_when(player_orientation < 0 ~ player_orientation + 360,
                                                   player_orientation > 360 ~ player_orientation - 360,
                                                   TRUE ~ tmp),
                    player_orientation_diff = abs(o - tmp),
                    player_orientation_diff = pmin(360 - player_orientation_diff,player_orientation_diff),
                    snap_dis_from_los = snap_x - los_x, 
                    separation = sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2),
                    avg_sep = mean(separation[in_play == 1], na.rm = TRUE),
                    snap_separation = ifelse(snap == 1,separation,NA),
                    separation_from_snap_position = sqrt((x1 - snap_x) ^ 2 + (y1 - snap_y) ^ 2),
                    separation_from_los_x = sqrt((x1 - los_x) ^ 2 + (y1 - los_y) ^ 2),
                    same_team = ifelse(team1 == team2,1,0),
                    same_player = ifelse(nflId1 == nflId2,1,0)) %>% # distance travelled
      ungroup() %>%
      group_by(gameId,playId,nflId1, frameId) %>%        
      mutate(
        #### PASS BLOCK SEPARATION #####
        ### closest pass block player
        closest_pass_block_player_id = nflId2[separation == min(separation[same_player == 0 & possessionTeam == team2 & pff_role2 == "Pass Block"],na.rm = T)][1],
        closest_pass_block_player_separation = min(separation[same_player == 0 & possessionTeam == team2 & nflId2 == closest_pass_block_player_id], na.rm = T),
        ### closest tackle 
        closest_pass_block_tackle_player_id = nflId2[separation == min(separation[same_player == 0 & possessionTeam == team2 & pff_role2 == "Pass Block" & pff_positionLinedUp2 %in% Tackles],na.rm = T)][1],
        closest_pass_block_tackle_player_separation = min(separation[same_player == 0 & possessionTeam == team2 & nflId2 == closest_pass_block_tackle_player_id], na.rm = T),
        ### closest guards
        closest_pass_block_guard_player_id = nflId2[separation == min(separation[same_player == 0 & possessionTeam == team2 & pff_role2 == "Pass Block" & pff_positionLinedUp2 %in% Guards],na.rm = T)][1],
        closest_pass_block_guard_player_separation = min(separation[same_player == 0 & possessionTeam == team2 & nflId2 == closest_pass_block_guard_player_id], na.rm = T),
        ### center separation
        closest_pass_block_center_player_id = nflId2[separation == min(separation[same_player == 0 & possessionTeam == team2 & pff_role2 == "Pass Block" & pff_positionLinedUp2 %in% Center],na.rm = T)][1],
        closest_pass_block_center_player_separation = min(separation[same_player == 0 & possessionTeam == team2 & nflId2 == closest_pass_block_center_player_id], na.rm = T),
        ## QB separation
        
        
        #### PASS RUSH SEPARATION #####
        closest_pass_rush_player_id = nflId2[separation == min(separation[same_player == 0 & defensiveTeam == team2 & pff_role2 == "Pass Rush"],na.rm = T)][1],
      ) %>%
      filter(same_player == 0 & pff_role1 == "Pass Rush") 
             
    
    

    
    end_time <- Sys.time()
    print(paste('Took', round(end_time - start_time, 2), 'minutes for week', i))
  }
  
  df_tracking_vars <- do.call('rbind', df_tracking_vars)
  
  return(df_tracking_vars)
}

tr<-bdb_get_vars_line(sample)


####
#### metric distance y?
### distance from offensive play
#### distance to QB
### distance to DT
### distance to EDGE rusher?
#### orientation to QB
### distance to LT, LG, C, RG, RT



#write_csv(tr,"tracking2020_agg.csv")


### Stunts 

# contextual 








