



bdb_clean_tracking<-function(tracking = tracking){
  
  require(tidyverse)
  
  ###load join data
  plays<-bdb_load_plays()
  pff<-bdb_load_pff_data() 
  games<-bdb_load_games()
  
  
  pff_roles<-pff %>%
    dplyr::select(gameId,playId,nflId,pff_role,pff_positionLinedUp,Blocked)


  
  tracking<- tracking %>%
    left_join(plays,tracking,by=c("gameId"="gameId","playId"="playId")) %>%
    left_join(pff_roles,by=c("gameId" = "gameId",
                             "playId" = "playId",
                             "nflId" = "nflId")) %>%
    left_join(games,by=c("gameId" = "gameId"))
  
  
  rm(plays)
  rm(pff)
  rm(pff_roles)

  


tracking<-tracking %>%
  mutate(
    ############# TEAM ########################
      team_type = case_when(
      possessionTeam  == team ~ "offense",
      defensiveTeam == team ~ "defense",
      TRUE ~ as.character(NA)
    ),
    score_differential = ifelse(
      team == homeTeamAbbr,
      preSnapHomeScore - preSnapVisitorScore,
      preSnapVisitorScore - preSnapHomeScore
    ),
    #### edit x and y to be ssame direction
    to_left = ifelse(playDirection == "left",1,0),
    x=ifelse(to_left == 1,120-x,x),
    y=ifelse(to_left == 1,160/3 - y,y),
    ############# standard orientation ########
    #rotate 180 degrees for angles
    o = ifelse(playDirection == "left",o+180,o),
    #    # degrees from 0 to 360
    o = ifelse(o > 360, o - 360, o),
    #    #convert to radians
    o_radians = pi * (o / 180),
    # get orientation and direction in x and y direction
    o_x = ifelse(is.na(o),NA_real_,sin(o_radians)),
    o_y = ifelse(is.na(o),NA_real_,cos(o_radians)),
    ############# standardize direction ########
    #rotate 180 degrees
    dir = ifelse(playDirection == "left",dir + 180,dir),
    # check if degrees between 0 & 360
    dir = ifelse(dir > 360, dir - 360, dir),
    #radians convert
    dir_radians = pi * (dir / 180),
    # get orientation and direction in x and y direction
    dir_x = ifelse(is.na(dir),NA_real_,sin(dir_radians)),    #same v_x
    dir_y = ifelse(is.na(dir),NA_real_,cos(dir_radians)),    # same as v_y
    s_x = dir_x * s,
    s_y = dir_y * s,
    s_theta = atan(s_x/s_y) * s,
    s_theta = ifelse(is.nan(s_theta),0,s_theta),
    ###
    a_x = dir_x * a,
    a_y = dir_y * a,
    ##### ADD FLAGS ####
    snap = ifelse(frameId == frameId[event == "ball_snap"][1],1,0),
    pass_forward = ifelse(frameId == frameId[event == "pass_forward"][1],1,0),
    qb_sack = ifelse(frameId == frameId[event %in% c("qb_sack",
                                                      "qb_strip_sack")][1],1,0),
    pre_play = ifelse(frameId < frameId[snap == 1][1], 1, 0),
    post_play = ifelse(frameId > frameId[pass_forward == 1][1], 1, 0),
    in_play = ifelse(pre_play == 0 & post_play == 0, 1, 0),
    number_frames_in_play = sum(in_play),
  ) %>%
  dplyr::select(-yardlineSide,-yardlineNumber,-gameClock,
                -preSnapHomeScore,-preSnapVisitorScore,
                -prePenaltyPlayResult,-playResult,
                -foulName1,-foulNFLId1,-foulName2,-foulNFLId2,
                -foulName3,-foulNFLId3,-season,
                -gameDate,-gameTimeEastern,-homeTeamAbbr,
                -visitorTeamAbbr) 

  #### football location at snap
  los<-tracking %>%
  filter(event == "ball_snap",
         team == "football") %>%
  group_by(gameId,playId) %>%
  summarize(los_x = mean(x,na.rm = T),
            los_y = mean(y,na.rm = T))
  
  #### players position at snap
  snap<-tracking %>%
    dplyr::filter(event == "ball_snap",
           team != "football") %>%
    dplyr::select(gameId,playId,nflId,x,y) %>%
    dplyr::rename(snap_x = x,
                  snap_y = y) %>%
    group_by(gameId,playId,nflId) %>%
    summarize(snap_x = mean(snap_x,na.rm = T),
              snap_y = mean(snap_y,na.rm = T))
  
  pass_forward<-tracking %>%
    dplyr::filter(event == "pass_forward",
                  team != "football") %>%
    dplyr::select(gameId,playId,nflId,x,y) %>%
    dplyr::rename(pass_forward_x = x,
                  pass_forward_y = y) %>%
    group_by(gameId,playId,nflId) %>%
    summarize(pass_forward_x = mean(pass_forward_x,na.rm = T),
              pass_forward_y = mean(pass_forward_y,na.rm = T))
  
  tracking<-tracking %>%
    left_join(los,tracking,by=c("gameId","playId")) %>%
    left_join(snap,tracking,by=c("gameId","playId","nflId")) %>%
    left_join(pass_forward,by=c("gameId","playId","nflId"))



  return(tracking)

}

#wk8<-read_csv("data/week8.csv")
tracking<-bdb_clean_tracking(tracking = read_csv("data/week1.csv"))

#rm(wk8)



