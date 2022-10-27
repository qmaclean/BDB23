


#### Load in PFF Scouting Information & clean to understand blocking assignments

bdb_load_pff_data<-function(){
  
  require(tidyverse)
  
  pff<-read.csv("data/src/pffScoutingData.csv") 
    ##### Join on manual charted data ###

  pff_pass_rush<-pff %>%
    dplyr::filter(pff_role == "Pass Rush") %>%
    dplyr::select(gameId,playId,nflId,pff_positionLinedUp,pff_hit,
                  pff_hurry,pff_sack) 
  
  pff_blocked_position<-pff %>%
    dplyr::filter(pff_role == "Pass Block") %>%
    dplyr::select(gameId,playId,nflId,pff_positionLinedUp,pff_beatenByDefender,
                  pff_hitAllowed,pff_hurryAllowed,pff_sackAllowed,pff_nflIdBlockedPlayer,
                  pff_blockType,pff_backFieldBlock) %>%
    dplyr::left_join(pff_pass_rush,by=c("gameId" = "gameId","playId" = "playId","pff_nflIdBlockedPlayer" = "nflId")) %>%
    dplyr::select(
      gameId,playId,nflId,pff_positionLinedUp.y,pff_nflIdBlockedPlayer
    ) %>%
    rename(pff_BlockedPlayerLinedUpPosition = pff_positionLinedUp.y)
  
  pff_blocked_id<-pff_blocked_position %>%
    dplyr::select(gameId,playId,pff_nflIdBlockedPlayer) %>%
    dplyr::mutate(Blocked = "Y") %>%
    dplyr::distinct()
  
  pff<-pff %>%
    dplyr::left_join(pff_blocked_position,by=c("gameId","playId","nflId")) %>%
    dplyr::select(-pff_nflIdBlockedPlayer.y) %>%
    dplyr::rename(pff_nflIdBlockedPlayer = pff_nflIdBlockedPlayer.x) %>%
    dplyr::left_join(pff_blocked_id,by=c("gameId" = "gameId","playId" = "playId","nflId" = "pff_nflIdBlockedPlayer")) %>%
    dplyr::mutate(Blocked = case_when(pff_role != "Pass Rush" ~ "N/A",
                                      pff_role == "Pass Rush" & Blocked == "Y" ~ "Yes",
                                      pff_role == "Pass Rush" ~ "No",
                                      TRUE ~ as.character(NA)))
    
  ### create a metric for unblocked?
  
    rm(pff_blocked_id)
    rm(pff_blocked_position)
    rm(pff_pass_rush)
    
    return(pff)
}

pff<-bdb_load_pff_data()


##### create charting data framework


### Stuntin' Is a Habit ####
#https://commons.wikimedia.org/wiki/File:American_football_Gaps_and_holes.svg 
wk5<-read.csv("data/week5.csv")

#players<-read.csv("data/players.csv")
jersey_numbers<-wk5 %>%
  dplyr::filter(frameId == 1) %>%
  dplyr::select(gameId,playId,nflId,jerseyNumber)

chart_data<-pff %>%
  dplyr::filter(pff_role %in% c("Pass Rush")) %>%
  dplyr::select(gameId,playId,nflId,pff_role,pff_positionLinedUp) %>%
  dplyr::inner_join(jersey_numbers,by=c("gameId" = "gameId","playId" = "playId",
                                       "nflId" = "nflId")) 

write.csv(chart_data,"chart_data_wk5.csv",row.names = FALSE)


##### export week 1 - 5 for training

## test on week 6 -8

  
