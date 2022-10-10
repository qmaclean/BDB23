


#### Load in PFF Scouting Information & clean to understand blocking assignments

bdb_load_pff_data<-function(){
  
  require(tidyverse)
  
  pff<-read.csv("data/pffScoutingData.csv") 
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
    
    return(pff)
}


