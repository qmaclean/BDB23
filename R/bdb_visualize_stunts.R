### visualize stunts off tracking data


bdb_visualize_twoman_stunts<-function(){

library(gganimate)
library(cowplot)
library(tidyverse)
library(ggimage)
library(RCurl)
library(ggrepel)
library(gt)
library(nflfastR)
library(nflreadr)
library(arrow)
library(ggpubr)
  
  source("R/bdb_load_plays.R")


df<-read_parquet("downline_tracking.parquet")
stunts<-read_parquet("line_stunts.parquet")
downline_df<-read_parquet("final_df.parquet")

snap<-df %>%
  dplyr::select(gameId,playId,frameId,event) %>%
  dplyr::filter(event == "ball_snap") %>%
  rename(snap_id = frameId) %>%
  distinct()

pass<-df %>%
  dplyr::select(gameId,playId,frameId,event) %>%
  dplyr::filter(event == "pass_forward") %>%
  rename(pass_id = frameId) %>%
  distinct()

df<-df %>%
  dplyr::left_join(snap,by=c("gameId" = "gameId",
                             "playId" = "playId")) %>%
  dplyr::filter(frameId >= snap_id) %>%
  dplyr::left_join(pass,by=c("gameId" = "gameId",
                             "playId" = "playId")) %>%
  dplyr::filter(frameId <= pass_id) %>%
  group_by(gameId,playId,nflId) %>%
  dplyr::mutate(newframeId = row_number()) %>%
  dplyr::select(-event.x,-event.y) %>%
  ungroup()






unassigned<-downline_df %>%
  dplyr::filter(assigned == 0) %>%
  dplyr::filter(pff_positionLinedUp %in% c('DLT','LEO','ROLB','DRT','RE','DLT','REO',
                                           'LE','NLT','NT','NRT','LOLB')) %>%
  dplyr::select(gameId,playId,nflId,assigned,target_var) %>%
  dplyr::group_by(gameId,playId) %>%
  #dplyr::arrange(target_var) %>%
  mutate(rk = row_number())

xblock<-read_parquet("expected_block_results.parquet")
xpressure<-read_parquet("expected_pressure_results.parquet")
players<-read_csv("data/src/players.csv") %>%
  dplyr::select(nflId,displayName,officialPosition)
### adding logos
url.logo <- getURL("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
df.logos <- read.csv(text = url.logo)

roster<-load_rosters(seasons = 2021) %>%
  dplyr::select(gsis_it_id,headshot_url)

source("R/bdb_load_plays.R")

plays<-bdb_load_plays() %>%
  dplyr::select("gameId","playId","possessionTeam","defensiveTeam","pff_playAction")


downline_pivot<-downline_df %>%
  distinct() %>%
  dplyr::inner_join(xblock,by=c("gameId" = "gameId",
                                "playId" = "playId",
                                "nflId" = "nflId")) %>%
  dplyr::inner_join(xpressure,by=c("gameId" = "gameId",
                                   "playId" = "playId",
                                   "nflId" = "nflId")) %>%
  dplyr::filter(pff_positionLinedUp %in% c('DLT','LEO','ROLB','DRT','RE','DLT','REO',
                                           'LE','NLT','NT','NRT','LOLB'),
                pff_role == "Pass Rush") %>%
  dplyr::mutate(assigned = ifelse(is.na(assigned),0,assigned),
                unassigned = ifelse(assigned == 1,0,1)
  ) %>%
  dplyr::select(-assigned) %>%
  inner_join(stunts,by=c("gameId" = "gameId",
                         "playId" = "playId")) %>%
  dplyr::left_join(plays,by=c("gameId" = "gameId",
                              "playId" = "playId"
  )) %>%
  dplyr::left_join(roster,by=c("nflId" = "gsis_it_id")) %>%
  distinct()

stunt_type_metrics<-downline_pivot %>%
  mutate(xBlocked = ifelse(xBlock == "Yes",1,0),
         Blocked = ifelse(Blocked == "Yes",1,0),
         xPressure = ifelse(xPressure_predictions == "A",1,0),
         Pressure = ifelse(pressure == "A",1,0),
         stunt_indicator = ifelse(stunt_category != "NO STUNT","STUNT","NO STUNT")
  ) %>%
  dplyr::group_by(stunt_type,stunt_category) %>%
  dplyr::summarise( n = n(),
                    xBlock_rate = sum(xBlocked) / n(),
                    Block_rate = sum(Blocked) / n(),
                    xPressure_rate = sum(xPressure) / n(),
                    Pressure_rate = sum(Pressure) / n()) %>%
  dplyr::mutate(Block_RTUE = xBlock_rate - Block_rate,
                Pressure_RTOE = Pressure_rate - xPressure_rate)


df<-df %>%
  left_join(unassigned,by=c("gameId" = "gameId",
                            "playId" = "playId",
                            "nflId" = "nflId",
                            "target_var" = "target_var"))


mean_route<-df %>%
  dplyr::select(gameId,playId,nflId,newframeId,x,y,target_var,stunt_type,min_x,assigned,rk) %>%
  dplyr::filter(assigned == 0,
                newframeId <= 50) %>%
  dplyr::filter(
    ### solo stunts ###
    (stunt_type == "LEFT A-GAP INSIDE SOLO STUNT" & rk == 1 & target_var %in% c("A1")) |
    (stunt_type == "RIGHT A-GAP INSIDE SOLO STUNT" & rk == 1 & target_var %in% c("A2")) |
    (stunt_type == "LEFT OUTSIDE SOLO STUNT" & rk == 1 & target_var %in% c("B3","C5")) |
    (stunt_type == "RIGHT OUTSIDE SOLO STUNT" & rk == 1 & target_var %in% c("B4","C6")) |
    ### double stunts ##
    (stunt_type == "A-GAP SWITCH" & rk <= 2 & target_var %in% c("A1","A2")) |
    (stunt_type == "LEFT SIDE A-B TWIST" & rk <= 2 & target_var %in% c("A1","B3")) |
    (stunt_type == "RIGHT SIDE A-B TWIST" & rk <= 2 & target_var %in% c("A2","B4")) | 
    (stunt_type == "LEFT SIDE B-C TWIST" & rk <= 2 & target_var %in% c("B3","C5")) |
    (stunt_type == "RIGHT SIDE B-C TWIST" & rk <= 2 & target_var %in% c("B4","C6")) |
    (stunt_type == "LEFT SIDE A-C TWIST" & rk <= 2 & target_var %in% c("A1","C5")) |  
    (stunt_type == "RIGHT SIDE A-C TWIST" & rk <= 2 & target_var %in% c("A2","C6")) |   
    (stunt_type == "B-GAP SWITCH" & rk <= 2 & target_var %in% c("B3","B4")) |  
    (stunt_type == "C-GAP SWITCH" & rk <= 2 & target_var %in% c("C5","C6")) |
    (stunt_type == "LEFT B to OPP C GAP SWITCH" & rk <= 2 & target_var %in% c("B3","C6")) |     
    (stunt_type == "RIGHT B to OPP C GAP SWITCH" & rk <= 2 & target_var %in% c("B4","C5")) |
    (stunt_type == "LEFT A to OPP OUTSIDE SWITCH" & rk <= 2 & target_var %in% c("A1","B4","C6")) |
    (stunt_type == "RIGHT A to OPP OUTSIDE SWITCH" & rk <= 2 & target_var %in% c("A2","B3","C5")) | 
    (stunt_type == "OVERLOAD INSIDE STUNT" & rk <= 2 & target_var %in% c("A1","A2")) |   
    (stunt_type == "LEFT OVERLOAD OUTSIDE STUNT" & rk <= 2 & target_var %in% c("B3","C5")) |  
    (stunt_type == "RIGHT OVERLOAD OUTSIDE STUNT" & rk <= 2 & target_var %in% c("B4","C6")) |
    (stunt_type == "MULTI-GAP STUNT" & rk >= 3) |  
    (stunt_type == "MULTI-GAP STUNT - SINGLE OVERLOAD" & rk >= 3) |  
    (stunt_type == "MULTI-GAP STUNT - MULTI OVERLOAD" & rk >= 3)    
  ) %>%
  dplyr::mutate(x = x,
                y = y) %>%
  dplyr::group_by(newframeId,stunt_type,target_var,rk) %>%
  dplyr::summarise(x = mean(x,na.rm = T),
                   y = mean(y,na.rm = T)) %>%
  dplyr::filter(
    (stunt_type == "A-GAP SWITCH" & ((target_var == "A1" & rk == 1) | (target_var == "A2" & rk == 2))) |
    (stunt_type == "LEFT SIDE A-B TWIST" & ((target_var == "A1" & rk == 1) | (target_var == "B3" & rk == 2))) |
    (stunt_type == "RIGHT SIDE A-B TWIST" & ((target_var == "A2" & rk == 1) | (target_var == "B4" & rk == 2))) | 
    (stunt_type == "LEFT SIDE B-C TWIST" & ((target_var == "B3" & rk == 1) | (target_var == "C5" & rk == 2))) |
    (stunt_type == "RIGHT SIDE B-C TWIST" & ((target_var == "B4" & rk == 1) | (target_var == "C6" & rk == 2))) | 
    (stunt_type == "LEFT SIDE A-C TWIST" & ((target_var == "A1" & rk == 1) | (target_var == "C5" & rk == 2))) | 
    (stunt_type == "RIGHT SIDE A-C TWIST" & ((target_var == "A2" & rk == 1) | (target_var == "C6" & rk == 2))) |  
    (stunt_type == "B-GAP SWITCH" & ((target_var == "B3" & rk == 1) | (target_var == "B4" & rk == 2))) | 
    (stunt_type == "C-GAP SWITCH" & ((target_var == "C5" & rk == 1) | (target_var == "C6" & rk == 2))) | 
    (stunt_type == "LEFT B to OPP C GAP SWITCH" & ((target_var == "B3" & rk == 1) | (target_var == "C6" & rk == 2))) | 
    (stunt_type == "RIGHT B to OPP C GAP SWITCH" & ((target_var == "B4" & rk == 1) | (target_var == "C5" & rk == 2))) |
    (stunt_type == "LEFT A to OPP OUTSIDE SWITCH" & ((target_var == "A1" & rk == 1) | (target_var == "C6" & rk == 2))) |  ## just use one example
    (stunt_type == "RIGHT A to OPP OUTSIDE SWITCH" & ((target_var == "A2" & rk == 1) | (target_var == "C5" & rk == 2))) |  ### just use one example 
    (stunt_type == "OVERLOAD INSIDE STUNT" & ((target_var == "A1" & rk == 1) | (target_var == "A1" & rk == 2))) |
    (stunt_type == "LEFT OVERLOAD OUTSIDE STUNT" & ((target_var == "B3" & rk == 1) | (target_var == "B3" & rk == 2))) |   ### just one example
    (stunt_type == "RIGHT OVERLOAD OUTSIDE STUNT" & ((target_var == "B4" & rk == 1) | (target_var == "B4" & rk == 2))) |   ### just one example
    (stunt_type == "MULTI GAP STUNT" & ((target_var == "A1" & rk == 1) | (target_var == "B3" & rk == 2) | (target_var == "B4" & rk == 3))) |   ### just one example
    (stunt_type == "MULTI-GAP STUNT - SINGLE OVERLOAD" & ((target_var == "A1" & rk == 1) | (target_var == "A1" & rk == 2) | (target_var == "B4" & rk == 3))) |   ### just one example
    (stunt_type == "MULTI-GAP STUNT - MULTI OVERLOAD" & ((target_var == "A1" & rk == 1) | (target_var == "A1" & rk == 2) | (target_var == "B4" & rk == 3) | (target_var == "B4" & rk == 4)))
    ) %>%
  dplyr::rename(mean_x = x,
                mean_y = y) %>%
  ungroup() 



### distribution
mean_route %>%
  ggplot(aes(x=mean_y,color=stunt_type)) +
  geom_density() +
  theme(
    legend.position = "none"
  )


  

### check
chk<-mean_route %>%
  group_by(stunt_type) %>%
  summarize(max = max(newframeId),
            min = min(newframeId),
            n = n())



df<-df %>%
  dplyr::left_join(mean_route,by = c("frameId" = "newframeId",
                                     "stunt_type" = "stunt_type",
                                     "rk" = "rk",
                                     "target_var" = "target_var")) %>%
  group_by(newframeId,stunt_type) %>%
  fill(mean_x,mean_y,.direction = "downup") %>%
  ungroup()

df<-df %>%
  dplyr::left_join(stunt_type_metrics,by = c("stunt_type" = "stunt_type",
                                             "stunt_category" = "stunt_category"))

summary(df)


df %>%
  #filter(stunt_type != "NO STUNT") %>%
  group_by(stunt_category) %>%
  summarize(n = n()) %>%
  mutate( freq = n / sum(n)) %>%
  arrange(desc(n)) %>%
  head(12L)









#mean_route %>%
#  ggplot() +
  #geom_point(alpha = 0.01) +
  #geom_line(aes(x = mean_y,y=mean_x,group=rk,color=rk)) +
#  geom_smooth(aes(x = mean_y,y=mean_x,color = target_var)) +
#  facet_wrap(~stunt_type) +
#  theme_minimal()

### add in counts

g2<-stunt_type_metrics %>%
  mutate(stunt_category2 = case_when(
    stunt_category == "DOUBLE SWITCH" ~ "DOUBLE SWITCH (2)",
    stunt_category == "DOUBLE TWIST" ~ "DOUBLE TWIST (2)",
    stunt_category == "MULTI-GAP STUNT" ~ "MULTI-GAP STUNT (3+)",
    stunt_category == "MULTI-GAP STUNT" ~ "MULTI-GAP STUNT - MULTI OVERLOAD (3+)",
    stunt_category == "MULTI-GAP STUNT - MULTI OVERLOAD" ~ "MULTI-GAP STUNT - MULTI OVERLOAD (3+)",
    stunt_category == "MULTI-GAP STUNT - SINGLE OVERLOAD" ~ "MULTI-GAP STUNT - SINGLE OVERLOAD (3+)",
    stunt_category == "NO STUNT" ~ "NO STUNT (0)",
    stunt_category == "SINGLE OVERLOAD" ~ "SINGLE OVERLOAD (2)",
    stunt_category == "SOLO STUNT" ~ "SOLO STUNT (1)",
    TRUE ~ as.character(NA)
  )) %>%
  arrange(desc(Block_RTUE)) %>%
  ggplot() +
  geom_col(aes(x=reorder(stunt_type,Block_RTUE),y = Block_RTUE,group = stunt_category2,fill=stunt_category2)) +
  coord_flip() +
  theme(
    axis.text.x = element_text(size=7),
    axis.text.y = element_text(size=7),
    legend.text = element_text(size=6),
    legend.title = element_text(size=8),
    legend.position = "right",
    legend.justification = "top",
    legend.direction = "vertical"
  ) +
    scale_fill_manual(values = c("#56B4E9","#56B4E9","#E69F00","#E69F00","#E69F00",
                                 "#999999","#56B4E9","Dark Blue")) +
  labs(
    x = "",
    y = "",
    title = "Block Rate Under Expected by Stunt Type"
  )


#return(ggarrange(g2,g1,ncol = 1,widths = c(3,1),
#          heights = c(4,7)))


}








  
