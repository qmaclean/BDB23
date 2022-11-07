


bdb_stunt_sample_block<-function(){




require(tidyverse)
require(gganimate)
require(arrow)
require(zoo)
require(caret)


source("R/bdb_load_games.R")
source("R/bdb_load_pff_scouting_data.R")
source("R/bdb_load_plays.R")

tr_lite<-read_csv("data/src/week7.csv") %>%
  dplyr::filter(gameId == "2021102410",
                playId == "854")


tr<-read_parquet("data_processed/wk_add_metrics/wk7_add_metrics.parquet") %>%
  dplyr::filter(gameId == "2021102410",
                playId == "854") %>%
  mutate(
    RT = ifelse(closest_pass_block_position == 'RT',1,0),
    LT = ifelse(closest_pass_block_position == 'LT',1,0),
    RG = ifelse(closest_pass_block_position == 'RG',1,0),
    LG = ifelse(closest_pass_block_position == 'LG',1,0),
    C = ifelse(closest_pass_block_position == 'C',1,0)
  ) %>%
  group_by(gameId,playId,frameId,nflId1,possessionTeam,defensiveTeam) %>%
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
    pff_positionLinedUp = pff_positionLinedUp1[1],
    RT = sum(RT,na.rm = T),
    LT = sum(LT,na.rm = T),
    LG = sum(LG,na.rm = T),
    RG = sum(RG,na.rm = T),
    C = sum(C,na.rm = T)
  ) %>%
  ungroup() %>%
  dplyr::rename(nflId = nflId1)  %>%
  mutate(
    #RT_pct = rollsum(x = RT,10,align = "right",fill = NA) / frameId
    RT_pct = (cumsum(RT) / frameId),
    LT_pct = (cumsum(LT) / frameId),
    LG_pct = (cumsum(LG) / frameId),
    RG_pct = (cumsum(RG) / frameId),
    C_pct = (cumsum(C) / frameId)
    #RT_pct = sum(RT,na.rm = T) / n_distinct(frameId),
    #LT_pct = sum(LT,na.rm = T) / n_distinct(frameId),
    #LG_pct = sum(LG,na.rm = T) / n_distinct(frameId),
    #RG_pct = sum(RG,na.rm = T) / n_distinct(frameId),
    #C_pct  = sum(C,na.rm = T) / n_distinct(frameId),
  ) %>%
  ungroup()


tr<-tr %>%
  mutate(
    snap_dis_from_los = ifelse(is.na(snap_dis_from_los),0,snap_dis_from_los),
    separation_from_snap_position = ifelse(is.na(separation_from_snap_position),0,separation_from_snap_position),
    separation_from_los_x = ifelse(is.na(separation_from_los_x),0,separation_from_los_x),
    left_side_sep_from_snap = ifelse(is.na(left_side_sep_from_snap),0,left_side_sep_from_snap),
    right_side_sep_from_snap = ifelse(is.na(right_side_sep_from_snap),0,right_side_sep_from_snap),
    forward_sep_from_snap = ifelse(is.na(forward_sep_from_snap),0,forward_sep_from_snap),
    forward_sep_dis_time = ifelse(is.na(forward_sep_dis_time),0,forward_sep_dis_time),
    backward_sep_from_snap = ifelse(is.na(backward_sep_from_snap),0,backward_sep_from_snap),
    closest_down_edge_separation = ifelse(is.na(closest_down_edge_separation),0,closest_down_edge_separation),
    closest_up_edge_separation = ifelse(is.na(closest_up_edge_separation),0,closest_up_edge_separation),
    closest_dt_separation = ifelse(is.na(closest_dt_separation),0,closest_dt_separation),
    qb_separation = ifelse(is.na(qb_separation),0,qb_separation),
    qb_dir_diff = ifelse(is.na(qb_dir_diff),0,qb_dir_diff),
    snap_separation = ifelse(is.na(snap_separation),0,snap_separation)
  ) %>%
  dplyr::group_by(gameId,playId,nflId) %>%
  dplyr::mutate(
    dis_los_x_max = ifelse(is.na(dis_los_x_max),0,dis_los_x_max),
    frame_cross_los = ifelse(is.na(frame_cross_los),0,frame_cross_los),
    cross_los_bool = ifelse(is.na(cross_los_bool),0,cross_los_bool)
  ) %>%
  ungroup()





##### GAP PREDICTION
gap_model<-readRDS("model_objects/active/gap_rf_model_opt_final.rds")

target_var<-predict(gap_model$finalModel,tr,type="response")

length(target_var$predictions)

tr$target_var<-target_var$predictions


tr<-tr %>%
  dplyr::mutate(
    A1_prob = target_var[,"A1"],
    A2_prob = target_var[,"A2"],
    B3_prob = target_var[,"B3"],
    B4_prob = target_var[,"B4"],
    C5_prob = target_var[,"C5"],
    C6_prob = target_var[,"C6"]
  ) %>%
  dplyr::select(-target_var) 

max<-tr %>%
  dplyr::select(A1_prob,A2_prob,B3_prob,B4_prob,C5_prob,C6_prob) %>%
  mutate(
    max = names(.)[max.col(.)]
  ) %>%
  dplyr::select(max)

tr<-cbind(tr,max)

tr<-tr %>%
  mutate(
    target_var = case_when(
      max == "A1_prob" ~ "A1",
      max == "A2_prob" ~ "A2",
      max == "B3_prob" ~ "B3",
      max == "B4_prob" ~ "B4",
      max == "C5_prob" ~ "C5",
      max == "C6_prob" ~ "C6",
      TRUE ~ as.character(NA)
    )
  ) %>%
  dplyr::select(-A1_prob,-A2_prob,-B3_prob,-B4_prob,-C5_prob,-C6_prob,-max)


#### BLOCK PREDICTION
block_model<-readRDS("model_objects/active/expected_block_model_rf.rds")
block_predictions<-predict(block_model$finalModel,tr,type="response")

tr$xBlock_predictions<-block_predictions$predictions

tr<-tr %>%
  dplyr::mutate(
    A_prob = xBlock_predictions[,"A"],
    B_prob = xBlock_predictions[,"B"]
  ) %>%
  dplyr::select(-xBlock_predictions) 

maxb<-tr %>%
  dplyr::select(A_prob,B_prob) %>%
  mutate(
    max = names(.)[max.col(.)]
  ) %>%
  dplyr::select(max)

# get class predictions? 

tr<-cbind(tr,maxb)

tr<-tr %>%
  mutate(
    xBlock = case_when(
      max == "A_prob" ~ "Yes",
      max == "B_prob" ~ "No",
      TRUE ~ as.character(NA)
    )
  ) %>%
  dplyr::select(-max) 

tr<-tr %>%
  dplyr::select(gameId,playId,frameId,nflId,target_var,A_prob,B_prob,xBlock)

example_play<-tr_lite %>%
  dplyr::left_join(tr,by=c("gameId" = "gameId",
                           "playId" = "playId",
                           "nflId" = "nflId",
                           "frameId" = "frameId")) #%>%
  #dplyr::select(-team.y) %>%
  #rename(team = team.x) 

#games<-bdb_load_games() %>%
#  dplyr::select(gameId,week)

#example_play<-example_play %>%
#  dplyr::left_join(games,by=c("gameId"))

#plays<-bdb_load_plays()

plays_join<-bdb_load_plays() %>%
  dplyr::select(gameId,playId,possessionTeam,defensiveTeam,down,yardsToGo,absoluteYardlineNumber,offenseFormation,
                personnelO,defendersInBox,personnelD,pff_passCoverage,pff_passCoverageType)

pff<-bdb_load_pff_data() %>%
  dplyr::select(gameId,playId,nflId,pff_positionLinedUp)

example_play<-example_play %>%
  dplyr::left_join(plays_join,by=c("gameId" = "gameId",
                                   "playId" = "playId")) %>%
  dplyr::left_join(pff,by=c("gameId" = "gameId",
                            "playId" = "playId",
                            "nflId" = "nflId"))




rm(gap_model)
rm(tr)
rm(tr_lite)
rm(target_var)
rm(block_model)
rm(block_predictions)
rm(max)
rm(maxb)
rm(plays_join)
rm(pff)

#xblock<-read_parquet("expected_block_results.parquet")
#xpressure<-read_parquet("expected_pressure_results.parquet")
#stunts<-read_parquet("line_stunts.parquet")


#example_play<-example_play %>%
#  dplyr::select(gameId,playId,nflId,frameId,jerseyNumber,
#                team,x,y,s,a,dis,o,event,possessionTeam,defensiveTeam,target_var,A_prob,
#                B_prob,xBlock,yardsToGo,absoluteYardlineNumber) %>%
#  ungroup()


example_play<-example_play %>%
  mutate(target_var = ifelse(pff_positionLinedUp %in%
                               c("DLT","LOLB","RE","REO",
                                 "ROLB","NT","DRT","LE","LEO"),target_var,
                             NA),
         A_prob = ifelse(pff_positionLinedUp %in%
                           c("DLT","LOLB","RE","REO",
                             "ROLB","NT","DRT","LE","LEO"),A_prob,
                         NA),
         B_prob = ifelse(pff_positionLinedUp %in%
                           c("DLT","LOLB","RE","REO",
                             "ROLB","NT","DRT","LE","LEO"),B_prob,
                         NA),
         xBlock = ifelse(pff_positionLinedUp %in%
                           c("DLT","LOLB","RE","REO",
                             "ROLB","NT","DRT","LE","LEO"),xBlock,
                         NA)
         ) %>%
  dplyr::filter(frameId <= 25)

#a<-example_play %>%
#  group_by(pff_positionLinedUp,target_var) %>%
#  summarize(n = n())
  
#### insert player ids


url.logo <- getURL("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
df.logos <- read.csv(text = url.logo) %>%
  dplyr::filter(team_code %in% c("SF","IND"))
  
  #field_color = #006400
# #73af34
  
  plot_field <- function(field_color="#3BD23D", line_color = "#212529", number_color = "#ffffff") {
    field_height <- 160/3
    field_width <- 120
    
    field <- ggplot() +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(hjust = 1),
        legend.position = "bottom",
        legend.title.align = 1,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.line = element_blank(),
        panel.background = element_rect(fill = field_color, color = "white"),
        panel.border = element_blank(),
        aspect.ratio = field_height/field_width
      ) +
      # major lines
      annotate(
        "segment",
        x = c(0, 0, 0,field_width, seq(10, 110, by=5)),
        xend = c(field_width,field_width, 0, field_width, seq(10, 110, by=5)),
        y = c(0, field_height, 0, 0, rep(0, 21)),
        yend = c(0, field_height, field_height, field_height, rep(field_height, 21)),
        colour = line_color
      ) +
      # hashmarks
      annotate(
        "segment",
        x = rep(seq(10, 110, by=1), 4),
        xend = rep(seq(10, 110, by=1), 4),
        y = c(rep(0, 101), rep(field_height-1, 101), rep(160/6 + 18.5/6, 101), rep(160/6 - 18.5/6, 101)),
        yend = c(rep(1, 101), rep(field_height, 101), rep(160/6 + 18.5/6 + 1, 101), rep(160/6 - 18.5/6 - 1, 101)),
        colour = line_color
      ) +
      # yard numbers
      annotate(
        "text",
        x = seq(20, 100, by = 10),
        y = rep(12, 9),
        label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
        size = 10,
        colour = number_color,
      ) +
      # yard numbers upside down
      annotate(
        "text",
        x = seq(20, 100, by = 10),
        y = rep(field_height-12, 9),
        label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
        angle = 180,
        size = 10,
        colour = number_color, 
      ) 
    
    return(field)
  }
  
  
  
  
  df_colors = data.frame(home_1 = '#B3995D',
                         home_2 = '#AA0000',
                         away_2 = '#002C5F',
                         away_1 = '#A2AAAD')
  


  example_play<-example_play %>%
    dplyr::mutate(B_prob_size = B_prob * 30)

  
    
 play_frames<-  plot_field() + 
    # line of scrimmage
    annotate(
      "segment",
      x = example_play$absoluteYardlineNumber, xend = example_play$absoluteYardlineNumber, y = 0, yend = 160/3,
      colour = "#0d41e1", size = 1.5
    ) +  # first down marker
    annotate(
      "segment",
      x = (example_play$absoluteYardlineNumber +  example_play$yardsToGo), xend = (example_play$absoluteYardlineNumber + example_play$yardsToGo), y = 0, yend = 160/3,
      colour = "#f9c80e", size = 1.5
    ) +  # away team velocities
      geom_segment(
        data = example_play %>% dplyr::filter(team == possessionTeam
                                              ),
        mapping = aes(x = x, y = y, xend = x, yend = y),
        colour = df_colors$away_2, size = 8, alpha = 0.5,arrow = arrow(length = unit(0.2, "cm"))
        
      ) +
      geom_segment(
        data = example_play %>% dplyr::filter(team == defensiveTeam),
        mapping = aes(x = x, y = y, xend = x, yend = y),
        colour = df_colors$home_2, size = 8, alpha = 0.5,arrow = arrow(length = unit(0.2, "cm"))
      ) +
      geom_point(
        data = example_play %>% dplyr::filter(team == possessionTeam
                                              ),
        mapping = aes(x = x, y = y),
        fill = "#f8f9fa", colour = df_colors$away_2,
        shape = 21, alpha = 0.7, size = 8, stroke = 1.5
      ) +
      geom_point(
        data = example_play %>% dplyr::filter(team == defensiveTeam,
                                              is.na(target_var)),
        mapping = aes(x = x, y = y),
        fill = "#f8f9fa", colour = df_colors$home_2,
        shape = 21, alpha = 0.7, size = 8, stroke = 1.5
      ) +
      geom_point(
        data = example_play %>% dplyr::filter(team == defensiveTeam,
                                              complete.cases(target_var)),
        mapping = aes(x = x, y = y,color = target_var
                      #size = B_prob_size
                      ),
        fill = "#f8f9fa", 
        shape = 21, alpha = 0.7, stroke = 1.5, size = 10
      ) +
      geom_text(
        data = example_play %>% dplyr::filter(team == possessionTeam),
        mapping = aes(x = x, y = y, label = jerseyNumber),
        colour = df_colors$away_1, size = 3
      ) +
      geom_text(
        data = example_play %>% dplyr::filter(team == defensiveTeam),
        mapping = aes(x = x, y = y, label = jerseyNumber),
        colour = df_colors$home_1, size = 3 
      ) +
      geom_point(
        data = example_play %>% dplyr::filter(team == "football"),
        mapping = aes(x = x, y = y),
        fill = "#935e38", colour = "#d9d9d9",
        shape = 21, alpha = 0.7, size = 6, stroke = 1
      ) +
       transition_time(frameId) +
      ease_aes('linear') +
      NULL
    
      

    

    # home team locs and jersey numbers

    

  
  
  
  play_length <- length(unique(example_play$frameId))
  
  
  
  play_anim <- animate(
    play_frames,
    fps = 5, 
    nframes = play_length,
    width = 800,
    height = 400,
    end_pause = 0
  )
  
  
  return(play_anim)
  
  
}



#bdb_stunt_sample_block()

