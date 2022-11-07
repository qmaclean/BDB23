
plays<-bdb_load_plays()

bdb_visualize_sample_stunts<-function(){
  
require(gganimate)
require(cowplot)
require(tidyverse)
require(arrow)


df<-read_parquet("downline_tracking.parquet")
downline_df<-read_parquet("final_df.parquet")
stunts<-read_parquet("line_stunts.parquet")

downline_df<-downline_df %>%
inner_join(stunts,by=c("gameId" = "gameId",
                       "playId" = "playId")) 

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

unassigned<-downline_df %>%
  dplyr::filter(assigned == 0) %>%
  dplyr::filter(pff_positionLinedUp %in% c('DLT','LEO','ROLB','DRT','RE','DLT','REO',
                                           'LE','NLT','NT','NRT','LOLB')) %>%
  dplyr::select(gameId,playId,nflId,assigned,target_var) %>%
  dplyr::group_by(gameId,playId) %>%
  #dplyr::arrange(target_var) %>%
  mutate(rk = row_number())




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
  ungroup() %>%
  left_join(unassigned,by=c("gameId" = "gameId",
                            "playId" = "playId",
                            "nflId" = "nflId",
                            "target_var" = "target_var"))






#### score each play 
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3


## Specific boundaries for a given play
ymin <- max(round(min(df$x - df$min_x, na.rm = TRUE) - 0, -1), 0)
ymax <- min(round(max(df$x - df$min_x, na.rm = TRUE) + 0, -1), 120)  #120
df_hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (0:110))  #:110
df_hash <- df_hash %>% filter(!(floor(y %% 5) == 0))
df_hash <- df_hash %>% filter(y < ymax, y > ymin)

theme_set(theme_minimal())
theme_update(
  #text = element_text(family = "mono",size=14),
  #plot.title = element_text('Courier', face = 'bold', size = 12, color = 'gray20'),
  plot.title.position = 'plot',
  #plot.subtitle = element_text('mono', face = 'bold', size = 10, color = 'gray50'),
  #axis.text = element_text(size = 8),
  #axis.ticks.x = element_blank(),
  #axis.text.x = element_markdown(margin = margin(t = -25,unit = "pt")),
  # axis.title = element_text(size = 24, face = 'bold'),
  #axis.title.y = element_text(size = 14, face = 'bold', hjust = 0.5),
  #axis.title.x = element_blank(),
  plot.background = element_rect(fill = '#FFFFFF', color = NA),
  #panel.background = element_rect(fill = '#FFFEF2', color = NA),
  plot.tag = element_text('mono', size = 10, color = 'gray20', hjust = 0), 
  plot.tag.position = c(.01, 0.02),
  legend.position = "none"
)

#play_dir<-df %>%
#  dplyr::select(gameId,playId,playDirection) %>%
#  distinct()


#check<-downline_df %>%
#  dplyr::filter(
#                stunt_type == "C-GAP SWITCH"#,
#                #assigned == 0
#                #pff_positionLinedUp %in% c("DRT","DLT","NT")
#                ) %>%
#  dplyr::group_by(gameId,playId) %>%
#  summarize(n = n_distinct(nflId),
#            mean_var_dir_y = mean(mean_var_dir_y,na.rm = T),
#            dis = sum(sum_dis)) %>%
#  dplyr::left_join(play_dir,by = c("gameId" = "gameId",
#                                   "playId" = "playId"))



  


sample_plays<-df %>%
  dplyr::filter(
    (gameId == 	2021092300 & playId == 3263) | # A - GAP SWITCH EXAMPLE
     # (gameId == 	2021092608 & playId == 1998) | # B - GAP SWITCH EXAMPLE
      (gameId == 	2021101012 & playId == 3590)  | # B-GAP SWITCH 
      (gameId == 	2021100304 & playId == 2868) | #LEFT A to OPP OUTSIDE SWITCH  
      (gameId == 2021101710 & playId == 2499) | # LEFT A-GAP INSIDE SOLO STUNT
      (gameId == 	2021091904 & playId == 1058) | # LEFT B to OPP C GAP SWITCH 
      (gameId == 	2021092612 & playId == 356) | #LEFT OUTSIDE SOLO STUNT
      (gameId == 	2021091201 & playId == 2709) | #LEFT OVERLOAD OUTSIDE STUNT 
    (gameId == 2021091204 & playId == 90) | #C-GAP SWITCH  -- CHANGE
    (gameId == 	2021101800 & playId == 3914) |  #LEFT SIDE A-B TWIST
    (gameId == 2021102401 & playId == 2029) | # LEFT SIDE A-C TWIST
    (gameId == 2021100302 & playId == 580 ) | # LEFT SIDE B-C TWIST 
    (gameId == 	2021102404 & playId == 3102) | # MULTI-GAP STUNT
    (gameId == 	2021101006 & playId == 2631 ) | #MULTI-GAP STUNT - MULTI OVERLOAD 
    (gameId == 2021101002 & playId == 1929) | #MULTI-GAP STUNT - SINGLE OVERLOAD
    (gameId == 2021091207 & playId == 1747) | #OVERLOAD INSIDE STUNT
    (gameId == 	2021091201 & playId == 4011)| #RIGHT A to OPP OUTSIDE SWITCH
    (gameId == 		2021101002 & playId == 1717) | #RIGHT A-GAP INSIDE SOLO STUNT
    (gameId ==  	2021091210 & playId == 1584) | #RIGHT B to OPP C GAP SWITCH -- doesn't look as accurate but limited options
    (gameId == 	2021102410 & playId == 3041 ) | #RIGHT OUTSIDE SOLO STUNT 
    (gameId == 	2021103107 & playId == 2575) | # RIGHT OVERLOAD OUTSIDE STUNT
    (gameId == 	2021102410 & playId == 854) | # RIGHT SIDE A-B TWIST
    (gameId == 2021102500 & playId == 2934) | # RIGHT SIDE A-C TWIST
    (gameId == 2021100307 & playId == 4843) # RIGHT SIDE B-C TWIST
  ) %>% 
  dplyr::filter(newframeId <= 30) %>%
  dplyr::mutate(
    to_left = ifelse(playDirection == "left",1,0),
    x=ifelse(to_left == 1,120-x,x),
    y=ifelse(to_left == 1,160/3 - y,y)
  )

snap<-sample_plays %>%
  group_by(gameId,playId,nflId) %>%
  dplyr::filter(newframeId == 1) %>%
  dplyr::select(gameId,playId,nflId,x,y) %>%
  dplyr::rename(snap_x = x,
                snap_y = y)

sample_plays<-sample_plays %>%
  dplyr::left_join(snap,by = c("gameId" = "gameId",
                               "playId" = "playId",
                               "nflId" = "nflId"))


###### do I add stationary lineman to indicate gaps? 

play_frames<-sample_plays %>%
  ggplot() +
  #scale_size_manual(values = c(6, 4, 6), guide = "none") + 
  #scale_shape_manual(values = c(21, 16, 21), guide = "none") +
  scale_fill_manual(values = c("#e31837", "#654321", "#002244"), guide = "none") + 
  #scale_colour_manual(values = c("black", "#654321", "#c60c30"), guide = "none") + 
  annotate("text", x = df_hash$x[df_hash$x > 55/2], 
           y = df_hash$y[df_hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) +
  annotate("segment", x = xmin, 
           y = seq(max(0, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(0, ymin), min(ymax, 110), by = 5)) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "white") +
  #geom_segment(
    #data = example_play %>% dplyr::filter(team == possessionTeam),
  #  mapping = aes(x = (snap_y - y), y = (snap_x - x), xend = (snap_y - y), yend = (snap_x - x),
  #                color = target_var),
  #  size = 5, arrow = arrow(length = unit(0.2, "cm"),ends="last")
  #) +
  geom_point(aes(x = ((xmax)- y),y = (snap_x - (x)),color = target_var)) +
  scale_color_manual(labels = c("A1","A2","B3","B4","C5","C6"),
                     values = c("dark blue","orange","#0072B2","grey","red","purple")) +
  ylim(ymin,ymax) +
  theme(
    #panel.background = element_rect(fill = "#006400"), # dark green
    #panel.background = element_rect(fill = "#7eaf34"),
    #panel.background = element_rect(fill = "white"),
    #panel.background = element_rect(fill = "#85b034"),
    panel.background = element_rect(fill = "#3BD23D"),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    strip.text = element_text(size = 6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.position = "left",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "top",
    legend.justification = "left",
    legend.direction = "horizontal"
  ) +
  labs(
    x = "",
    y = "",
    title = "Stunt Types Classified (Sample Plays)",
    color = "Stunt Gap",
    caption = "Movement identified in first 30 frames or 3 secs from snap"
  ) +
  facet_wrap(~stunt_type,ncol = 4,
             labeller = labeller(groupwrap = label_wrap_gen(15))) +
  guides(col = guide_legend(ncol = 6)) +
  transition_time(newframeId) +
  shadow_mark() +
  ease_aes('linear') +
  NULL

play.length.ex<-length(unique(sample_plays$newframeId))

return(animate(play_frames,fps = 10,nframe = play.length.ex))

}






  













