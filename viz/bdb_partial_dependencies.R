

bdb_partial_dependence<-function(){

library(tidyverse)
library(arrow)
library(patchwork)
library(caret)



df<-read_parquet("final_df.parquet")
eb<-read_parquet("expected_block_results.parquet")
pr<-read_parquet("expected_pressure_results.parquet")
rf<-readRDS("model_objects/active/expected_block_model_rf.rds")

rf_varImp_bl<-read.csv("expected_block_importance.csv")
rf_varImp_pr<-read.csv("expected_pressure_importance.csv")

df<-df %>%
  dplyr::left_join(eb,by=c("gameId" = "gameId",
                           "playId" = "playId",
                           "nflId" = "nflId"))

df<-df %>%
  dplyr::left_join(pr,by=c("gameId" = "gameId",
                           "playId" = "playId",
                           "nflId" = "nflId"))


#https://stats.stackexchange.com/questions/50560/how-to-calculate-partial-dependence-when-i-have-4-predictors



df<-df %>%
  mutate(closest_pass_block_separation_grp = cut(closest_pass_block_separation, 
                                                 seq(from = 0, to = 13, by = 0.1)),
         closest_pass_block_tackle_separation_grp = cut(closest_pass_block_tackle_separation, 
                                                        seq(from = 0, to = 13, by = 0.1)),
         closest_pass_block_guard_separation_grp = cut(closest_pass_block_guard_separation, 
                                                        seq(from = 0, to = 13, by = 0.1)),
         qb_separation_grp = cut(qb_separation,
                                 seq(from = 0,to = 13,by = 0.1)),
         sum_dis_grp = cut(sum_dis,
                           seq(from = 0,to = 150,by = 5)),
         separation_from_los_x_grp = cut(separation_from_los_x,
                               seq(from = 0, to = 13,by = 0.1))
         ) 

pd_var1<-df %>%
  group_by(closest_pass_block_separation_grp) %>%
  summarize(cpbs_mean = mean(A_prob,na.rm = T),
            cpbs_sd = sd(A_prob,na.rm = T)) %>%
  tidyr::fill(cpbs_sd,.direction = "downup")

pd_var2<-df %>%
  group_by(closest_pass_block_tackle_separation_grp) %>%
  summarize(cpbts_mean = mean(A_prob,na.rm = T),
            cpbts_sd = sd(A_prob,na.rm = T)) %>%
  tidyr::fill(cpbts_sd,.direction = "downup")

pd_var3<-df %>%
  group_by(closest_pass_block_guard_separation_grp) %>%
  summarize(cpbgs_mean = mean(A_prob,na.rm = T),
            cpbgs_sd = sd(A_prob,na.rm = T)) %>%
  tidyr::fill(cpbgs_sd,.direction = "downup")

pd_var4<-df %>%
  group_by(qb_separation_grp) %>%
  summarize(qbs_mean = mean(A_Pressure_prob,na.rm = T),
            qbs_sd = sd(A_Pressure_prob,na.rm = T)) %>%
  tidyr::fill(qbs_sd,.direction = "downup")

pd_var5<-df %>%
  group_by(sum_dis_grp) %>%
  summarize(sd_mean = mean(A_Pressure_prob,na.rm = T),
            sd_sd = sd(A_Pressure_prob,na.rm = T)) %>%
  tidyr::fill(sd_sd,.direction = "downup")

pd_var6<-df %>%
  group_by(separation_from_los_x_grp) %>%
  summarize(ssg_mean = mean(A_Pressure_prob,na.rm = T),
            ssg_sd = sd(A_Pressure_prob,na.rm = T)) %>%
  tidyr::fill(ssg_sd,.direction = "downup")


df<-df %>%
  dplyr::left_join(pd_var1,by = c("closest_pass_block_separation_grp" = "closest_pass_block_separation_grp")) %>%
  mutate(A_prob_cpbs_uppr = ifelse(A_prob + cpbs_sd > 1,1,A_prob + cpbs_sd),
         A_prob_cpbs_lwr = ifelse(A_prob - cpbs_sd < 0,0,A_prob - cpbs_sd)) %>%
  dplyr::left_join(pd_var2,by = c("closest_pass_block_tackle_separation_grp" = "closest_pass_block_tackle_separation_grp")) %>%
  mutate(A_prob_cpbts_uppr = ifelse(A_prob + cpbts_sd > 1,1,A_prob + cpbts_sd),
         A_prob_cpbts_lwr = ifelse(A_prob - cpbts_sd < 0,0,A_prob - cpbts_sd)) %>%
  dplyr::left_join(pd_var3,by = c("closest_pass_block_guard_separation_grp" = "closest_pass_block_guard_separation_grp")) %>% 
  mutate(A_prob_cpbgs_uppr = ifelse(A_prob + cpbgs_sd > 1,1,A_prob + cpbgs_sd),
         A_prob_cpbgs_lwr = ifelse(A_prob - cpbgs_sd < 0,0,A_prob - cpbgs_sd)) %>%
  dplyr::left_join(pd_var4,by = c("qb_separation_grp" = "qb_separation_grp")) %>%
  mutate(A_Pressure_prob_qbs_uppr = ifelse(A_Pressure_prob + qbs_sd > 1,1,A_Pressure_prob + qbs_sd),
         A_Pressure_prob_qbs_lwr = ifelse(A_Pressure_prob - qbs_sd < 0,0,A_Pressure_prob - qbs_sd)) %>%
  dplyr::left_join(pd_var5,by=c("sum_dis_grp" = "sum_dis_grp")) %>%
  mutate(A_Pressure_prob_sd_uppr = ifelse(A_Pressure_prob + sd_sd > 1,1,A_Pressure_prob + sd_sd),
         A_Pressure_prob_sd_lwr = ifelse(A_Pressure_prob - sd_sd < 0,0,A_Pressure_prob - sd_sd)) %>%
  dplyr::left_join(pd_var6,by=c("separation_from_los_x_grp" = "separation_from_los_x_grp")) %>%
  mutate(A_Pressure_prob_ssg_uppr = ifelse(A_Pressure_prob + ssg_sd > 1,1,A_Pressure_prob + ssg_sd),
         A_Pressure_prob_ssg_lwr = ifelse(A_Pressure_prob - ssg_sd < 0,0,A_Pressure_prob - ssg_sd)) 



g1<- df %>%
  dplyr::filter(closest_pass_block_separation <= 10,
                closest_pass_block_separation > 0) %>%
  ggplot() + 
  geom_smooth(aes(x = closest_pass_block_separation,y = A_prob),method = "loess",se = F,color = "#004d25") +
  geom_smooth(aes(x = closest_pass_block_separation,y = A_prob_cpbs_uppr),method = "loess",se= F,linetype = "dotted",color = "#004d25") +
  geom_smooth(aes(x = closest_pass_block_separation,y = A_prob_cpbs_lwr),method = "loess",se = F,linetype = "dotted",color= "#004d25") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8, face = 'bold', hjust = 0.5),
    axis.title = element_text(size = 8, face = 'bold', hjust = 0.5)
  ) + 
  labs(x = "Closest O-Line Separation (Yds)",
       y = "Block Probability",
       title = "Block Probability Model") +
  ylim(0,1)


  

g2<-df %>%
  dplyr::filter(closest_pass_block_tackle_separation <= 10,
                closest_pass_block_tackle_separation > 0) %>%
  ggplot() + 
  geom_smooth(aes(x = closest_pass_block_tackle_separation,y = A_prob),method = "loess",se = F,color = "#11823b") +
  geom_smooth(aes(x = closest_pass_block_tackle_separation,y = A_prob_cpbts_uppr),method = "loess",se= F,linetype = "dotted",color = "#11823b") +
  geom_smooth(aes(x = closest_pass_block_tackle_separation,y = A_prob_cpbts_lwr),method = "loess",se = F,linetype = "dotted",color = "#11823b") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8, face = 'bold', hjust = 0.5),
    axis.title = element_text(size = 8, face = 'bold', hjust = 0.5)
  ) + 
  labs(x = "Closest Off. Tackle Separation (Yds)",
       y = "Block Probability") +
  ylim(0,1)


g3<-df %>%
  dplyr::filter(closest_pass_block_guard_separation <= 10,
                closest_pass_block_guard_separation > 0) %>%
  ggplot() + 
  geom_smooth(aes(x = closest_pass_block_guard_separation,y = A_prob),method = "loess",se = F,color = "#48bf53") +
  geom_smooth(aes(x = closest_pass_block_guard_separation,y = A_prob_cpbgs_uppr),method = "loess",se= F,linetype = "dotted",color = "#48bf53") +
  geom_smooth(aes(x = closest_pass_block_guard_separation,y = A_prob_cpbgs_lwr),method = "loess",se = F,linetype = "dotted",color = "#48bf53") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8, face = 'bold', hjust = 0.5),
    axis.title = element_text(size = 8, face = 'bold', hjust = 0.5)
  ) + 
  labs(x = "Closest Off. Guard Separation (Yds)",
       y = "Block Probability") +
  ylim(0,1)

g4<-rf_varImp_bl %>%
  mutate(
    var = case_when(
      var == "closest_pass_block_separation" ~ "Closest O-Line Separation Avg",
      var == "closest_pass_block_tackle_separation" ~ "Closest Off. Tackle Separation Avg",
      var == "mean_var_s" ~ "Speed Variance",
      var == "closest_pass_block_guard_separation" ~ "Closest Off. Guard Separation Avg",
      var == "separation_from_los_x" ~ "Line of Scrimmage Separation Avg",
      var == "closest_up_edge_separation" ~ "Closest Up Edge Separation Avg",
      var == "mean_var_x" ~ "X Position Variance",
      var == "mean_var_y" ~ "Y Position Variance",
      var == "separation" ~ "Closest Player Separation Avg",
      var == "forward_sep_dis_time" ~ "Time Spent Moving Forward",
      var == "snap_separation" ~ "Separation at Snap",
      var == "closest_down_edge_separation" ~ "Closest Down Edge Separation Avg",
      var == "snap_dis_from_los" ~ "Distance from LOS at Snap",
      var == "mean_var_dir_x" ~ "X Direction Variance",
      var == "RG_pct" ~ "Percentage of play RG was closest player",
      var == "pre_play_dis" ~ "Distance Travelled (Pre-Play)",
      var == "qb_separation" ~ "QB Separation Avg",
      var == "closest_dt_separation" ~ "DT Separation Avg",
      var == "mean_var_dir_y" ~ "Y Direction Variance",
      var == "closest_pass_block_player_dir_diff" ~ "Closest O-Line Direction Difference Avg",
      var == "mean_var_s_theta" ~ "Angular Speed Variance",
      var == "separation_from_snap_position" ~ "Euclidean Separation from position at Snap Avg",
      var == "LG_pct" ~ "Percentage of play LG was closest player",
      var == "qb_dir_diff" ~ "QB Direction Difference Avg",
      var == "LT_pct" ~ "Percentage of play LT was closest player",
      var == "mean_var_dir" ~ "Direction Varaince",
      var == "RT_pct" ~ "Percentage of play RT was closest player",
      var == "C_pct" ~ "Percentage of play C was closest player",
      TRUE ~ as.character(var)
    )
  ) %>%
  head(15L) %>%
  ggplot() +
  aes(x = reorder(var,rel.inf),y = rel.inf,fill = rel.inf) +
  geom_bar(stat = "identity",position = "dodge") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8, face = 'bold', hjust = 0.5),
    axis.title = element_text(size = 8, face = 'bold', hjust = 0.5)
  ) + 
  guides(fill = "none") +
  scale_fill_gradient2(low = "#48bf53",mid = "#11823b",high = "#004d25",
                       midpoint = 10) +
  labs(
    x = "Variables",
    y = "Relative Influence",
    title = "Top 15 Variables"
  )

g5<-df %>%
  dplyr::filter(sum_dis <= 150,
                sum_dis > 0) %>%
  ggplot() + 
  geom_smooth(aes(x = sum_dis,y = A_Pressure_prob),method = "loess",se = F,color = "#ffd7b5") +
  geom_smooth(aes(x = sum_dis,y = A_Pressure_prob_sd_uppr),method = "loess",se= F,linetype = "dotted",color = "#ffd7b5") +
  geom_smooth(aes(x = sum_dis,y = A_Pressure_prob_sd_lwr),method = "loess",se = F,linetype = "dotted",color = "#ffd7b5") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8, face = 'bold', hjust = 0.5),
    axis.title = element_text(size = 8, face = 'bold', hjust = 0.5)
  ) + 
  labs(x = "Distance Travelled (Yds)",
       y = "Pressure Probability") +
  ylim(0,1)


g6<-df %>%
  dplyr::filter(qb_separation <= 10,
                qb_separation > 0) %>%
  ggplot() + 
  geom_smooth(aes(x = qb_separation,y = A_Pressure_prob),method = "loess",se = F,color = "#ff9248") +
  geom_smooth(aes(x = qb_separation,y = A_Pressure_prob_qbs_uppr),method = "loess",se= F,linetype = "dotted",color = "#ff9248") +
  geom_smooth(aes(x = qb_separation,y = A_Pressure_prob_qbs_lwr),method = "loess",se = F,linetype = "dotted",color = "#ff9248") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8, face = 'bold', hjust = 0.5),
    axis.title = element_text(size = 8, face = 'bold', hjust = 0.5)
  ) + 
  labs(x = "QB Separation (Yds)",
       y = "Pressure Probability",
       title = "Pressure Probability Model") +
  ylim(0,1)





g7<-df %>%
  dplyr::filter(separation_from_los_x <= 10,
                separation_from_los_x > 0) %>%
  ggplot() + 
  geom_smooth(aes(x = separation_from_los_x,y = A_Pressure_prob),method = "loess",se = F,color = "#ffd7b5") +
  geom_smooth(aes(x = separation_from_los_x,y = A_Pressure_prob_ssg_uppr),method = "loess",se= F,linetype = "dotted",color = "#ffd7b5") +
  geom_smooth(aes(x = separation_from_los_x,y = A_Pressure_prob_ssg_lwr),method = "loess",se = F,linetype = "dotted",color = "#ffd7b5") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8, face = 'bold', hjust = 0.5),
    axis.title = element_text(size = 8, face = 'bold', hjust = 0.5)
  ) + 
  labs(x = "Separation from LOS (Yds)",
       y = "Pressure Probability") +
  ylim(0,1)


 
g8<-rf_varImp_pr %>%
  mutate(
    rowlabels = case_when(
      rowlabels == "closest_pass_block_separation" ~ "Closest O-Line Separation Avg",
      rowlabels == "closest_pass_block_tackle_separation" ~ "Closest Off. Tackle Separation Avg",
      rowlabels == "mean_var_s" ~ "Speed Variance",
      rowlabels == "closest_pass_block_guard_separation" ~ "Closest Off. Guard Separation Avg",
      rowlabels == "separation_from_los_x" ~ "Line of Scrimmage Separation Avg",
      rowlabels == "closest_up_edge_separation" ~ "Closest Up Edge Separation Avg",
      rowlabels == "mean_var_x" ~ "X Position Variance",
      rowlabels == "mean_var_y" ~ "Y Position Variance",
      rowlabels == "separation" ~ "Closest Player Separation Avg",
      rowlabels == "forward_sep_dis_time" ~ "Time Spent Moving Forward",
      rowlabels == "snap_separation" ~ "Separation at Snap",
      rowlabels == "closest_down_edge_separation" ~ "Closest Down Edge Separation Avg",
      rowlabels == "snap_dis_from_los" ~ "Distance from LOS at Snap",
      rowlabels == "mean_var_dir_x" ~ "X Direction Variance",
      rowlabels == "RG_pct" ~ "Percentage of play RG was closest player",
      rowlabels == "pre_play_dis" ~ "Distance Travelled (Pre-Play)",
      rowlabels == "qb_separation" ~ "QB Separation Avg",
      rowlabels == "closest_dt_separation" ~ "DT Separation Avg",
      rowlabels == "mean_var_dir_y" ~ "Y Direction Variance",
      rowlabels == "closest_pass_block_player_dir_diff" ~ "Closest O-Line Direction Difference Avg",
      rowlabels == "mean_var_s_theta" ~ "Angular Speed Variance",
      rowlabels == "separation_from_snap_position" ~ "Euclidean Separation from position at Snap Avg",
      rowlabels == "LG_pct" ~ "Percentage of play LG was closest player",
      rowlabels == "qb_dir_diff" ~ "QB Direction Difference Avg",
      rowlabels == "LT_pct" ~ "Percentage of play LT was closest player",
      rowlabels == "mean_rowlabels_dir" ~ "Direction rowlabelsaince",
      rowlabels == "RT_pct" ~ "Percentage of play RT was closest player",
      rowlabels == "C_pct" ~ "Percentage of play C was closest player",
      rowlabels == "sum_dis" ~ "Total Distance Travelled",
      rowlabels == "froward_sep_from_snap" ~ "Forward Movement from Snap",
      rowlabels == "mean_var_dir" ~ "Direction Variance",
      rowlabels == "right_side_sep_from_snap" ~ "Right Side Movement from Snap",
      rowlabels == "left_side_sep_from_snap" ~ "Left Side Movement from Snap",
      rowlabels == "forward_sep_from_snap" ~ "Forward Side Movement from Snap",
      TRUE ~ as.character(rowlabels)
    )
  ) %>%
  arrange(desc(Overall)) %>%
  head(15L) %>%
  ggplot() +
  aes(x = reorder(rowlabels,Overall),y = Overall,fill = Overall) +
  geom_bar(stat = "identity",position = "dodge") +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8, face = 'bold', hjust = 0.5),
    axis.title = element_text(size = 8, face = 'bold', hjust = 0.5)
  ) + 
  guides(fill = "none") +
  scale_fill_gradient2(low = "#ffd7b5",mid = "#ffb38a",high = "#ff9248",
                       midpoint = 10) +
  labs(
    x = "Variables",
    y = "Relative Influence",
    title = "Top 15 Variables"
  )

###### Pressure RTOE




#panel <- (p1 | p2 | p3) / (p4 | p5 | p6)

panel <- (g1 | g2 | g3 | g4) /  (g5 | g6 | g7 | g8)

return(panel)
#panel

}

bdb_partial_dependence()



ggsave("partial_dependence.png")


#https://stats.stackexchange.com/questions/50560/how-to-calculate-partial-dependence-when-i-have-4-predictors


#pdp_rf <- DALEX::model_profile(explainer = explainer_rf, variables = c("closest_pass_block_separation",
#                                                                       "closest_pass_block_tackle_separation",
#                                                                       "mean_var_s",
#                                                                       "mean_var_y"),
#                               type = "partial",
#                               groups = "Blocked")


#plot(pdp_rf) +
#  ggtitle("Partial Dependence Profile ","")