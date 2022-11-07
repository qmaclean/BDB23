

bdb_partial_dependence<-function(){

library(tidyverse)
library(arrow)
library(patchwork)


df<-read_parquet("final_df.parquet")
eb<-read_parquet("expected_block_results.parquet")
pr<-read_parquet("expected_pressure_results.parquet")
#rf<-readRDS("model_objects/active/expected_block_model_rf.rds")




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
         snap_separation_grp = cut(snap_separation,
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
  group_by(snap_separation_grp) %>%
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
  dplyr::left_join(pd_var6,by=c("snap_separation_grp" = "snap_separation_grp")) %>%
  mutate(A_Pressure_prob_ssg_uppr = ifelse(A_Pressure_prob + ssg_sd > 1,1,A_Pressure_prob + ssg_sd),
         A_Pressure_prob_ssg_lwr = ifelse(A_Pressure_prob - ssg_sd < 0,0,A_Pressure_prob - ssg_sd)) 



g1<- df %>%
  dplyr::filter(closest_pass_block_separation <= 10,
                closest_pass_block_separation > 0) %>%
  ggplot() + 
  geom_smooth(aes(x = closest_pass_block_separation,y = A_prob),method = "loess",se = F,color = "#004d25") +
  geom_smooth(aes(x = closest_pass_block_separation,y = A_prob_cpbs_uppr),method = "loess",se= F,linetype = "dotted",color = "#004d25") +
  geom_smooth(aes(x = closest_pass_block_separation,y = A_prob_cpbs_lwr),method = "loess",se = F,linetype = "dotted",color= "#004d25") +
  theme(
    axis.text.x = element_text(size = 6, face = 'bold', hjust = 0.5),
    axis.title.x = element_text(size = 6, face = 'bold', hjust = 0.5)
  ) + 
  theme_minimal() +
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
  theme(
    axis.text.x = element_text(size = 6, face = 'bold', hjust = 0.5),
    axis.title.x = element_text(size = 6, face = 'bold', hjust = 0.5)
    ) + 
  theme_minimal() +
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
  theme(
    axis.text.x = element_text(size = 6, face = 'bold', hjust = 0.5),
    axis.title.x = element_text(size = 6, face = 'bold', hjust = 0.5)
    ) + 
  theme_minimal() +
  labs(x = "Closest Off. Guard Separation (Yds)",
       y = "Block Probability") +
  ylim(0,1)

#df %>%
#  ggplot() + 
  #geom_smooth(aes(x = mean_var_s,y = A_prob),method = "loess",se = F,color = "#48bf53") +
#  geom_smooth(aes(x = mean_var_y,y = A_prob),method = "loess",se = F,color = "#48bf53") 

#"#ffd7b5","#ffb38a","#ff9248","#ff6700"

g4<-df %>%
  dplyr::filter(qb_separation <= 10,
                qb_separation > 0) %>%
  ggplot() + 
  geom_smooth(aes(x = qb_separation,y = A_Pressure_prob),method = "loess",se = F,color = "#ff9248") +
  geom_smooth(aes(x = qb_separation,y = A_Pressure_prob_qbs_uppr),method = "loess",se= F,linetype = "dotted",color = "#ff9248") +
  geom_smooth(aes(x = qb_separation,y = A_Pressure_prob_qbs_lwr),method = "loess",se = F,linetype = "dotted",color = "#ff9248") +
  theme(
    axis.text.x = element_text(size = 6, face = 'bold', hjust = 0.5),
    axis.title.x = element_text(size = 6, face = 'bold', hjust = 0.5)
  ) + 
  theme_minimal() +
  labs(x = "QB Separation (Yds)",
       y = "Pressure Probability",
       title = "Pressure Probability Model") +
  ylim(0,1)

g5<-df %>%
  dplyr::filter(sum_dis <= 150,
                sum_dis > 0) %>%
  ggplot() + 
  geom_smooth(aes(x = sum_dis,y = A_Pressure_prob),method = "loess",se = F,color = "#ffb38a") +
  geom_smooth(aes(x = sum_dis,y = A_Pressure_prob_sd_uppr),method = "loess",se= F,linetype = "dotted",color = "#ffb38a") +
  geom_smooth(aes(x = sum_dis,y = A_Pressure_prob_sd_lwr),method = "loess",se = F,linetype = "dotted",color = "#ffb38a") +
  theme(
    axis.text.x = element_text(size = 6, face = 'bold', hjust = 0.5),
    axis.title.x = element_text(size = 6, face = 'bold', hjust = 0.5)
  ) + 
  theme_minimal() +
  labs(x = "Distance Travelled (Yds)",
       y = "Pressure Probability") +
  ylim(0,1)


g6<-df %>%
  dplyr::filter(snap_separation <= 10,
                snap_separation > 0) %>%
  ggplot() + 
  geom_smooth(aes(x = snap_separation,y = A_Pressure_prob),method = "loess",se = F,color = "#ffd7b5") +
  geom_smooth(aes(x = snap_separation,y = A_Pressure_prob_ssg_uppr),method = "loess",se= F,linetype = "dotted",color = "#ffd7b5") +
  geom_smooth(aes(x = snap_separation,y = A_Pressure_prob_ssg_lwr),method = "loess",se = F,linetype = "dotted",color = "#ffd7b5") +
  theme(
    axis.text.x = element_text(size = 6, face = 'bold', hjust = 0.5),
    axis.title.x = element_text(size = 6, face = 'bold', hjust = 0.5)
  ) + 
  theme_minimal() +
  labs(x = "Snap Separation (Yds)",
       y = "Pressure Probability") +
  ylim(0,1)

 




###### Pressure RTOE




#panel <- (p1 | p2 | p3) / (p4 | p5 | p6)

panel <- (g1 | g2 | g3) /  (g4 | g5 | g6)

return(panel)

}


#https://stats.stackexchange.com/questions/50560/how-to-calculate-partial-dependence-when-i-have-4-predictors


#pdp_rf <- DALEX::model_profile(explainer = explainer_rf, variables = c("closest_pass_block_separation",
#                                                                       "closest_pass_block_tackle_separation",
#                                                                       "mean_var_s",
#                                                                       "mean_var_y"),
#                               type = "partial",
#                               groups = "Blocked")


#plot(pdp_rf) +
#  ggtitle("Partial Dependence Profile ","")