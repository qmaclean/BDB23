### visualize stunts off tracking data
library(gganimate)
library(cowplot)
library(tidyverse)


df<-read_parquet("downline_tracking.parquet")






xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3




mean_route<-df %>%
  dplyr::select(gameId,playId,nflId,frameId,x,y,target_var,stunt_type,min_x) %>%
  dplyr::mutate(x = x,
                y = y) %>%
  dplyr::filter(y <= 30,
                y >= 20,
                x >= 40,
                x <= 80) %>%
  dplyr::group_by(frameId,target_var,stunt_type) %>%
  dplyr::summarise(x = median(x,na.rm = T),
                   y = median(y,na.rm = T)) %>%
  dplyr::rename(mean_x = x,
                mean_y = y) %>%
  ungroup() 

### distribution
mean_route %>%
  ggplot(aes(x=mean_y,color=stunt_type)) +
  geom_density()






df<-df %>%
  dplyr::left_join(mean_route,by = c( "frameId" = "frameId",
                                     "target_var" = "target_var",
                                     "stunt_type" = "stunt_type"))



df %>%
  filter(stunt_type != "NO STUNT") %>%
  group_by(stunt_category) %>%
  summarize(n = n()) %>%
  mutate( freq = n / sum(n)) %>%
  arrange(desc(n)) %>%
  head(12L)


#### score each play 


## Specific boundaries for a given play
ymin <- max(round(min(df$x - df$min_x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(df$x - df$min_x, na.rm = TRUE) + 10, -1), 120)
df_hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df_hash <- df_hash %>% filter(!(floor(y %% 5) == 0))
df_hash <- df_hash %>% filter(y < ymax, y > ymin)

df %>%
  dplyr::filter(frameId <= 30,
                stunt_category == "SOLO STUNT") %>%
  mutate(Id = paste0(gameId,playId,nflId),
         Id2 = paste0(gameId,playId,nflId)) %>%
ggplot() +
  scale_size_manual(values = c(6, 4, 6), guide = "none") + 
  scale_shape_manual(values = c(21, 16, 21), guide = "none") +
  scale_fill_manual(values = c("#e31837", "#654321", "#002244"), guide = "none") + 
  scale_colour_manual(values = c("black", "#654321", "#c60c30"), guide = "none") + 
  annotate("text", x = df_hash$x[df_hash$x < 55/2], 
           y = df_hash$y[df_hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df_hash$x[df_hash$x > 55/2], 
           y = df_hash$y[df_hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  #annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
  #         label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
  #         angle = 270, size = 4) + 
  #annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
  #         label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
  #         angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "white") +
  
  geom_line(aes(x = (xmax - y) + 2.5, y = x - min_x + 10,group = Id),alpha = 0.4,color = "dark grey") +
  #geom_line(aes(x =  mean_x, y = mean_y,group=Id2),color = "red",alpha = 0.5) +
  #geom_line(data = wk1 %>% 
  #            dplyr::group_by(stunt_type,frameId) %>%
  #            dplyr::summarize(x = mean(x,na.rm = T),
  #                             y = mean(y,na.rm = T),
  #                             min_x = mean(min_x,na.rm = T)),
  #          aes(x = (xmax-y) + 2.5,y = x - min_x + 10),alpha = 0.5,color = "red") +
  facet_wrap(~stunt_type,ncol = 6) + 
  ylim(ymin,ymax) +
  #coord_fixed() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#006400"),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    strip.text = element_text(size = 5)
  ) +
  labs(
    x = "",
    y = "",
    title = "Solo Stunts"
  )


mean_route %>%
  group_by(stunt_type) %>%
  summarize(max = max(frameId))

mean_route %>%
  dplyr::filter(frameId <= 30) %>%
  ggplot() +
  geom_segment(
    mapping = aes(y = mean_x, x = mean_y, yend = mean_x, xend = mean_y,color=target_var),
   size = 5, arrow = arrow(length = unit(0.2, "cm"),ends="last")
  )  + 
  facet_wrap(~stunt_type) +
  transition_time(frameId) +
  shadow_trail(distance = 0.5) +
  #ylim(ymin,ymax) +
  #coord_fixed() +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#006400"),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    strip.text = element_text(size = 5)
  ) +
  labs(
    x = "",
    y = "",
    title = "NFL Stunt Types"
  )
  
