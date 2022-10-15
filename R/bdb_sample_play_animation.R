


#wk1<-read.csv("data/week1.csv")




#example_play<-wk5 %>%
#  dplyr::filter(gameId == "2021100700",
#                playId == "301") %>%
#  left_join(plays,by=c("gameId","playId")) %>%
#  left_join(pff,by=c("gameId","playId","nflId")) %>%
#  dplyr::filter(pff_role %in% c("Pass Block","Pass Rush") |
#                  team == "football")


example_play<-tracking %>%
  dplyr::filter(gameId == "2021100700",
                playId == "301") %>%
    dplyr::filter(pff_role %in% c("Pass Block","Pass Rush") |
                    team == "football")

#### insert player ids


bdb_play_visualization<-function(example_play){
  
  require(tidyverse)
  require(gganimate)
  
  #field_color = #006400
  
  plot_field <- function(field_color="#FFFFFF", line_color = "#212529", number_color = "#ffffff") {
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
  

  df_colors = data.frame(home_1 = 'white',
                         home_2 = 'dark blue',
                         away_1 = 'black',
                         away_2 = 'orange')
  
  
  
  
  play_frames<-plot_field() + 
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
    )  +  # away team velocities
    geom_segment(
      data = example_play %>% dplyr::filter(team == possessionTeam),
      mapping = aes(x = x, y = y, xend = x, yend = y),
      color = df_colors$away_1, size = 5, arrow = arrow(length = unit(0.2, "cm"),ends="last")
    )  + # home team velocities
    geom_segment(
      data = example_play %>% dplyr::filter(team == defensiveTeam),
      mapping = aes(x = x, y = y, xend = x, yend = y),
      colour = df_colors$home_2, size = 5, arrow = arrow(length = unit(0.2, "cm"))
    ) +  # away team locs and jersey numbers
    geom_point(
      data = example_play %>% dplyr::filter(team == possessionTeam),
      mapping = aes(x = x, y = y),
      fill = "#f8f9fa", colour = df_colors$away_2,
      shape = 21, alpha = 0.7, size = 8, stroke = 1.5
    )  +
    geom_text(
      data = example_play %>% dplyr::filter(team == possessionTeam),
      mapping = aes(x = x, y = y, label = jerseyNumber),
      colour = df_colors$away_1, size = 3
    ) +
    # home team locs and jersey numbers
    geom_point(
      data = example_play %>% dplyr::filter(team == defensiveTeam),
      mapping = aes(x = x, y = y),
      fill = df_colors$home_1, colour = df_colors$home_2,
      shape = 21, alpha = 0.7, size = 8, stroke = 1.5
    )  + geom_text(
      data = example_play %>% dplyr::filter(team == defensiveTeam),
      mapping = aes(x = x, y = y, label = jerseyNumber),
      colour = df_colors$home_2, size = 3 
    ) + # ball
    geom_point(
      data = example_play %>% dplyr::filter(team == "football"),
      mapping = aes(x = x, y = y),
      fill = "#935e38", colour = "#d9d9d9",
      shape = 21, alpha = 0.7, size = 6, stroke = 1
    ) +
    transition_time(frameId) +
    ease_aes('linear') +
    NULL
  
  
  play_length <- length(unique(example_play$frameId))
  
  
  
  play_anim <- animate(
    play_frames,
    fps = 10, 
    nframes = play_length,
    width = 800,
    height = 400,
    end_pause = 0
  )
  
  
  return(play_anim)
  
  
}


bdb_play_visualization(example_play = example_play)



