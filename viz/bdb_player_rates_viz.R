

bdb_top_player_rates_viz<-function(){


library(ggimage)
library(RCurl)
library(ggrepel)
library(gt)
library(nflfastR)
library(nflreadr)
library(arrow)
library(gtExtras)
library(cowplot)
library(webshot2)

#remotes::install_github("jthomasmock/gtExtras")
#install.packages("gt")


stunts<-read_parquet("line_stunts.parquet")
downline_df<-read_parquet("final_df.parquet")
xblock<-read_parquet("expected_block_results.parquet")
xpressure<-read_parquet("expected_pressure_results.parquet")
players<-read_csv("data/src/players.csv") %>%
  dplyr::select(nflId,displayName,officialPosition)
### adding logos
url.logo <- getURL("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
df.logos <- read.csv(text = url.logo)

#### Bring in passing epa effects

pbp<-nflreadr::load_pbp(2021) %>%
  dplyr::filter(week <= 8,
                play_type == "pass") %>%
  dplyr::select(old_game_id,play_id,posteam,home_team,away_team,defteam,posteam_type,total_home_epa,total_away_epa,
                total_home_pass_epa,total_away_pass_epa,desc,play_type) %>%
  dplyr::mutate(off_epa = ifelse(posteam_type == "home",total_home_epa,total_away_epa),
                def_epa = ifelse(posteam_type == "away",total_home_epa,total_away_epa),
                off_pass_epa = ifelse(posteam_type == "home",total_home_pass_epa,total_away_pass_epa),
                def_pass_epa = ifelse(posteam_type == "away",total_home_pass_epa,total_away_pass_epa)) %>%
  mutate(old_game_id = as.integer(old_game_id))



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
  dplyr::left_join(pbp,by=c("gameId" = "old_game_id",
                            "playId" = "play_id")) %>%
  distinct()


player_rate_metrics<-downline_pivot %>%
  mutate(xBlocked = ifelse(xBlock == "Yes",1,0),
         Blocked = ifelse(Blocked == "Yes",1,0),
         xPressure = ifelse(xPressure_predictions == "Yes",1,0),
         Pressure = ifelse(pressure == "A",1,0),
         stunt_indicator = ifelse(unassigned == 1,"STUNT","NO STUNT")) %>%
  dplyr::group_by(nflId,defensiveTeam,stunt_indicator,headshot_url) %>%
  dplyr::summarise( n = n(),
                    xBlock_rate = sum(xBlocked) / n(),
                    Block_rate = sum(Blocked) / n(),
                    xPressure_rate = sum(xPressure) / n(),
                    Pressure_rate = sum(Pressure) / n(),
                    def_pass_epa = mean(def_pass_epa,na.rm = T)) %>%
  dplyr::mutate(Block_RTUE = xBlock_rate - Block_rate,
                Pressure_RTOE = Pressure_rate - xPressure_rate) %>%
  ungroup() %>%
  #dplyr::select(-xBlock_rate,-xPressure_rate) %>%
  dplyr::left_join(players,by=c("nflId" = "nflId")) %>%
  tidyr::pivot_wider(id_cols = c("nflId","defensiveTeam","displayName","officialPosition","headshot_url"),
                     names_from = c("stunt_indicator"),
                     values_from = c("n","Block_RTUE","Pressure_RTOE","Block_rate","Pressure_rate",
                                     "xBlock_rate","xPressure_rate","def_pass_epa")) %>%
  dplyr::left_join(df.logos,by=c("defensiveTeam" = "team_code"))  %>%
  mutate_if(is.numeric,~replace_na(.,0))%>%
  dplyr::mutate(total = `n_NO STUNT` + `n_STUNT`) %>%
  dplyr::filter(total >= 50,
                `n_NO STUNT` > 30,
                `n_STUNT` > 30) %>%
  dplyr::mutate(Block_RTUE_Delta = `Block_RTUE_STUNT` - `Block_RTUE_NO STUNT`,
                Pressure_RTOE_Delta  = `Pressure_RTOE_STUNT` - `Pressure_RTOE_NO STUNT`,
                Def_EPA_Delta = `def_pass_epa_STUNT` - `def_pass_epa_NO STUNT`) 


p1<-player_rate_metrics %>%
  dplyr::select(headshot_url,url,displayName,officialPosition,`n_NO STUNT`,
                n_STUNT,`Pressure_rate_NO STUNT`,`Pressure_rate_STUNT`,
                Pressure_RTOE_Delta,Block_RTUE_Delta,Def_EPA_Delta) %>%
  arrange(desc(Pressure_RTOE_Delta)) %>%
  head(15L) %>%
  gt() %>%
  text_transform(
    locations = cells_body(c(url)),
    fn = function(x){
      web_image(
        url = x,
        height = px(30)
      )
    }
  ) %>%
  text_transform(
    locations = cells_body(c(headshot_url)),
    fn = function(x){
      web_image(
        url = x,
        height = px(30)
      )
    }
  ) %>%
  cols_label(
    url = "",
    headshot_url = "",
    displayName = "Name",
    officialPosition = "Pos.",
    `n_NO STUNT` = "No Stunt",
    `n_STUNT` = "Stunt",
    `Pressure_rate_NO STUNT` = "No Stunt",
    `Pressure_rate_STUNT` = "Stunt",
    Block_RTUE_Delta = "BRUE +/-",
    Pressure_RTOE_Delta = "PROE +/-",
    Def_EPA_Delta = "Defensive Pass EPA +/-"
  ) %>%
  data_color(
    columns = c(`Pressure_RTOE_Delta`),
    colors = scales::col_numeric(
      palette = c("#f7f7f7", "#7fbf7b"),
      domain = NULL
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = c(displayName,officialPosition)
    )
  ) %>%
  tab_options(
    column_labels.background.color = "white",
    column_labels.font.weight = "bold",
    table.border.top.width = px(3),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    data_row.padding = px(3),
    source_notes.font.size = 12,
    table.font.size = 12,
    heading.align = "left"
  ) %>%
  opt_table_font(
    font = list(
      google_font("Mono"),
      default_fonts()
    )
  )  %>%
  tab_spanner(
    label = "Snaps",
    columns = 5:6
  ) %>%
  tab_spanner(
    label = "Pressure Rate",
    columns = 7:8
  ) %>%
  tab_spanner(
    label = "Stunt Influence to Exp. Block/Pressure Rate",
    columns = 9:11
  ) %>%
  tab_header(
    title = md("**Players with Biggest Increase to Pressure Rate Over Expected Through Stunts**"),
    subtitle = md("Viz: @QuinnsWisdom | At least 30 stunts & 30 non-stunts")
  ) %>%
  fmt_number(
    sep_mark = ",",
    decimals = 3,
    columns = 11
  ) %>%
  fmt_percent(
    decimals = 2,
    columns = 7:10
  ) %>%
  tab_footnote(
    footnote = "Increase/Decrease in Block Rate Under Expected (BRUE) through a Stunt",
    locations = cells_column_labels(
      columns = 10
    )
  ) %>%
  tab_footnote(
    footnote = "Increase/Decrease in Pressure Rate Over Expected (PROE) through a Stunt",
    locations = cells_column_labels(
      columns = 9
    )
  ) %>%
  tab_footnote(
    footnote = "Difference to Defensive Pass EPA mean w/ Stunts (neg. would team indicate better w/o Stunt)",
    locations = cells_column_labels(
      columns = 11
    )
  ) %>%
  tab_source_note(
    source_note = "Data: NFL NGS | 2021 Wk1 - 8"
  )

return(p1)

}

#p1<-bdb_top_player_rates_viz()
#p1

#p1 %>%
#gtsave("figs/players_with_stunt_boost.png", expand = 10)




bdb_bottom_player_rates_viz<-function(){
  
  library(ggimage)
  library(RCurl)
  library(ggrepel)
  library(gt)
  library(nflfastR)
  library(nflreadr)
  library(arrow)
  library(gtExtras)
  library(cowplot)
  library(webshot2)
  
  #remotes::install_github("jthomasmock/gtExtras")
  #install.packages("gt")
  
  
  stunts<-read_parquet("line_stunts.parquet")
  downline_df<-read_parquet("final_df.parquet")
  xblock<-read_parquet("expected_block_results.parquet")
  xpressure<-read_parquet("expected_pressure_results.parquet")
  players<-read_csv("data/src/players.csv") %>%
    dplyr::select(nflId,displayName,officialPosition)
  ### adding logos
  url.logo <- getURL("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
  df.logos <- read.csv(text = url.logo)
  
  #### Bring in passing epa effects
  
  pbp<-nflreadr::load_pbp(2021) %>%
    dplyr::filter(week <= 8,
                  play_type == "pass") %>%
    dplyr::select(old_game_id,play_id,posteam,home_team,away_team,defteam,posteam_type,total_home_epa,total_away_epa,
                  total_home_pass_epa,total_away_pass_epa,desc,play_type) %>%
    dplyr::mutate(off_epa = ifelse(posteam_type == "home",total_home_epa,total_away_epa),
                  def_epa = ifelse(posteam_type == "away",total_home_epa,total_away_epa),
                  off_pass_epa = ifelse(posteam_type == "home",total_home_pass_epa,total_away_pass_epa),
                  def_pass_epa = ifelse(posteam_type == "away",total_home_pass_epa,total_away_pass_epa)) %>%
    mutate(old_game_id = as.integer(old_game_id))
  
  
  
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
    dplyr::left_join(pbp,by=c("gameId" = "old_game_id",
                              "playId" = "play_id")) %>%
    distinct()
  
  
  player_rate_metrics<-downline_pivot %>%
    mutate(xBlocked = ifelse(xBlock == "Yes",1,0),
           Blocked = ifelse(Blocked == "Yes",1,0),
           xPressure = ifelse(xPressure_predictions == "Yes",1,0),
           Pressure = ifelse(pressure == "A",1,0),
           stunt_indicator = ifelse(unassigned == 1,"STUNT","NO STUNT")) %>%
    dplyr::group_by(nflId,defensiveTeam,stunt_indicator,headshot_url) %>%
    dplyr::summarise( n = n(),
                      xBlock_rate = sum(xBlocked) / n(),
                      Block_rate = sum(Blocked) / n(),
                      xPressure_rate = sum(xPressure) / n(),
                      Pressure_rate = sum(Pressure) / n(),
                      def_pass_epa = mean(def_pass_epa,na.rm = T)) %>%
    dplyr::mutate(Block_RTUE = xBlock_rate - Block_rate,
                  Pressure_RTOE = Pressure_rate - xPressure_rate) %>%
    ungroup() %>%
    #dplyr::select(-xBlock_rate,-xPressure_rate) %>%
    dplyr::left_join(players,by=c("nflId" = "nflId")) %>%
    tidyr::pivot_wider(id_cols = c("nflId","defensiveTeam","displayName","officialPosition","headshot_url"),
                       names_from = c("stunt_indicator"),
                       values_from = c("n","Block_RTUE","Pressure_RTOE","Block_rate","Pressure_rate",
                                       "xBlock_rate","xPressure_rate","def_pass_epa")) %>%
    dplyr::left_join(df.logos,by=c("defensiveTeam" = "team_code"))  %>%
    mutate_if(is.numeric,~replace_na(.,0))%>%
    dplyr::mutate(total = `n_NO STUNT` + `n_STUNT`) %>%
    dplyr::filter(total >= 50,
                  `n_NO STUNT` > 30,
                  `n_STUNT` > 30) %>%
    dplyr::mutate(Block_RTUE_Delta = `Block_RTUE_STUNT` - `Block_RTUE_NO STUNT`,
                  Pressure_RTOE_Delta  = `Pressure_RTOE_STUNT` - `Pressure_RTOE_NO STUNT`,
                  Def_EPA_Delta = `def_pass_epa_STUNT` - `def_pass_epa_NO STUNT`) 
  

###### Players better not on stunts 
p2<-player_rate_metrics %>%
  dplyr::select(headshot_url,url,displayName,officialPosition,`n_NO STUNT`,
                n_STUNT,`Pressure_rate_NO STUNT`,Pressure_rate_STUNT,
                Pressure_RTOE_Delta,Block_RTUE_Delta,Def_EPA_Delta) %>%
  dplyr::filter(`Pressure_rate_NO STUNT`  >= Pressure_rate_STUNT) %>%
  arrange(Pressure_RTOE_Delta) %>%
  head(15L) %>%
  gt() %>%
  text_transform(
    locations = cells_body(c(url)),
    fn = function(x){
      web_image(
        url = x,
        height = px(30)
      )
    }
  ) %>%
  text_transform(
    locations = cells_body(c(headshot_url)),
    fn = function(x){
      web_image(
        url = x,
        height = px(30)
      )
    }
  ) %>%
  cols_label(
    headshot_url = "",
    url = "",
    displayName = "Name",
    officialPosition = "Pos.",
    `n_NO STUNT` = "No Stunt",
    `n_STUNT` = "Stunt",
    `Pressure_rate_NO STUNT` = "No Stunt",
    `Pressure_rate_STUNT` = "Stunt",
    Block_RTUE_Delta = "BRUE +/-",
    Pressure_RTOE_Delta = "PROE +/-",
    Def_EPA_Delta = "Defensive Pass EPA +/-"
  ) %>%
  data_color(
    columns = c(`Pressure_RTOE_Delta`),
    colors = scales::col_numeric(
      palette = c("#af8dc3","#f7f7f7"),
      domain = NULL
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = c(displayName,officialPosition)
    )
  ) %>%
  tab_options(
    column_labels.background.color = "white",
    column_labels.font.weight = "bold",
    table.border.top.width = px(3),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    data_row.padding = px(3),
    source_notes.font.size = 12,
    table.font.size = 12,
    heading.align = "left"
  ) %>%
  opt_table_font(
    font = list(
      google_font("Mono"),
      default_fonts()
    )
  ) %>%
  tab_spanner(
    label = "Snaps",
    columns = 5:6
  ) %>%
  tab_spanner(
    label = "Pressure Rate",
    columns = 7:8
  ) %>%
  tab_spanner(
    label = "Snaps Influence to Exp. Block/Pressure Rate",
    columns = 9:11
  ) %>%
  tab_header(
    title = md("**Players who Reduce their Pressure Rates Over Expected  w/ Stunts**"),
    subtitle = md("Viz: @QuinnsWisdom | At least 30 stunts & no stunts")
  ) %>%
  fmt_percent(
    decimals = 2,
    columns = 7:10
  ) %>%
  fmt_number(
    sep_mark = ",",
    decimals = 3,
    columns = 11
  ) %>%
  tab_footnote(
    footnote = "Increase/Decrease in Block Rate Under Expected (BRUE) through a Stunt",
    locations = cells_column_labels(
      columns = 10
    )
  ) %>%
  tab_footnote(
    footnote = "Increase/Decrease in Pressure Rate Over Expected (PROE) through a Stunt",
    locations = cells_column_labels(
      columns = 9
    )
  ) %>%
  tab_source_note(
    source_note = "Data: NFL NGS | 2021 Wk1 - 8"
  )

return(p2)

}

#p2<-bdb_bottom_player_rates_viz()
#p2

#p2 %>%
#  gtsave("figs/players_with_least_stunt_boost.png", expand = 10)


#p1 %>%
#  gtsave("viz/p1.png")


#p2 %>%
#  gtsave("viz/p2.png")



#p111<-ggdraw() + draw_image("viz/p1.png",scale = 1)
#p112<-ggdraw() + draw_image("viz/p2.png",scale = 1)
#plot_grid(p111,p112)


#}


bdb_top_player_rates_unblock_viz<-function(){
  
  
  library(ggimage)
  library(RCurl)
  library(ggrepel)
  library(gt)
  library(nflfastR)
  library(nflreadr)
  library(arrow)
  library(gtExtras)
  library(cowplot)
  library(webshot2)
  
  #remotes::install_github("jthomasmock/gtExtras")
  #install.packages("gt")
  
  
  stunts<-read_parquet("line_stunts.parquet")
  downline_df<-read_parquet("final_df.parquet")
  xblock<-read_parquet("expected_block_results.parquet")
  xpressure<-read_parquet("expected_pressure_results.parquet")
  players<-read_csv("data/src/players.csv") %>%
    dplyr::select(nflId,displayName,officialPosition)
  ### adding logos
  url.logo <- getURL("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
  df.logos <- read.csv(text = url.logo)
  
  #### Bring in passing epa effects
  
  pbp<-nflreadr::load_pbp(2021) %>%
    dplyr::filter(week <= 8,
                  play_type == "pass") %>%
    dplyr::select(old_game_id,play_id,posteam,home_team,away_team,defteam,posteam_type,total_home_epa,total_away_epa,
                  total_home_pass_epa,total_away_pass_epa,desc,play_type) %>%
    dplyr::mutate(off_epa = ifelse(posteam_type == "home",total_home_epa,total_away_epa),
                  def_epa = ifelse(posteam_type == "away",total_home_epa,total_away_epa),
                  off_pass_epa = ifelse(posteam_type == "home",total_home_pass_epa,total_away_pass_epa),
                  def_pass_epa = ifelse(posteam_type == "away",total_home_pass_epa,total_away_pass_epa)) %>%
    mutate(old_game_id = as.integer(old_game_id))
  
  
  
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
    dplyr::left_join(pbp,by=c("gameId" = "old_game_id",
                              "playId" = "play_id")) %>%
    distinct()
  
  
  player_rate_metrics<-downline_pivot %>%
    mutate(xBlocked = ifelse(xBlock == "Yes",1,0),
           Blocked = ifelse(Blocked == "Yes",1,0),
           xPressure = ifelse(xPressure_predictions == "Yes",1,0),
           Pressure = ifelse(pressure == "A",1,0),
           stunt_indicator = ifelse(unassigned == 1,"STUNT","NO STUNT")) %>%
    dplyr::group_by(nflId,defensiveTeam,stunt_indicator,headshot_url) %>%
    dplyr::summarise( n = n(),
                      xBlock_rate = sum(xBlocked) / n(),
                      Block_rate = sum(Blocked) / n(),
                      xPressure_rate = sum(xPressure) / n(),
                      Pressure_rate = sum(Pressure) / n(),
                      def_pass_epa = mean(def_pass_epa,na.rm = T)) %>%
    dplyr::mutate(Block_RTUE = xBlock_rate - Block_rate,
                  Pressure_RTOE = Pressure_rate - xPressure_rate) %>%
    ungroup() %>%
    #dplyr::select(-xBlock_rate,-xPressure_rate) %>%
    dplyr::left_join(players,by=c("nflId" = "nflId")) %>%
    tidyr::pivot_wider(id_cols = c("nflId","defensiveTeam","displayName","officialPosition","headshot_url"),
                       names_from = c("stunt_indicator"),
                       values_from = c("n","Block_RTUE","Pressure_RTOE","Block_rate","Pressure_rate",
                                       "xBlock_rate","xPressure_rate","def_pass_epa")) %>%
    dplyr::left_join(df.logos,by=c("defensiveTeam" = "team_code"))  %>%
    mutate_if(is.numeric,~replace_na(.,0))%>%
    dplyr::mutate(total = `n_NO STUNT` + `n_STUNT`) %>%
    dplyr::filter(total >= 50,
                  `n_NO STUNT` > 30,
                  `n_STUNT` > 30) %>%
    dplyr::mutate(Block_RTUE_Delta = `Block_RTUE_STUNT` - `Block_RTUE_NO STUNT`,
                  Pressure_RTOE_Delta  = `Pressure_RTOE_STUNT` - `Pressure_RTOE_NO STUNT`,
                  Def_EPA_Delta = `def_pass_epa_STUNT` - `def_pass_epa_NO STUNT`) 
  
  
  p1<-player_rate_metrics %>%
    dplyr::select(headshot_url,url,displayName,officialPosition,`n_NO STUNT`,
                  n_STUNT,`Pressure_rate_NO STUNT`,`Pressure_rate_STUNT`,
                  Pressure_RTOE_Delta,Block_RTUE_Delta,Def_EPA_Delta) %>%
    arrange(desc(Block_RTUE_Delta)) %>%
    head(15L) %>%
    gt() %>%
    text_transform(
      locations = cells_body(c(url)),
      fn = function(x){
        web_image(
          url = x,
          height = px(30)
        )
      }
    ) %>%
    text_transform(
      locations = cells_body(c(headshot_url)),
      fn = function(x){
        web_image(
          url = x,
          height = px(30)
        )
      }
    ) %>%
    cols_label(
      url = "",
      headshot_url = "",
      displayName = "Name",
      officialPosition = "Pos.",
      `n_NO STUNT` = "No Stunt",
      `n_STUNT` = "Stunt",
      `Pressure_rate_NO STUNT` = "No Stunt",
      `Pressure_rate_STUNT` = "Stunt",
      Block_RTUE_Delta = "BRUE +/-",
      Pressure_RTOE_Delta = "PROE +/-",
      Def_EPA_Delta = "Defensive Pass EPA +/-"
    ) %>%
    data_color(
      columns = c(`Pressure_RTOE_Delta`),
      colors = scales::col_numeric(
        palette = c("#f7f7f7", "#7fbf7b"),
        domain = NULL
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        columns = c(displayName,officialPosition)
      )
    ) %>%
    tab_options(
      column_labels.background.color = "white",
      column_labels.font.weight = "bold",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 12,
      heading.align = "left"
    ) %>%
    opt_table_font(
      font = list(
        google_font("Mono"),
        default_fonts()
      )
    )  %>%
    tab_spanner(
      label = "Snaps",
      columns = 5:6
    ) %>%
    tab_spanner(
      label = "Pressure Rate",
      columns = 7:8
    ) %>%
    tab_spanner(
      label = "Stunt Influence to Exp. Block/Pressure Rate",
      columns = 9:11
    ) %>%
    tab_header(
      title = md("**Players with Biggest Increase to Pressure Rate Over Expected Through Stunts**"),
      subtitle = md("Viz: @QuinnsWisdom | At least 30 stunts & 30 non-stunts")
    ) %>%
    fmt_number(
      sep_mark = ",",
      decimals = 3,
      columns = 11
    ) %>%
    fmt_percent(
      decimals = 2,
      columns = 7:10
    ) %>%
    tab_footnote(
      footnote = "Increase/Decrease in Block Rate Under Expected (BRUE) through a Stunt",
      locations = cells_column_labels(
        columns = 10
      )
    ) %>%
    tab_footnote(
      footnote = "Pressure Rate Over Expected +/- (PROE) measure players' ability to apply pressure (qb hit, sack, hurry) relative to play expectation",
      locations = cells_column_labels(
        columns = 9
      )
    ) %>%
    tab_footnote(
      footnote = "Difference to Defensive Pass EPA mean w/ Stunts (neg. would team indicate better w/o Stunt)",
      locations = cells_column_labels(
        columns = 11
      )
    ) %>%
    tab_source_note(
      source_note = "Data: NFL NGS | 2021 Wk1 - 8"
    )
  
  return(p1)
  
}



