

bdb_stunt_types_table<-function(){

library(ggimage)
library(RCurl)
library(ggrepel)
library(gt)
library(nflfastR)
library(nflreadr)
library(arrow)

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



stunt_type_metrics<-downline_pivot %>%
  mutate(xBlocked = ifelse(xBlock == "Yes",1,0),
         Blocked = ifelse(Blocked == "Yes",1,0),
         xPressure = ifelse(xPressure_predictions == "Yes",1,0),
         Pressure = ifelse(pressure == "A",1,0),
         stunt_indicator = ifelse(stunt_category != "NO STUNT","STUNT","NO STUNT"),
         id = paste0(gameId,playId),
         players_involved = case_when(
           stunt_category == "NO STUNT" ~ "0",
           stunt_category == "SOLO STUNT" ~ "1",
           stunt_category %in% c("SINGLE OVERLOAD","DOUBLE TWIST","MULTI-GAP STUNT",
                                 "DOUBLE SWITCH") ~ "2",
           stunt_category %in% c("MULTI-GAP STUNT - SINGLE OVERLOAD",
                                 "MULTI-GAP STUNT") ~ "3+",
           stunt_category %in% c("MULTI-GAP STUNT - MULTI OVERLOAD") ~ "4+",
           TRUE ~ as.character(NA)
         )
  ) %>%
  dplyr::group_by(stunt_type,stunt_category,players_involved) %>%
  dplyr::summarise( n = n_distinct(id),
                    xBlock_rate = sum(xBlocked) / n(),
                    Block_rate = sum(Blocked) / n(),
                    xPressure_rate = sum(xPressure) / n(),
                    Pressure_rate = sum(Pressure) / n(),
                    off_epa = mean(off_epa,na.rm = T),
                    off_pass_epa = mean(off_pass_epa,na.rm = T),
                    def_epa = mean(def_epa,na.rm = T),
                    def_pass_epa = mean(def_pass_epa,na.rm = T)) %>%
  dplyr::mutate(Block_RTUE = xBlock_rate - Block_rate,
                Pressure_RTOE = Pressure_rate - xPressure_rate) %>%
  ungroup()


stunt_type_metrics %>%
  dplyr::select(stunt_type,stunt_category,players_involved,n,Block_rate,Pressure_rate,Block_RTUE,Pressure_RTOE,def_pass_epa) %>%
  dplyr::arrange(desc(Block_RTUE)) %>%
  gt() %>%
  cols_label(
    stunt_type = "Stunt Type",
    stunt_category = "Stunt Category",
    players_involved = "Players Involved",
    n = "Plays",
    Block_RTUE = "Block RTUE",
    Pressure_RTOE = "Pressure RTOE",
    def_pass_epa = "Def Pass EPA/play",
    Block_rate = "Block Rate",
    Pressure_rate = "Pressure  Rate"
  ) %>%
  data_color(
    columns = c(`Block_RTUE`),
    colors = scales::col_numeric(
      palette = c("#FFFFFF","#91f086","#48bf53","#11823b","#004d25","#02231c"),
      domain = NULL
    )
  ) %>%
  data_color(
    columns = c(`Pressure_RTOE`),
    colors = scales::col_numeric(
      palette = c("#FFFFFF","#ffd7b5","#ffb38a","#ff9248","#ff6700"),
      domain = NULL
    ) 
  ) %>%
  data_color(
    columns = c(`def_pass_epa`),
    colors = scales::col_numeric(
      palette = c("#FFFFFF","#bbeeff","#99ccff","#5588ff","#3366ff"),
      domain = NULL
    ) 
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = c(stunt_type,stunt_category)
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
    source_notes.font.size = 10,
    table.font.size = 10,
    heading.title.font.size = 8,
    column_labels.font.size = 8,
    heading.align = "center"
  ) %>%
  opt_table_font(
    font = list(
      google_font("Mono"),
      default_fonts()
    )
  ) %>%
  tab_spanner(
    label = "Adv. Metrics",
    columns = 7:9
  ) %>%
  tab_spanner(
    label = "Actual Rates",
    columns = 5:6
  ) %>%
  tab_header(
    title = md("**Stunt Performance**"),
    subtitle = md("Viz: @QuinnsWisdom")
  ) %>%
  fmt_number(
    sep_mark = ",",
    decimals = 3,
    columns = 9
  ) %>%
  fmt_number(
    sep_mark = ",",
    decimals = 0,
    columns = 4
  ) %>%
  fmt_percent(
    decimals = 1,
    columns = 5:6
  ) %>%
  fmt_percent(
    decimals = 1,
    columns = 7:8
  ) %>%
  tab_footnote(
    footnote = "Block Rate Under Expected (Block RTUE) measure players' ability to go unblocked relative to play expectation",
    locations = cells_column_labels(
      columns = 6
    )
  ) %>%
  tab_footnote(
    footnote = "Pressure Rate Over Expected (Pressure RTOE) measure players' ability to apply pressure (qb hit, sack, hurry) relative to play expectation",
    locations = cells_column_labels(
      columns = 7
    )
  ) %>%
  tab_footnote(
    footnote = "Defensive Pass Expected Points Added per Play; Positive refers to a good defensive play",
    locations = cells_column_labels(
      columns = 8
    )
  ) %>%
  tab_source_note(
    source_note = "Data: NFL NGS | 2021 Wk1 - 8"
  ) %>%
  tab_footnote(
    footnote = "Only considering passing plays",
    locations = cells_column_labels(
      columns = 4
    )
  ) 




}






