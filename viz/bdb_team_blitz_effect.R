


bdb_team_blitz_effect<-function(){

library(tidyverse)
library(ggimage)
library(RCurl)
library(ggrepel)
library(gt)
library(nflfastR)
library(nflreadr)
library(arrow)
library(gtExtras)

#remotes::install_github("jthomasmock/gtExtras")
#install.packages("gt")


stunts<-read_parquet("line_stunts.parquet")
downline_df<-read_parquet("final_df.parquet")
xblock<-read_parquet("expected_block_results.parquet")
xpressure<-read_parquet("expected_pressure_results.parquet")
players<-read_csv("data/src/players.csv") %>%
  dplyr::select(nflId,displayName,officialPosition)
### adding logos
#url.logo <- getURL("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
#df.logos <- read.csv(text = url.logo)

teams <- "https://github.com/nflverse/nflfastR-data/raw/master/teams_colors_logos.rds"
team_df <- readRDS(url(teams))
team_df %>% dplyr::select(
  team_abbr,team_wordmark
)

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



team_scheme_rate_metrics<-downline_pivot %>%
  mutate(xBlocked = ifelse(xBlock == "Yes",1,0),
         Blocked = ifelse(Blocked == "Yes",1,0),
         xPressure = ifelse(xPressure_predictions == "Yes",1,0),
         Pressure = ifelse(pressure == "A",1,0),
         stunt_indicator = ifelse(stunt_category != "NO STUNT","STUNT","NO STUNT"),
         blitz_indicator = ifelse(blitz_category != "NO BLITZ","BLITZ","NO BLITZ"),
         id = paste0(gameId,playId)) %>%
  dplyr::group_by(defensiveTeam,stunt_indicator,blitz_indicator) %>%
  dplyr::summarise( n = n_distinct(id),
                    xBlock_rate = sum(xBlocked) / n(),
                    Block_rate = sum(Blocked) / n(),
                    xPressure_rate = sum(xPressure) / n(),
                    Pressure_rate = sum(Pressure) / n(),
                    def_epa = round(mean(def_epa,na.rm = T),3),
                    def_pass_epa = round(mean(def_pass_epa,na.rm = T),3)) %>%
  dplyr::mutate(Block_RTUE = round(xBlock_rate - Block_rate,3) * 100,
                Pressure_RTOE = round(Pressure_rate - xPressure_rate,3),
                stunt_indicator = as.factor(stunt_indicator)) %>%
  #dplyr::left_join(df.logos,by=c("defensiveTeam" = "team_code")) %>%
  dplyr::select(-xBlock_rate,-xPressure_rate,-def_epa) %>%
  pivot_wider(id_cols = c("defensiveTeam"),
              names_from = c("stunt_indicator","blitz_indicator"),
              values_from = c("n",
                              "Block_RTUE","Pressure_RTOE","def_pass_epa")) %>%
  ungroup() %>%
  dplyr::left_join(team_df,by=c("defensiveTeam" = "team_abbr")) %>%
  dplyr::mutate(total_snaps = `n_NO STUNT_BLITZ` + `n_NO STUNT_NO BLITZ` + 
                                  `n_STUNT_BLITZ` + `n_STUNT_NO BLITZ`) %>%
  dplyr::select(team_wordmark,`n_STUNT_BLITZ`,`n_STUNT_NO BLITZ`,total_snaps,`Block_RTUE_STUNT_BLITZ`,
                `Block_RTUE_STUNT_NO BLITZ`,`Pressure_RTOE_STUNT_BLITZ`,
                `Pressure_RTOE_STUNT_NO BLITZ`,
                `def_pass_epa_STUNT_BLITZ`,`def_pass_epa_STUNT_NO BLITZ` , 
                )


tbl<-team_scheme_rate_metrics %>%
  gt() %>%
  text_transform(
    locations = cells_body(c(team_wordmark)),
    fn = function(x){
      web_image(
        url = x,
        height = px(30)
      )
    }
  ) %>%
  cols_label(
    team_wordmark = "",
    ### Snaps
    `n_STUNT_BLITZ` = " w/ BLITZ",
    `n_STUNT_NO BLITZ` = "Reg. Stunt",
    total_snaps = "Total",
    ### Block RTUE
    `Block_RTUE_STUNT_BLITZ` = "w/ BLITZ",
    `Block_RTUE_STUNT_NO BLITZ` = "Reg. Stunt",
    ### PRESSURE RTOE
    `Pressure_RTOE_STUNT_BLITZ` = "w/ BLITZ",
    `Pressure_RTOE_STUNT_NO BLITZ` = "Reg. Stunt",
    ### DEF PASS EPA
    `def_pass_epa_STUNT_BLITZ` = "w / BLITZ",
    `def_pass_epa_STUNT_NO BLITZ` = "Reg. Stunt",   
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
    source_notes.font.size = 8,
    table.font.size = 10,
    heading.title.font.size = 16,
    column_labels.font.size = 8,
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
    columns = 2:4
  ) %>%
  tab_spanner(
    label = "BRUE",
    columns = 5:6
  ) %>%
  tab_spanner(
    label = "PROE",
    columns = 7:8
  ) %>%
  tab_spanner(
    label = "Defensive Pass EPA",
    columns = 9:10
  ) %>%
  tab_header(
    title = md("**Stunt Performance with or without additional pressure from Blitz**"),
    subtitle = md("Viz: @QuinnsWisdom")
  ) %>%
  gt_color_box(columns = c(
    Block_RTUE_STUNT_BLITZ,`Block_RTUE_STUNT_NO BLITZ`),
               domain=range(
                          team_scheme_rate_metrics$Block_RTUE_STUNT_BLITZ,
                            team_scheme_rate_metrics$`Block_RTUE_STUNT_NO BLITZ`), 
    palette = c("#FFFFFF","#91f086","#48bf53","#11823b","#004d25","#02231c"),
               accuracy = 0.01,suffix="%") %>%
  gt_color_box(columns = c(
                     `Pressure_RTOE_STUNT_BLITZ`,
                      `Pressure_RTOE_STUNT_NO BLITZ`),
    domain=range(
                 team_scheme_rate_metrics$`Pressure_RTOE_STUNT_BLITZ`,
                 team_scheme_rate_metrics$`Pressure_RTOE_STUNT_NO BLITZ`), 
    palette = c("#FFFFFF","#ffd7b5","#ffb38a","#ff9248","#ff6700"),
    accuracy = 0.01,suffix="%") %>%
  gt_color_box(columns = c(
                        `def_pass_epa_STUNT_BLITZ`,
                        `def_pass_epa_STUNT_NO BLITZ`),
    domain=range(
                 team_scheme_rate_metrics$`def_pass_epa_STUNT_BLITZ`,
                 team_scheme_rate_metrics$`def_pass_epa_STUNT_NO BLITZ`), 
    palette = c("#FFFFFF","#bbeeff","#99ccff","#5588ff","#3366ff"),
    #palette = "rcartocolor::Sunset"
    accuracy = 0.01)

  




return (tbl)


}

#bdb_team_blitz_effect()
