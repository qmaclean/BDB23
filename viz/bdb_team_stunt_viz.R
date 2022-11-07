

bdb_team_stunt_viz<-function(){

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


team_scheme_rate_metrics<-downline_pivot %>%
  mutate(xBlocked = ifelse(xBlock == "Yes",1,0),
         Blocked = ifelse(Blocked == "Yes",1,0),
         xPressure = ifelse(xPressure_predictions == "A",1,0),
         Pressure = ifelse(pressure == "A",1,0),
         stunt_indicator = ifelse(stunt_category != "NO STUNT","STUNT","NO STUNT")) %>%
  dplyr::group_by(defensiveTeam,stunt_indicator) %>%
  dplyr::summarise( n = n(),
                    xBlock_rate = sum(xBlocked) / n(),
                    Block_rate = sum(Blocked) / n(),
                    xPressure_rate = sum(xPressure) / n(),
                    Pressure_rate = sum(Pressure) / n(),
                    off_epa = mean(off_epa,na.rm = T),
                    off_pass_epa = mean(off_pass_epa,na.rm = T),
                    def_epa = mean(def_epa,na.rm = T),
                    def_pass_epa = mean(def_pass_epa,na.rm = T)) %>%
  dplyr::mutate(Block_RTUE = xBlock_rate - Block_rate,
                Pressure_RTOE = Pressure_rate - xPressure_rate,
                stunt_indicator = as.factor(stunt_indicator)) %>%
  dplyr::left_join(df.logos,by=c("defensiveTeam" = "team_code"))

#### do we use 0 as average?
def_pass_epa_mean<-mean(downline_pivot$def_pass_epa,na.rm = T)
block_rtue_mean<-mean(team_scheme_rate_metrics$Block_RTUE)


transparent <- function(img) {
  magick::image_fx(img, expression = "0.2*a", channel = "alpha")
}

transparent1 <- function(img) {
  magick::image_fx(img, expression = "0.01*a", channel = "alpha")
}

plot<-team_scheme_rate_metrics %>%
  ggplot(aes(x=Block_RTUE,y=def_pass_epa)) +
  geom_image(data = team_scheme_rate_metrics %>% dplyr::filter(stunt_indicator == "NO STUNT",
                                                               defensiveTeam != "WAS"),
             aes(image = url), size = 0.03,image_fun = transparent) + 
  geom_image(data = team_scheme_rate_metrics %>% dplyr::filter(stunt_indicator == "STUNT"),
             aes(image = url), size = 0.05) + 
  geom_image(data = team_scheme_rate_metrics %>% dplyr::filter(stunt_indicator == "NO STUNT",
                                                               defensiveTeam == "WAS"),
             aes(image = url), size = 0.03,image_fun = transparent1) + 
  geom_abline(intercept = -5,slope=-500,color="light grey",alpha=0.3) +
  geom_abline(intercept = 0,slope=-500,color="light grey",alpha=0.3) +
  geom_abline(intercept = 5,slope=-500,color="light grey",alpha=0.3) +
  geom_abline(intercept = 10,slope=-500,color="light grey",alpha=0.3) +
  geom_abline(intercept = 15,slope=-500,color="light grey",alpha=0.3) +
  geom_abline(intercept = 20,slope=-500,color="light grey",alpha=0.3) +
  geom_abline(intercept = 25,slope=-500,color="light grey",alpha=0.3) +
  geom_hline(yintercept = def_pass_epa_mean,color = "orange",alpha = 0.7,linetype = "dashed") +
  geom_vline(xintercept = block_rtue_mean,color = "orange",alpha = 0.7,linetype = "dashed") +
  annotate(geom="text", x=0.04, y=1.13, label="Mean Def Pass EPA",
           color="grey") +
  annotate(geom="text", x=0.0105, y=8.2, label="Mean Block RTUE",angle = 90,
           color="grey") +
  #annotate(geom="text", x=0.01, y=-0.002, label="Stunts: -, No Stunts: +",
  #         color="grey") + 
  #annotate(geom="text", x=-0.02, y=-0.002, label="Stunts: -, No Stunts: -",
  #         color="grey") + 
  theme_minimal() +
  labs(
    x = "Block Rate Under Expected",
    y = "defensive Pass EPA/play",
    title = "Block Rate Under Expected for Stunts vs No Stunts",
    caption =  "Faded Image or Smaller Image: Non Stunt | Fully Opaque Image: Stunt"
  )

#plot

return(plot)


}
