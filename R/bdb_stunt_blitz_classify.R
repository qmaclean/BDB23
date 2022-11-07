
df<-read_parquet("final_df.parquet")

df %>%
  group_by(pff_role) %>%
  summarise( n = n())

#### SOLO STUNTS
##### DOWNLINE ONLY
### individual stunts = 1
## outside to inside solo stunt, inside to outside solo stunt


### TWO PERSON TWISTS - indiviudal stunts = 2
### inside stunt = if A1, A2 are unassigned, inside twist - if A1 & B3, A2, B4
### outside twist = B3 & C5, B4 & C6
### overload inside stunt, overload outside stunt = individual stunt = 2 & multiGap stunt = 1


### MULTI HOLE STUNTS (3+)
#### Multi Stunt, Solo Gap
#### indivdual stunts > 3, multiGap = 0

### Multi Stunt, Multi Gap
## individual stunts > 3, multiGap > 1

downline_pivot<-df %>%
  distinct() %>%
  dplyr::filter(pff_positionLinedUp %in% c('DLT','LEO','ROLB','DRT','RE','DLT','REO',
                                           'LE','NLT','NT','NRT','LOLB'),
                pff_role == "Pass Rush") %>%
  dplyr::select(gameId,playId,target_var,assigned) %>%
  dplyr::mutate(unassigned = ifelse(assigned == 1,0,1)
                ) %>%
  dplyr::select(-assigned) %>%
  group_by(gameId,playId,target_var) %>%
  dplyr::summarize(unassigned = sum(unassigned)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c("gameId","playId"),names_from = "target_var",values_from = c("unassigned")) %>%
  mutate(dl_individual_stunts = rowSums(.[3:8],na.rm = T),
         dl_multiGap_stunt = rowSums(.[3:8] > 1,na.rm = T)) %>%
mutate(stunt_type = case_when(
  dl_individual_stunts == 0 ~ "NO STUNT",
  ##### SOLO STUNTS
  dl_individual_stunts == 1 & (A1 == 1) ~ "LEFT A-GAP INSIDE SOLO STUNT",
  dl_individual_stunts == 1 & (A2 == 1) ~ "RIGHT A-GAP INSIDE SOLO STUNT",
  dl_individual_stunts == 1 & (B3 == 1 | C5 == 1) ~ "LEFT OUTSIDE SOLO STUNT",
  dl_individual_stunts == 1 & (B4 == 1 | C6 == 1) ~ "RIGHT OUTSIDE SOLO STUNT",
  #### DOUBLE STUNTS
  dl_individual_stunts == 2 & A1 == 1 & A2 == 1 ~ "A-GAP SWITCH",
  dl_individual_stunts == 2 & (A1 == 1 & B3 == 1) & dl_multiGap_stunt == 0 ~ "LEFT SIDE A-B TWIST",
  dl_individual_stunts == 2 & (A2 == 1 & B4 == 1) & dl_multiGap_stunt == 0 ~ "RIGHT SIDE A-B TWIST",
  dl_individual_stunts == 2 & (B3 == 1 & C5 == 1) & dl_multiGap_stunt == 0 ~ "LEFT SIDE B-C TWIST",
  dl_individual_stunts == 2 & (B4 == 1 & C6 == 1) & dl_multiGap_stunt == 0 ~ "RIGHT SIDE B-C TWIST",
  dl_individual_stunts == 2 & (A1 == 1 & C5 == 1) & dl_multiGap_stunt == 0 ~ "LEFT SIDE A-C TWIST",
  dl_individual_stunts == 2 & (A2 == 1 & C6 == 1) & dl_multiGap_stunt == 0 ~ "RIGHT SIDE A-C TWIST",
  dl_individual_stunts == 2 & (B3 == 1 & B4 == 1) & dl_multiGap_stunt == 0 ~ "B-GAP SWITCH",
  dl_individual_stunts == 2 & (C5 == 1 & C6 == 1) & dl_multiGap_stunt == 0 ~ "C-GAP SWITCH",
  dl_individual_stunts == 2 & (B3 == 1 & C6 == 1) & dl_multiGap_stunt == 0 ~ "LEFT B to OPP C GAP SWITCH",
  dl_individual_stunts == 2 & (B4 == 1 & C5 == 1) & dl_multiGap_stunt == 0 ~ "RIGHT B to OPP C GAP SWITCH",
  dl_individual_stunts == 2 & (A1 == 1 & (B4 == 1 | C6 == 1)) & dl_multiGap_stunt == 0 ~ "LEFT A to OPP OUTSIDE SWITCH",
  dl_individual_stunts == 2 & (A2 == 1 & (B3 == 1 | C5 == 1)) & dl_multiGap_stunt == 0 ~ "RIGHT A to OPP OUTSIDE SWITCH",
  dl_individual_stunts == 2 & ((A1 == 2 | A2 == 2)) & dl_multiGap_stunt == 1 ~ "OVERLOAD INSIDE STUNT",
  dl_individual_stunts == 2 & ((B3 == 2 | C5 == 2)) & dl_multiGap_stunt == 1 ~ "LEFT OVERLOAD OUTSIDE STUNT",
  dl_individual_stunts == 2 & ((B4 == 2 | C6 == 2)) & dl_multiGap_stunt == 1 ~ "RIGHT OVERLOAD OUTSIDE STUNT",
  dl_individual_stunts >= 3 & dl_multiGap_stunt == 0 ~ "MULTI-GAP STUNT",
  dl_individual_stunts >= 3 & dl_multiGap_stunt == 1 ~ "MULTI-GAP STUNT - SINGLE OVERLOAD",
  dl_individual_stunts >= 3 & dl_multiGap_stunt >= 2 ~ "MULTI-GAP STUNT - MULTI OVERLOAD",
  TRUE ~ as.character(NA)
),
stunt_category = case_when(
  stunt_type %in% c("NO STUNT") ~ "NO STUNT",
  stunt_type %in% c("LEFT A-GAP INSIDE SOLO STUNT","RIGHT A-GAP INSIDE SOLO STUNT",
                    "LEFT OUTSIDE SOLO STUNT","RIGHT OUTSIDE SOLO STUNT") ~ "SOLO STUNT",
  stunt_type %in% c("LEFT SIDE A-B TWIST","RIGHT SIDE A-B TWIST","LEFT SIDE B-C TWIST",
                    "RIGHT SIDE B-C TWIST","LEFT SIDE A-C TWIST","RIGHT SIDE A-C TWIST") ~ "DOUBLE TWIST",
  stunt_type %in% c("A-GAP SWITCH","B-GAP SWITCH","C-GAP SWITCH","LEFT B to OPP C GAP SWITCH",
                    "RIGHT B to OPP C GAP SWITCH","LEFT A to OPP OUTSIDE SWITCH","RIGHT A to OPP OUTSIDE SWITCH") ~ "DOUBLE SWITCH",
  stunt_type %in% c("OVERLOAD INSIDE STUNT","LEFT OVERLOAD OUTSIDE STUNT","RIGHT OVERLOAD OUTSIDE STUNT") ~ "SINGLE OVERLOAD",
  stunt_type %in% c("MULTI-GAP STUNT") ~ "MULTI-GAP STUNT",
  stunt_type %in% c("MULTI-GAP STUNT - SINGLE OVERLOAD") ~ "MULTI-GAP STUNT - SINGLE OVERLOAD",
  stunt_type %in% c("MULTI-GAP STUNT - MULTI OVERLOAD") ~ "MULTI-GAP STUNT - MULTI OVERLOAD",
  TRUE ~ as.character(NA)
))

fix<-downline_pivot %>%
  dplyr::filter(!complete.cases(stunt_type))


#### ADD A TAG FOR NON DOWN LINE TO BE BLITZ
non_downline_pivot<-df %>%
  distinct() %>%
  dplyr::filter(!pff_positionLinedUp %in% c('DLT','LEO','ROLB','DRT','RE','DLT','REO',
                                           'LE','NLT','NT','NRT','LOLB'),
                pff_role == "Pass Rush") %>%
  dplyr::select(gameId,playId,target_var,assigned,nflId,pff_positionLinedUp) %>%
  dplyr::mutate(assigned = ifelse(is.na(assigned),0,assigned),
                unassigned = ifelse(assigned == 1,0,1)
  ) %>%
  dplyr::select(-assigned) %>%
  group_by(gameId,playId,target_var) %>%
  dplyr::summarize(unassigned = sum(unassigned)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c("gameId","playId"),names_from = "target_var",values_from = c("unassigned")) %>%
  mutate(individual_blitz = rowSums(.[3:8],na.rm = T),
         multiGap_blitz = rowSums(.[3:8] > 1,na.rm = T)) %>%
  mutate(blitz_type = case_when(
    ##### SOLO BLITZ
    individual_blitz == 1 & (A1 == 1) ~ "SOLO LEFT A-GAP BLITZ",
    individual_blitz == 1 & (A2 == 1) ~ "SOLO RIGHT A-GAP BLITZ",
    individual_blitz == 1 & (B3 == 1 | C5 == 1) ~ "SOLO LEFT OUTSIDE BLITZ",
    individual_blitz == 1 & (B4 == 1 | C6 == 1) ~ "SOLO RIGHT OUTSIDE BLITZ",
    #### DOUBLE BLITZ
    individual_blitz == 2 & (A1 == 1 & A2 == 1) & multiGap_blitz == 0 ~ "DOUBLE A BLITZ",
    individual_blitz == 2 & (A1 == 1 & B3 == 1) & multiGap_blitz == 0 ~ "DOUBLE LEFT A-B BLITZ",
    individual_blitz == 2 & (A2 == 1 & B4 == 1) & multiGap_blitz == 0 ~ "DOUBLE RIGHT A-B BLITZ",
    individual_blitz == 2 & (B3 == 1 & C5 == 1) & multiGap_blitz == 0 ~ "DOUBLE LEFT B-C BLITZ",
    individual_blitz == 2 & (B4 == 1 & C6 == 1) & multiGap_blitz == 0 ~ "DOUBLE RIGHT B-C BLITZ",
    individual_blitz == 2 & (A1 == 1 & C5 == 1) & multiGap_blitz == 0 ~ "DOUBLE LEFT A-C BLITZ",
    individual_blitz == 2 & (A2 == 1 & C6 == 1) & multiGap_blitz == 0 ~ "DOUBLE RIGHT A-C BLITZ",
    individual_blitz == 2 & (B3 == 1 & B4 == 1) & multiGap_blitz == 0 ~ "DOUBLE B BLITZ",
    individual_blitz == 2 & (C5 == 1 & C6 == 1) & multiGap_blitz == 0 ~ "DOUBLE C BLITZ",
    individual_blitz == 2 & (B3 == 1 & C6 == 1) & multiGap_blitz == 0 ~ "LEFT B to OPP C BLITZ",
    individual_blitz == 2 & (B4 == 1 & C5 == 1) & multiGap_blitz == 0 ~ "RIGHT B to OPP C BLITZ",
    individual_blitz == 2 & (A1 == 1 & (B4 == 1 | C6 == 1)) & multiGap_blitz == 0 ~ "LEFT A to OPP OUTSIDE BLITZ",
    individual_blitz == 2 & (A2 == 1 & (B3 == 1 | C5 == 1)) & multiGap_blitz == 0 ~ "RIGHT A to OPP OUTSIDE BLITZ",
    individual_blitz == 2 & (A1 == 2 | A2 == 2) & multiGap_blitz == 1 ~ "DOUBLE INSIDE OVERLOAD BLITZ",
    individual_blitz == 2 & (B3 == 2 | C5 == 2) & multiGap_blitz == 1 ~ "DOUBLE LEFT OUTSIDE OVERLOAD BLITZ",
    individual_blitz == 2 & (B4 == 2 | C6 == 2) & multiGap_blitz == 1 ~ "DOUBLE RIGHT OUTSIDE OVERLOAD BLITZ",
    ### MULTI
    individual_blitz >= 3 & multiGap_blitz == 0 ~ "MULTI-GAP BLITZ",
    individual_blitz >= 3 & multiGap_blitz == 1 ~ "MULTI-GAP BLITZ - SINGLE OVERLOAD",
    individual_blitz >= 3 & multiGap_blitz >= 2 ~ "MULTI-GAP BLITZ - MULTI OVERLOAD",
    TRUE ~ as.character(NA)
  ),
  blitz_category = case_when(
    blitz_type %in% c("SOLO LEFT A-GAP BLITZ","SOLO RIGHT A-GAP BLITZ",
                      "SOLO LEFT OUTSIDE BLITZ","SOLO RIGHT OUTSIDE BLITZ") ~ "SOLO BLITZ",
    blitz_type %in% c("DOUBLE LEFT A-B BLITZ","DOUBLE RIGHT A-B BLITZ","DOUBLE LEFT B-C BLITZ",
                      "DOUBLE RIGHT B-C BLITZ","DOUBLE LEFT A-C BLITZ",
                      "DOUBLE RIGHT A-C BLITZ") ~ "DOUBLE SIDE BLITZ",
    blitz_type %in% c("DOUBLE A BLITZ","DOUBLE B BLITZ","DOUBLE C BLITZ","LEFT B to OPP C BLITZ",
                      "RIGHT B to OPP C BLITZ","LEFT A to OPP OUTSIDE BLITZ",
                      "RIGHT A to OPP OUTSIDE BLITZ") ~ "DOUBLE BOTH BLITZ",
    blitz_type %in% c("DOUBLE INSIDE OVERLOAD BLITZ","DOUBLE LEFT OUTSIDE OVERLOAD BLITZ",
                      "DOUBLE RIGHT OUTSIDE OVERLOAD BLITZ") ~ "SINGLE OVERLOAD",
    blitz_type %in% c("MULTI-GAP BLITZ") ~ "MULTI-GAP BLITZ",
    blitz_type %in% c("MULTI-GAP BLITZ - SINGLE OVERLOAD") ~ "MULTI-GAP BLITZ - SINGLE OVERLOAD",
    blitz_type %in% c("MULTI-GAP BLITZ - MULTI OVERLOAD") ~ "MULTI-GAP BLITZ - MULTI OVERLOAD",
    TRUE ~ as.character(NA)
  )
  )


fix<-non_downline_pivot %>%
  dplyr::filter(!complete.cases(blitz_type))


### Stunt Blitz (Join to flag a non downline man coming too)




downline_pivot %>%
  group_by(stunt_type) %>%
  summarize(n = n()) %>%
  mutate( freq = n / sum(n)) %>%
  arrange(desc(n))

downline_pivot %>%
  group_by(stunt_category) %>%
  summarize(n = n()) %>%
  mutate( freq = n / sum(n)) %>%
  arrange(desc(n))

downline_pivot<-downline_pivot %>%
  dplyr::left_join(non_downline_pivot,by=c("gameId" = "gameId",
                                           "playId" = "playId")) %>%
  dplyr::select(-A1.x,-A2.x,-B3.x,-B4.x,-C5.x,-C6.x,
                -A1.y,-A2.y,-B3.y,-B4.y,-C5.y,-C6.y) %>%
  dplyr::mutate(stunt_blitz = ifelse(stunt_category != "NO STUNT" & individual_blitz >= 1,1,0),
                individual_blitz = ifelse(is.na(individual_blitz),0,individual_blitz),
                multiGap_blitz = ifelse(is.na(multiGap_blitz),0,multiGap_blitz),
                blitz_type = ifelse(is.na(blitz_type),"NO BLITZ",blitz_type),
                blitz_category = ifelse(is.na(blitz_category),"NO BLITZ",blitz_category),
                stunt_blitz = ifelse(is.na(stunt_blitz),0,stunt_blitz))

down


write_parquet(downline_pivot,"line_stunts.parquet")
write_parquet(non_line_pivot,"blitz_info.parquet")


####### combine #####
stunts<-read_parquet("line_stunts.parquet")
downline_df<-read_parquet("final_df.parquet")



downline_pivot<-downline_df %>%
  distinct() %>%
  dplyr::filter(pff_positionLinedUp %in% c('DLT','LEO','ROLB','DRT','RE','DLT','REO',
                                           'LE','NLT','NT','NRT','LOLB'),
                pff_role == "Pass Rush") %>%
  dplyr::select(gameId,playId,target_var,assigned,nflId,pff_positionLinedUp) %>%
  dplyr::mutate(assigned = ifelse(is.na(assigned),0,assigned),
                unassigned = ifelse(assigned == 1,0,1)
  ) %>%
  dplyr::select(-assigned) %>%
  dplyr::filter(unassigned >= 1)
  


##### wk1 
wk1<-read.csv("data/src/week1.csv")


wk1<-wk1 %>%
  inner_join(stunts,by=c("gameId" = "gameId",
                         "playId" = "playId")) %>%
  dplyr::filter(stunt_type != "NO STUNT") %>%
  dplyr::inner_join(downline_pivot,by=c("gameId" = "gameId",
                                        "playId" = "playId",
                                        "nflId"  = "nflId")) %>%
  dplyr::filter(unassigned == 1)


play_min<-wk1 %>%
  group_by(gameId,playId,nflId) %>%
  summarize(min_x = min(x))

wk1<-wk1 %>%
  left_join(play_min,by=c("gameId","playId","nflId"))

#### wk2
wk2<-read.csv("data/src/week2.csv")


wk2<-wk2 %>%
  inner_join(stunts,by=c("gameId" = "gameId",
                         "playId" = "playId")) %>%
  dplyr::filter(stunt_type != "NO STUNT") %>%
  dplyr::inner_join(downline_pivot,by=c("gameId" = "gameId",
                                        "playId" = "playId",
                                        "nflId"  = "nflId")) %>%
  dplyr::filter(unassigned == 1)


play_min<-wk2 %>%
  group_by(gameId,playId,nflId) %>%
  summarize(min_x = min(x))

wk2<-wk2 %>%
  left_join(play_min,by=c("gameId","playId","nflId"))

##### week 3
wk3<-read.csv("data/src/week3.csv")


wk3<-wk3 %>%
  inner_join(stunts,by=c("gameId" = "gameId",
                         "playId" = "playId")) %>%
  dplyr::filter(stunt_type != "NO STUNT") %>%
  dplyr::inner_join(downline_pivot,by=c("gameId" = "gameId",
                                        "playId" = "playId",
                                        "nflId"  = "nflId")) %>%
  dplyr::filter(unassigned == 1)


play_min<-wk3 %>%
  group_by(gameId,playId,nflId) %>%
  summarize(min_x = min(x))

wk3<-wk3 %>%
  left_join(play_min,by=c("gameId","playId","nflId"))

##### week 4
wk4<-read.csv("data/src/week4.csv")


wk4<-wk4 %>%
  inner_join(stunts,by=c("gameId" = "gameId",
                         "playId" = "playId")) %>%
  dplyr::filter(stunt_type != "NO STUNT") %>%
  dplyr::inner_join(downline_pivot,by=c("gameId" = "gameId",
                                        "playId" = "playId",
                                        "nflId"  = "nflId")) %>%
  dplyr::filter(unassigned == 1)


play_min<-wk4 %>%
  group_by(gameId,playId,nflId) %>%
  summarize(min_x = min(x))

wk4<-wk4 %>%
  left_join(play_min,by=c("gameId","playId","nflId"))

##### week 5
wk5<-read.csv("data/src/week5.csv")



wk5<-wk5 %>%
  inner_join(stunts,by=c("gameId" = "gameId",
                         "playId" = "playId")) %>%
  dplyr::filter(stunt_type != "NO STUNT") %>%
  dplyr::inner_join(downline_pivot,by=c("gameId" = "gameId",
                                        "playId" = "playId",
                                        "nflId"  = "nflId")) %>%
  dplyr::filter(unassigned == 1)


play_min<-wk5 %>%
  group_by(gameId,playId,nflId) %>%
  summarize(min_x = min(x))

wk5<-wk5 %>%
  left_join(play_min,by=c("gameId","playId","nflId"))

### week 6
wk6<-read.csv("data/src/week6.csv")


wk6<-wk6 %>%
  inner_join(stunts,by=c("gameId" = "gameId",
                         "playId" = "playId")) %>%
  dplyr::filter(stunt_type != "NO STUNT") %>%
  dplyr::inner_join(downline_pivot,by=c("gameId" = "gameId",
                                        "playId" = "playId",
                                        "nflId"  = "nflId")) %>%
  dplyr::filter(unassigned == 1)


play_min<-wk6 %>%
  group_by(gameId,playId,nflId) %>%
  summarize(min_x = min(x))

wk6<-wk6 %>%
  left_join(play_min,by=c("gameId","playId","nflId"))


### week 7
wk7<-read.csv("data/src/week7.csv")


wk7<-wk7 %>%
  inner_join(stunts,by=c("gameId" = "gameId",
                         "playId" = "playId")) %>%
  dplyr::filter(stunt_type != "NO STUNT") %>%
  dplyr::inner_join(downline_pivot,by=c("gameId" = "gameId",
                                        "playId" = "playId",
                                        "nflId"  = "nflId")) %>%
  dplyr::filter(unassigned == 1)


play_min<-wk7 %>%
  group_by(gameId,playId,nflId) %>%
  summarize(min_x = min(x))

wk7<-wk7 %>%
  left_join(play_min,by=c("gameId","playId","nflId"))

### week 8
wk8<-read.csv("data/src/week8.csv")


wk8<-wk8 %>%
  inner_join(stunts,by=c("gameId" = "gameId",
                         "playId" = "playId")) %>%
  dplyr::filter(stunt_type != "NO STUNT") %>%
  dplyr::inner_join(downline_pivot,by=c("gameId" = "gameId",
                                        "playId" = "playId",
                                        "nflId"  = "nflId")) %>%
  dplyr::filter(unassigned == 1)


play_min<-wk8 %>%
  group_by(gameId,playId,nflId) %>%
  summarize(min_x = min(x))

wk8<-wk8 %>%
  left_join(play_min,by=c("gameId","playId","nflId"))


downline_tracking<-rbind(wk1,wk2,wk3,wk4,wk5,wk6,wk7,wk8)

write_parquet(downline_tracking,"downline_tracking.parquet")


#### pivot longer 

downline_tracking<-read_parquet("downline_tracking.parquet")





            