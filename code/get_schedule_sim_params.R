rec %>% str
rec %>% names
rec$data %>% str

rec$dim <- list()

rec$dim$divisions <- rec$data %>% select(
  division_id
  ,division_season_id
  ,division_name
  ,division_day_of_week
  ,division_start_time
  ,division_end_time
  ,time
) %>% 
  mutate(time = as.POSIXct(time)) %>%
  unique %>% as_tibble %>%
  group_by(
    division_season_id
    ,division_id
    ,division_name
    ,division_day_of_week
    ,division_start_time
    ,division_end_time
  ) %>%
  mutate(
    min_date = min(time,na.rm = TRUE)
    ,max_date = max(time,na.rm = TRUE)
  ) %>% 
  select(-time) %>% unique 

rec$dim$divisions %>% head(20)

rec$dim$sched <- rec$data %>% 
  filter(division_id == "BDks3k") %>% 
  select(
    time
    ,location_identifier
    ,home_competitor_id
    ,away_competitor_id
    ,home_competitor_elo_cache_previous
    ,away_competitor_elo_cache_previous 
  ) %>%
  mutate(wk = dense_rank(time)) 

rec$dim$teams <- data.frame(
  recTeamId = c(
    rec$dim$sched$home_competitor_id,
    rec$dim$sched$away_competitor_id) %>%
    unique()
  ) %>% 
  arrange(recTeamId) %>%
  mutate(id = order(recTeamId))

rec$dim$courts <- rec$dim$sched %>%
  mutate(court = as.numeric(location_identifier)) %>%
  pull(court) %>% unique  %>% sort.int 

###
rec$dim$teams$newTeam <- NA
rec$dim$teams$prevWins <- NA   
rec$dim$teams$elo <- NA
for (t in rec$dim$teams$recTeamId){
  #t <- rec$dim$teams$recTeamId[2]
  hist <- rbind(
    rec$data %>%
      filter(time < min(rec$dim$sched$time)
           & home_competitor_id == t) %>%
      select(id, time, score = home_score, division_id, division_season_id)
    ,rec$data %>%
      filter(time < min(rec$dim$sched$time)
             & away_competitor_id == t) %>%
      select(id, time, score = away_score, division_id, division_season_id)
  )
  
  if (nrow(hist) > 0 ){
    rec$dim$teams[rec$dim$teams$recTeamId == t,]$newTeam <- 0
  } else {
    rec$dim$teams[rec$dim$teams$recTeamId == t,]$newTeam <- 1
    next
    }
  
  rec$dim$teams[rec$dim$team$recTeamId == t,]$prevWins <- hist %>%
    group_by(division_id) %>%
    mutate(mintime_season = min(time)) %>%
    arrange(time) %>%
    ungroup() %>%
    mutate(sea_hist = dense_rank(desc(mintime_season)) ) %>%
    filter(sea_hist == 1) %>%
    pull(score) %>% sum

  rec$dim$teams[rec$dim$team$recTeamId == t,]$elo <- 
    rbind(
      rec$dim$sched %>% filter(
        home_competitor_id == t
        & wk == 1) %>%
        select(elo = home_competitor_elo_cache_previous)
      ,rec$dim$sched %>% filter(
        away_competitor_id == t
        & wk == 1) %>%
        select(elo = away_competitor_elo_cache_previous)
    ) %>% pull(elo)
  
}  
rec$dim$teams

hist(rec$dim$teams$elo)
hist(rec$dim$teams$prevWins)
sd(rec$dim$teams$elo,na.rm = TRUE)
sd(rec$dim$teams$prevWins, na.rm = TRUE)
