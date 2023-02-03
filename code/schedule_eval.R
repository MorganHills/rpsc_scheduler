eval <- list()

eval$seasons <- as.list(rec$data$division_id %>% unique)

eval$seasons[[1]] <- list(sched = data.frame()
                          ,teams = data.frame()
                          )
eval$seasons[[1]]$sched <- 
  rec$data[rec$data$division_id=="2pxs63",]


glimpse(eval$season[[1]]$sched)

eval$seasons[[1]]$sched <- 
  eval$seasons[[1]]$sched %>% 
  mutate(date = as.Date(time))

eval$seasons[[1]]$teams <- bind_rows(
  eval$seasons[[1]]$sched[eval$seasons[[1]]$sched$date == min(eval$seasons[[1]]$sched$date),] %>%
  select(teamId = home_competitor_id
         ,teamName = home_competitor_name
         ,teamRating = home_elo_cache_previous)
  ,eval$seasons[[1]]$sched[eval$seasons[[1]]$sched$date == min(eval$seasons[[1]]$sched$date),] %>%
    select(teamId = away_competitor_id
           ,teamName = away_competitor_name
           ,teamRating = away_elo_cache_previous)
) %>% 
  rowwise %>% 
  mutate(isNewTeam = if(teamRating == 1000) {TRUE} else {FALSE}) %>%
  arrange(desc(teamRating)) %>% print() 

eval$seasons[[1]]$teams$id <- seq.int(nrow(eval$seasons[[1]]$teams))

sim$df <- 
  eval$seasons[[1]]$teams %>%
    select(team = id, isNewTeam, wins = teamRating)


hist(eval$seasons[[1]]$teams$teamRating)


