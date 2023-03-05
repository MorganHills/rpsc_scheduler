#rec %>% str
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
  filter(division_id == "BV4s8x") %>% 
  select(
    time
    ,location_identifier
    ,home_competitor_id
    ,away_competitor_id
    ,home_competitor_name
    ,away_competitor_name
    ,home_competitor_elo_cache_previous
    ,away_competitor_elo_cache_previous
  ) %>%
  mutate(wk = dense_rank(time)) 

rec$dim$teams <- rbind(
  data.frame(
    recTeamId = rec$dim$sched$home_competitor_id
    ,recTeamName = rec$dim$sched$home_competitor_name
  )
  ,data.frame(
    recTeamId = rec$dim$sched$away_competitor_id
    ,recTeamName = rec$dim$sched$away_competitor_name
  )
) %>% unique %>% 
  arrange(recTeamId) %>%
  mutate(id = order(recTeamId))
      
# 
# rec$dim$teams <- data.frame(
#   recTeamId = c(
#     rec$dim$sched$home_competitor_id,
#     rec$dim$sched$away_competitor_id) %>%
#     unique()
#   ) %>% 
#   arrange(recTeamId) %>%
#   mutate(id = order(recTeamId))

rec$dim$courts <- rec$dim$sched %>%
  mutate(court = as.numeric(location_identifier)) %>%
  pull(court) %>% unique  %>% sort.int 

###
rec$dim$teams$newTeam <- NA
rec$dim$teams$prevWins <- NA   
rec$dim$teams$elo <- NA

for (t in rec$dim$teams$recTeamId){
  #t <- rec$dim$teams$recTeamId[1]
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
rm(list = c("hist","t"))
rec$dim$teams

hist(rec$dim$teams$elo)
hist(rec$dim$teams$prevWins)
sd(rec$dim$teams$elo,na.rm = TRUE)
sd(rec$dim$teams$prevWins, na.rm = TRUE)

####
## COPY SCHEDULE
####

#rm("param")
dat <- rec$dim

dat$teams[is.na(dat$teams$elo),]$elo <- 1000
dat$teams[is.na(dat$teams$prevWins),]$prevWins <- 4

dat$teams$zscore_elo <- (dat$teams$elo - mean(dat$team$elo, na.rm = TRUE)) / 
  sd(dat$teams$elo, na.rm = TRUE)

dat$teams$zscore_wins <- (dat$teams$prevWins - mean(dat$teams$prevWins, 
                                                    na.rm = TRUE)) /
  sd(dat$teams$prevWins, na.rm = TRUE)

#
sim <- list()


##
sim$results <- list()
sim$results <- list( list(sched = rec$dim$sched))
sim$results[[1]]$sched

for (i in seq_along(1:1000)){
  #i <- 2
  if (i > 1){
    sim$results[[i]] <- list(new_assignments = sample(rec$dim$teams$recTeamId))
    # rec$dim$teams$recTeamId
    # sim$results[[i]]$new_assignments
    
    sim$results[[i]]$sched <- rec$dim$sched %>% 
      select(-home_competitor_elo_cache_previous,
             -away_competitor_elo_cache_previous)
    
    #unmap competitors, assume not integer ids
    for (t in 1:length(sim$results[[i]]$new_assignments)){
      # t <- 1
      orig_team <- rec$dim$teams$recTeamId[t]
      if(nrow(sim$results[[i]]$sched[sim$results[[i]]$sched$home_competitor_id == orig_team,])>0){
        sim$results[[i]]$sched[sim$results[[i]]$sched$home_competitor_id == orig_team,]$home_competitor_id <- t}
      if(nrow(sim$results[[i]]$sched[sim$results[[i]]$sched$away_competitor_id == orig_team,])>0){
        sim$results[[i]]$sched[sim$results[[i]]$sched$away_competitor_id == orig_team,]$away_competitor_id <- t   
      }
    }
    #map to new assignments, assume not integer ids
    for (t in 1:length(sim$results[[i]]$new_assignments)){
      new_team <- sim$results[[i]]$new_assignments[t]
      if(nrow(sim$results[[i]]$sched[sim$results[[i]]$sched$home_competitor_id == t,])>0){
        sim$results[[i]]$sched[sim$results[[i]]$sched$home_competitor_id == t,]$home_competitor_id <- new_team}
      if(nrow(sim$results[[i]]$sched[sim$results[[i]]$sched$away_competitor_id == t,])>0){
        sim$results[[i]]$sched[sim$results[[i]]$sched$away_competitor_id == t,]$away_competitor_id <- new_team}     
    }
    
  }
  
  sim$results[[i]]$sched <- 
    sim$results[[i]]$sched %>% 
    left_join(
      dat$teams %>% 
        select(recTeamId, 
               home_id = id,
               home_newTeam = newTeam,
               home_prevWins = prevWins,
               home_elo = elo,
               home_zscore_elo = zscore_elo,
               home_zscore_wins = zscore_wins
               ),
      by = c("home_competitor_id" = "recTeamId"), 
      suffix = c(".x","home_")
    ) %>% 
    left_join(
      dat$teams %>% 
        select(recTeamId, 
               away_id = id,
               away_newTeam = newTeam,
               away_prevWins = prevWins,
               away_elo = elo,
               away_zscore_elo = zscore_elo,
               away_zscore_wins = zscore_wins
        ),
      by = c("away_competitor_id" = "recTeamId"), 
      suffix = c(".x","away_")
    ) %>%
    mutate(
      zscore_diff_elo = home_zscore_elo - away_zscore_elo,
      zscore_diff_wins = home_zscore_wins - away_zscore_wins
    )

  sim$results[[i]]$metric <- list(
      elo = list(
        var = sapply(dat$teams$recTeamId,function(y)
                sum(sim$results[[i]]$sched[sim$results[[i]]$sched$home_competitor_id==y,]$zscore_diff_elo) + 
                  (-1 * sum(sim$results[[i]]$sched[sim$results[[i]]$sched$away_competitor_id==y,]$zscore_diff_elo))) %>% 
              var 
        ,means = mean(abs(sim$results[[i]]$sched$zscore_diff_elo))
        ,score = sqrt(
          (sapply(dat$teams$recTeamId,function(y)
            sum(sim$results[[i]]$sched[sim$results[[i]]$sched$home_competitor_id==y,]$zscore_diff_elo) + 
              (-1 * sum(sim$results[[i]]$sched[sim$results[[i]]$sched$away_competitor_id==y,]$zscore_diff_elo))) %>% 
            var ) ^2
          + (mean(abs(sim$results[[i]]$sched$zscore_diff_elo))) ^2)
        )
      ,prevWins = list(
        var = sapply(dat$teams$recTeamId,function(y)
          sum(sim$results[[i]]$sched[sim$results[[i]]$sched$home_competitor_id==y,]$zscore_diffwWins) + 
            (-1 * sum(sim$results[[i]]$sched[sim$results[[i]]$sched$away_competitor_id==y,]$zscore_diff_wins))) %>% 
          var 
        ,means = mean(abs(sim$results[[i]]$sched$zscore_diff_wins))
        ,score = sqrt(
          (sapply(dat$teams$recTeamId,function(y)
            sum(sim$results[[i]]$sched[sim$results[[i]]$sched$home_competitor_id==y,]$zscore_diff_wins) + 
              (-1 * sum(sim$results[[i]]$sched[sim$results[[i]]$sched$away_competitor_id==y,]$zscore_diff_wins))) %>% 
             var ) ^2
          + (mean(abs(sim$results[[i]]$sched$zscore_diff_wins))) ^2
          )
        )
      )
    
}



my_eval <- data.frame(
  sim = integer()
  ,elo_var = numeric()
  ,elo_means = numeric()
  ,elo_score = numeric()
  ,wins_var = numeric()
  ,wins_means = numeric()
  ,wins_score = numeric()
)

for(i in 1:length(sim$results)){
  tmp <- sim$results[[i]]$metric
  my_eval <- rbind(
    my_eval,
    data.frame(
      sim = i
      ,elo_var = tmp$elo$var
      ,elo_means = tmp$elo$means
      ,elo_score = tmp$elo$score
      ,wins_var = tmp$prevWins$var
      ,wins_means = tmp$prevWins$means
      ,wins_score = tmp$prevWins$score
    )
  )
}

my_eval

plot(my_eval$elo_score,my_eval$wins_score)
text(x=my_eval$elo_score[1],y=my_eval$wins_score[1],labels = "actual",pos = 2)

hist(my_eval$elo_score)
hist(my_eval$wins_score)

b<-my_eval[my_eval$wins_score==min(my_eval$wins_score),][1]


tmp <- sim$results[[b]]$sched %>%
  left_join(rec$dim$teams %>% select(recTeamId, homeName = recTeamName, -id),by = c("home_competitor_id" = "recTeamId" )) %>%
  left_join(rec$dim$teams %>% select(recTeamId, awayName = recTeamName, -id),by = c("away_competitor_id" = "recTeamId" ))

rec$dim$teams

for (t in rec$dim$teams$recTeamId){
  # t <- rec$dim$teams$recTeamId[1]
  rec$dim$teams[rec$dim$teams$recTeamId==t,]$recTeamName %>% print
  dat$teams[dat$teams$recTeamId==t,]%>% print
  rbind(
    tmp[tmp$home_competitor_id == t,] %>%
    select(wk,opponent = awayName,wins = away_prevWins, elo = away_elo)
    ,tmp[tmp$away_competitor_id == t,] %>%
      select(wk,opponent = homeName,wins = home_prevWins, elo = home_elo)
  )%>% print
}
