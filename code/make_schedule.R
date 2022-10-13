#rm(list = ls())
require(tidyverse)
require(gtools)
require(parallel)

setwd("C:/Users/Morgan/_data_projects/rpsc_scheduler")

## 
param <- list()
param$teams <- 1:20
param$weeks <- 1:8
param$courts <- 1:10
param$court_weeks <- rep(list(param$courts),length(param$weeks))

## SIMULATION PROCESSING PARAMETERS
param$sim$settings$schedules_n <- 200
param$sim$settings$processing_options <- c(
  "parallel" #windows machine with parallel package installed, faster
  ,"base" #single processor, easier admin, slower
)
# select processing method:
param$sim$settings$processing_method <- "parallel"

sim <- list()
#randomize scenario
sim$df <- data.frame(
  team = param$teams
  ,isNewTeam = FALSE
  ,wins = 0
)

##assign new teams
param$sim$settings$newTeams_n <- rbinom(1,length(param$weeks),.3)
if(param$sim$settings$newTeams_n > 0){
  sim$df[1:param$sim$settings$newTeams_n,"isNewTeam"] <- TRUE
}
  
  sim$df$isNewTeam <- FALSE

  sim$df$wins <- c(8,8,7,6,5,5,5,5,5,4,3,3,3,3,3,2,2,2,1,0)
  
hist(sim$df$wins)
  
sim$df$zscore <- (sim$df$wins - mean(sim$df$wins))/sd(sim$df$wins)

#function to calculate std deviations between source and sample
sim$func$get_zscore_dist <- function(zscore,zscore_vector){
  1-pnorm(abs(zscore - zscore_vector))
}

#function to calculate percentages
sim$func$get_zscore_dist_prob <- function(zscore,zscore_vector){
  zdists <- sim$func$get_zscore_dist(zscore,zscore_vector)
  zdists / sum(zdists)
}

#tmp <- sim$func$get_zscore_dist_prob(zscore, zscore_vector)

sim$func$previous_opponents <- function(sched,team){
  c(sched[sched$h == team,]$a,
    sched[sched$a == team,]$h) %>% unique
}

sim$func$already_on_week_sched <- function(sched,week){
  c(sched[sched$wk == week,]$h,
    sched[sched$wk == week,]$a)
}

#team <- h
sim$func$possible_opponents <- function(teams, team, sched, week){
  teams[!teams %in% 
          unique(c(team,
                   sim$func$previous_opponents(sched,team),
                   sim$func$already_on_week_sched(sched,week)))] %>%
    sort
}

sim$func$create_schedule <- function(param,sim){
  require(tidyverse)
  
  teams <- sim$df$team
  sched <- data.frame(
    wk = as.integer()
    ,h = as.integer()
    ,a = as.integer()
    ,dif = as.double()
    ,crt = as.integer()
  )
  blank_sched <- sched
  
  keep_going <- 1
  x <- 0
  
  while(keep_going == 1){
    x <- x + 1
    
    sched <- blank_sched
    reset <- 0
    
    #to-do: check for bye 
    
    for (w in param$weeks){
      #w <- 1
      if ( reset == 1 ) {break}
      
      pick_order <- sample(teams, length(teams))
      
      for(h in pick_order){
        #h <- pick_order[15]
        if (h %in% sim$func$already_on_week_sched(sched,w)) {next}
        pre_ops <- sim$func$previous_opponents(sched,h)
        
        #to-do: remove other new teams as options for first week
        
        poss_ops <- sim$func$possible_opponents(teams,h,sched,w)
        
        if ( length(poss_ops) == 0 | reset == 1) {
          reset <- 1
          break
        }
        
        if(length(poss_ops) == 1){
          a <- poss_ops[1]
          } else {
          a <- sample(poss_ops,1,
                 prob = sim$func$get_zscore_dist_prob(
                   sim$df[h,]$zscore
                   ,sim$df[poss_ops,]$zscore))
          }
        
        dif = sim$df[h,]$zscore - sim$df[a,]$zscore
          
        sched <- rbind(sched,
                       data.frame(
                         "wk" = w
                         ,"h" = h
                         ,"a" = a
                         ,"dif" = dif
                         ,"crt" = NA))
        
      }
    }
    if(reset == 0) {keep_going <- 0}
  }
  return(sched)  
}

##test: create 1 schedule
#sched <- sim$fun$create_schedule(param,sim)

##eval: teams do not play same team more than once
# rbind(sched[sched$h>sched$a,2:3]
#       ,sched[sched$h<sched$a,c(3,2)]) %>% 
#   count(h,a) %>% pull(n) %>% max() == 1


#create x number of schedules
if(param$sim$settings$processing_method == "parallel"){
  
  require(parallel)
    sim$processing$cores_n <- (detectCores() - 1)
    sim$processing$cluster <- makeCluster(sim$processing$cores_n)
    clusterExport(sim$processing$cluster,c("param","sim"))
    system.time(
      scheds <- parLapply(sim$processing$cluster
                              ,1:param$sim$settings$schedules_n,function(x) 
                                sim$func$create_schedule(param,sim))
      )
    gc()
    stopCluster(sim$processing$cluster)
} 

if(param$sim$settings$processing_method == "base"){
  system.time(
    scheds <- lapply(1:param$sim$settings$n_schedules, function(x)
      sim$fun$create_schedule(param,sim))
  )
}

# find schedule with lowest:
  # 1. variance of mean standard deviation distance 
  # 2. lowest overall mean standard deviation distance

diag <- list()
#evaluates schedules for how much discrepancy in team strength in matches
diag$variances <- sapply(scheds,function(x)
  sapply(param$teams,function(y)
    sum(x[x$h==y,]$dif) + (-1 * sum(x[x$a==y,]$dif))) %>% 
    var 
  ) 

#evaluates how similar teams are in strength for matches
diag$means <- lapply(1:length(scheds), function(x)
  mean(abs(scheds[[x]]$dif))) %>% unlist

diag$score <- sqrt(diag$variances^2 + diag$means^2)

plot(diag$variances,diag$means)

#select best schedule
sched <- scheds[min(diag$score)==diag$score][[1]]

#review 
for(i in param$teams){
  #i <- 1
  sim$df[i,] %>% left_join(
    data.frame(
      team = i
        # INTERPRETATION: 
          # Variances and Means closer to 0 represent optimal schedules
      ,variance = var(c(-1 * sched[sched$h==i,]$dif,sched[sched$a==i,]$dif))
        # INTERPRETATION:
        # variance = 0, schedule has few games with big differences in team strength
        # variance >= 1, schedule has games with big differences in team strength
      ,mean = mean(c(-1 * sched[sched$h==i,]$dif,sched[sched$a==i,]$dif))
        # INTERPRETATION:
        # mean = 0, schedule averages out to equal strength to that of team
        # mean > 0, schedule averages to be challenging
        # mean < 0, schedule averages to be easier than strength of team
    ), by = c("team")) %>% print
  
  bind_rows(sched[sched$h==i,c("wk","a","dif")] %>% rename(opponent = a)
            ,sched[sched$a==i,c("wk","h","dif")] %>% rename(opponent = h)
  ) %>% arrange(wk) %>% 
    left_join(sim$df, by = c("opponent" = "team")) %>% 
    print
}
rm("i")

