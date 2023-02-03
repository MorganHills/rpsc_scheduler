#rm(list = ls())
require(tidyverse)
require(gtools)

setwd("C:/Users/Morgan/_data_projects/rpsc_scheduler")

param <- list()
param$teams <- 1:20
param$weeks <- 1:8
param$courts <- 1:10
param$court_weeks <- rep(list(param$courts),length(param$weeks))

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
sim$df$zscore <- (sim$df$wins - mean(sim$df$wins))/sd(sim$df$wins)

sched <- data.frame(
  wk = as.integer()
  ,h = as.integer()
  ,a = as.integer()
  ,dif = as.double()
  ,crt = as.integer()
)
blank_sched <- sched


sim$df



















#sched <- unlist(lapply(param$weeks,rep,10))
for(x in 1:1000){
  sched <- blank_sched 
  for(w in param$weeks){
    gms <- matrix(sample(param$teams,20),ncol =2, nrow = 10)
    sched <- rbind(sched,data.frame(
        wk = rep(w,10)
        ,h = gms[,1]
        ,a = gms[,2]
        ,crt = sample(param$court_weeks[[w]],10)
      )
    )
  }
  
  if(rbind(sched[sched$h>sched$a,2:3]
           ,sched[sched$h<sched$a,c(3,2)]) %>% 
     count(h,a) %>% pull(n) %>% max() == 1
  ) {break} else {
    sched <- blank_sched 
  }
}
tt <- c()
for (i in 1:2000){
  sched$crt <- sapply(param$weeks,function(x)
    sample(param$court_weeks[[x]],10)) %>% as.vector()
  
  dup <- data.frame(
    rbindlist(list(sched[,c(3:4)],sched[,c(2,4)])
            ,use.names = FALSE)) %>%
  count(a,crt) %>% filter(n > 1) %>% nrow 
  if(dup == 0) {break}
else {
  tt <- c(tt,dup)
}
}
hist(tt)
(0 - mean(tt)) / sd(tt) 


