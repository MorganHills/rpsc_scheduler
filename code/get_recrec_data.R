#rm(list = ls())
source("./code/main.R")

main$func$require_packages(
  c("tidyverse","httr","jsonlite","DBI"
    ,"XML","xml2","rvest")
)

rec <- list()
rec$func <- list()
rec$venues <- list(
  brooklyn = list(
    name = "Brooklyn"
    ,website = "https://app.recrec.io/royal-palms-brooklyn"
    ,metadata = list(
      leagues = data.frame()
      ,divisions = data.frame()
    )
    ,leagues = list()
  )
)

rec$func$get_leagues_ids <- function(URL){
  pg <- read_html(URL)
  tmp <-html_nodes(pg,"tr") 
  doc <- htmlParse(tmp)
  links <- xpathSApply(doc, "//a/@href")
  str_remove(links,"/seasons/") %>%
    str_remove("/standings") %>% 
    str_remove("/info") %>% unique
}

rec$func$get_division_ids <- function(leagueId){
 # leagueId <- "317CBm"
  URL <- sprintf("https://app.recrec.io/seasons/%s/info",leagueId)
  pg <- read_html(URL)
  tmp <-html_nodes(pg,"tr") 
  doc <- htmlParse(tmp)
  links <- xpathSApply(doc, "//a/@href")
  divids <- str_remove(links,"/divisions/") 
  divids[!grepl("new",divids)]
}

rec$func$get_matches <- function(divisionId){
  #divisionId <- "o5gspj"
  endpoint <- sprintf("https://app.recrec.io/api/matches?api_key=%s&division_id=%s",
                      main$config$recrec_api_key
                      ,divisionId)
  resp <- GET(endpoint)
  
  resp$content %>%
    rawToChar %>% fromJSON
}

for(i in seq_along(rec$venues)){
  #i <- 1
    rec$venues[[i]]$metadata$leagues <- 
            data.frame(ids = rec$func$get_leagues_ids(rec$venues[[i]]$website))
    
    rec$venues[[i]]$leagues <- lapply(rec$venues[[i]]$metadata$leagues$ids,
                                      function(x) list(name = x ))
    
    for(y in seq_along(rec$venues[[i]]$metadata$leagues$ids)){
      #y <- 1 
      rec$venues[[i]]$leagues[[y]]$divisions <- 
        rec$func$get_division_ids(rec$venues[[i]]$leagues[[y]]$name)
      
      rec$venues[[i]]$leagues[[y]]$matches <- list()
      rec$venues[[i]]$leagues[[y]]$matches <- 
        lapply(rec$venues[[i]]$leagues[[y]]$divisions, function(z)
          rec$venues[[i]]$leagues[[y]]$matches[[z]] <- NULL
          )
      names(rec$venues[[i]]$leagues[[y]]$matches) <- 
        rec$venues[[i]]$leagues[[y]]$divisions
        
      for(x in seq_along(rec$venues[[i]]$leagues[[y]]$divisions) ){
        #x <- 1
        rec$venues[[i]]$leagues[[y]]$matches[[x]] <-
          rec$func$get_matches(rec$venues[[i]]$leagues[[y]]$divisions[x])
        
      }
    }
    
}
rm(list = c("i","y","x"))

rec$data <- data.frame(
 id                                 =as.character() 
 ,time                               =as.character() 
 ,away_score                         =as.integer() 
 ,home_score                         =as.integer()  
 ,comment                            =as.logical() 
 ,elo_multiplier                     =as.double()
 ,away_elo_cache_current             =as.integer()  
 ,away_elo_cache_previous            =as.integer()  
 ,home_elo_cache_current             =as.integer()  
 ,home_elo_cache_previous            =as.integer()  
 ,division.id                        =as.character() 
 ,division.name                      =as.character() 
 ,division.color                     =as.character() 
 ,division.day_of_week               =as.character() 
 ,division.start_time                =as.character() 
 ,division.end_time                  =as.character() 
 ,division.season_id                 =as.character() 
 ,location.id                        =as.character() 
 ,location.name                      =as.character() 
 ,location.identifier                =as.character() 
 ,location.venue_id                  =as.character() 
 ,away_competitor.id                 =as.character() 
 ,away_competitor.type               =as.character() 
 ,away_competitor.name               =as.character() 
 ,away_competitor.instagram_user     =as.character() 
 ,away_competitor.elo_cache_current  =as.integer()  
 ,away_competitor.elo_cache_previous =as.integer()  
 ,home_competitor.id                 =as.character() 
 ,home_competitor.type               =as.character() 
 ,home_competitor.name               =as.character() 
 ,home_competitor.instagram_user     =as.character() 
 ,home_competitor.elo_cache_current  =as.integer()  
 ,home_competitor.elo_cache_previous =as.integer()  
)

for(v in 1:length(rec$venues)){
  #v <- 1 
  for(l in 1:length(rec$venues[[v]]$metadata$leagues$ids)){
    #l <- 1
    for(d in 1:length(rec$venues[[v]]$leagues[[l]]$divisions)){
        #d <- 1 
        s <- rec$venues[[v]]$leagues[[l]]$matches[[d]]
        if(!is.data.frame(s)) {break}
        if(nrow(s) == 0) {break}
        s <- flatten(s)
            
        rec$data <- bind_rows(rec$data,s)
    }
  }
}
rm(list = c("v","l","d","s"))

names(rec$data) <- names(rec$data) %>% 
  sapply(function(x) gsub("\\.","_",x))

saveRDS(rec,file = "./data/rec_data.RDS")
