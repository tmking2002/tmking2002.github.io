library(softballR)
library(tictoc)
library(tidyverse)
library(anytime)

options(warn = -1)

get_pitching_box <- function(id){
  
  raw <- glue::glue("https://stats.ncaa.org/contests/{id}/box_score") %>%
    readLines()
  
  pitching_id <- raw[grep("\t   <a href=\"/game/box_score/", raw)] %>%
    stringr::str_remove_all("\t   <a href=\"/game/box_score/|\">Box Score </a>")
  
  raw <- glue::glue("https://stats.ncaa.org/game/box_score/{pitching_id}?year_stat_category_id=15021") %>%
    rvest::read_html() %>%
    rvest::html_table()
  
  first_team <- as.character(raw[[6]][1,1])
  second_team <- as.character(raw[[7]][1,1])
  
  upd <- rbind(raw[[6]],raw[[7]]) %>%
    `names<-`(raw[[6]][2,]) %>%
    janitor::clean_names() %>%
    dplyr::filter(!(player %in% c(first_team, second_team,"Player","Totals")))
  
  upd$team <- ifelse(upd$player %in% raw[[6]]$X1, first_team, second_team)
  upd$opponent <- ifelse(upd$team == first_team, second_team, first_team)
  upd[upd == ""] <- "0"
  upd[] <- lapply(upd, gsub, pattern="/", replacement="")
  
  upd <- upd %>%
    dplyr::mutate(across(3:26, as.numeric)) %>%
    dplyr::filter(ip > 0) %>%
    dplyr::mutate(game_id = id)
  
  return(upd)
  
}

get_hitting_box <- function(id){
  
  raw <- glue::glue("https://stats.ncaa.org/contests/{id}/box_score") %>%
    rvest::read_html() %>%
    rvest::html_table()
  
  first_team <- as.character(raw[[6]][1,1])
  second_team <- as.character(raw[[7]][1,1])
  
  upd <- rbind(raw[[6]],raw[[7]]) %>%
    `names<-`(raw[[6]][2,]) %>%
    janitor::clean_names() %>%
    dplyr::filter(!(player %in% c(first_team, second_team,"Player","Totals")))
  
  upd$team <- ifelse(upd$player %in% raw[[6]]$X1, first_team, second_team)
  upd$opponent <- ifelse(upd$team == first_team, second_team, first_team)
  upd[upd == ""] <- "0"
  
  upd <- upd %>%
    dplyr::mutate(across(3:26, as.numeric)) %>%
    dplyr::mutate(game_id = id)
  
  return(upd)
  
}

curr_hitting_box <- readRDS("d1_hitting_box_scores_2023.RDS")
curr_pitching_box <- readRDS("d1_pitching_box_scores_2023.RDS")

scoreboard <- load_ncaa_scoreboard(2023, division = "D1") %>% 
  distinct(game_id, game_date)

#curr_hitting_box <- merge(curr_hitting_box, scoreboard, by = "game_id")
#curr_pitching_box <- merge(curr_pitching_box, scoreboard, by = "game_id")

most_recent <- min(c(max(anydate(curr_hitting_box$game_date)), 
                     max(anydate(curr_pitching_box$game_date))))

game_ids <- scoreboard %>% filter(anydate(game_date) > most_recent) %>%  pull(game_id) %>% sort

get_ncaa_hitter_player_box <- function(game_id){
  
  i <<- i + 1
  
  hitting <- try(get_hitting_box(game_id))
  
  if(i %% 100 == 0) {toc(); tic(i+100)}
  
  return(hitting)
  
}

get_ncaa_pitcher_player_box <- function(game_id){
  
  i <<- i + 1
  
  pitching <- try(get_pitching_box(game_id))
  
  if(i %% 100 == 0) {toc(); tic(i+100)}
  
  return(pitching)
  
}

# Hitter box scores

i <- 0

tic(100)
box <- do.call(rbind, lapply(X = game_ids, FUN = get_ncaa_hitter_player_box))
toc()

box <- box %>% 
  filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>% 
  merge(scoreboard, by = "game_id")

hitting_box <- rbind(curr_hitting_box, box) %>% 
  distinct()

saveRDS(object = hitting_box, file = "d1_hitting_box_scores_2023.RDS")

# Pitcher box scores

i <- 0

tic(100)
box <- do.call(rbind, lapply(X = game_ids, FUN = get_ncaa_pitcher_player_box))
toc()

box <- box %>% 
  filter(!str_detect(player,"Error : Document is empty|subscript out of bounds|Timeout was reached")) %>% 
  merge(scoreboard, by = "game_id")

pitching_box <- rbind(curr_pitching_box, box) %>% 
  distinct()

saveRDS(object = pitching_box, file = "d1_pitching_box_scores_2023.RDS")
