library(tidyverse)
library(softballR)

#### Functions ####

get_current_rpi <- function(scoreboard){
  
  team1_scoreboard <- scoreboard[c(9,1,4,5,8)] %>% `names<-`(c("date","team_name","runs","opponent_name","opponent_runs"))
  team2_scoreboard <- scoreboard[c(9,5,8,1,4)] %>% `names<-`(c("date","team_name","runs","opponent_name","opponent_runs"))
  
  scoreboard_upd <- rbind(team1_scoreboard, team2_scoreboard) %>%
    mutate(win = case_when(runs > opponent_runs ~ 1,
                           runs < opponent_runs ~ 0,
                           runs == opponent_runs ~ 0.5))
  
  
  win_perc <- scoreboard_upd %>%
    group_by(team_name) %>%
    summarise(games = n(),
              win_perc = mean(win)) %>%
    select(-games) %>% 
    drop_na()
  
  scoreboard_upd_2 <- scoreboard_upd %>%
    merge(win_perc, by.x = "opponent_name", by.y = "team_name") %>%
    rename(opponent_win_perc = win_perc) %>%
    merge(win_perc, by = "team_name")
  
  
  opponent_win_perc <- scoreboard_upd_2 %>%
    group_by(team_name) %>%
    summarise(opponent_opponent_win_perc = mean(opponent_win_perc))
  
  scoreboard_upd_3 <- scoreboard_upd_2 %>%
    merge(opponent_win_perc, by.x = "opponent_name", by.y = "team_name")
  
  
  rpi <- scoreboard_upd_3 %>%
    group_by(team_name) %>%
    summarise(rpi_coef = (.5 * mean(win_perc) + .25 * mean(opponent_win_perc) + .25 * mean(opponent_opponent_win_perc)),
              record = paste(floor(sum(win)),floor(n() - sum(win)),ceiling(sum(win) %% 1), sep = "-")) %>%
    ungroup() %>%
    mutate(rpi_rank = rank(-rpi_coef))
  
  
  return(rpi)
}

get_power_ratings <- function(scoreboard){
  
  scoreboard_longer <- rbind(scoreboard[c(9,1,4,5,8)] %>% `names<-`(c("date", "team", "runs", "opponent", "opponent_runs")),
                             scoreboard[c(9,5,8,1,4)] %>% `names<-`(c("date", "team", "runs", "opponent", "opponent_runs")))
  
  rpi <- get_current_rpi(scoreboard) %>%
    select(team_name, rpi_rank)
  
  sos <- scoreboard_longer %>%
    merge(rpi, by.x = "opponent", by.y = "team_name") %>%
    group_by(team) %>%
    summarise(avg_opponent_rpi = mean(rpi_rank)) %>%
    ungroup() %>%
    mutate(rank = rank(avg_opponent_rpi)) %>%
    select(team, rank)
  
  runs_scored <- scoreboard_longer %>% 
    group_by(team) %>% 
    summarise(avg_runs_scored = mean(runs),
              games = n()) %>% 
    select(-games) %>% 
    drop_na()
  
  runs_allowed <- scoreboard_longer %>% 
    group_by(team) %>% 
    summarise(avg_runs_allowed = mean(opponent_runs),
              games = n()) %>% 
    select(-games) %>% 
    drop_na()
  
  best_offenses <- scoreboard_longer %>% 
    merge(runs_allowed, by.x = "opponent", by.y = "team") %>% 
    mutate(diff = runs - avg_runs_allowed) %>% 
    group_by(team) %>% 
    summarise(offensive_rating = mean(diff),
              games = n()) %>% 
    ungroup() %>% 
    drop_na()
  
  best_defenses <- scoreboard_longer %>% 
    merge(runs_scored, by.x = "opponent", by.y = "team") %>% 
    mutate(diff = avg_runs_scored - opponent_runs) %>% 
    group_by(team) %>% 
    summarise(defensive_rating = mean(diff),
              games = n()) %>% 
    ungroup() %>% 
    drop_na()
  
  standings <- scoreboard_longer %>% 
    group_by(team) %>% 
    summarise(wins = sum(runs > opponent_runs, na.rm=T),
              losses = sum(runs < opponent_runs, na.rm=T),
              ties = sum(runs == opponent_runs, na.rm=T),
              win_perc = wins / (wins + losses),
              games = sum(wins, losses, ties)) %>% 
    drop_na() %>% 
    merge(best_offenses, by = "team") %>% 
    merge(best_defenses, by = "team") %>% 
    merge(sos, by = "team") %>% 
    filter(games >= 10) %>% 
    select(team, wins, losses, ties, win_perc, offensive_rating, defensive_rating, rank) 
  
  load("~/Projects/tmking2002.github.io/projects/top_players/power_rating_winperc_model.RDA")
  
  standings$overall_rating <- predict(model, standings) - coef(model)["rank"] * standings$rank
  
  standings$power_rating <- (standings$overall_rating-min(standings$overall_rating)) / 
    (max(standings$overall_rating - min(standings$overall_rating)))
  
  return(standings)
  
}

#### Read box score data and create pitcher stats ####

box <- load_ncaa_softball_playerbox(category = "Pitching")

scoreboard <- load_ncaa_softball_scoreboard(season = 2023)

logos <- scoreboard %>% 
  distinct(home_team, home_team_logo)

power_ratings <- get_power_ratings(scoreboard)

opponent_offensive_stats <- rbind(scoreboard[c(9,1,4,5,8)] %>% 
                                    `names<-`(c("date", "team", "runs", "opponent", "opponent_runs")),
                                  scoreboard[c(9,5,8,1,4)] %>% 
                                    `names<-`(c("date", "team", "runs", "opponent", "opponent_runs"))) %>% 
  merge(power_ratings %>% select(team, offensive_rating), by.x = "opponent", by.y = "team") %>% 
  group_by(team) %>% 
  summarise(avg_opp_off_rating = mean(offensive_rating)) %>% 
  ungroup()

stats <- box %>% 
  separate(ip, c("innings", "frac"), sep = "\\.") %>% 
  mutate(ip = ifelse(is.na(frac), innings, as.numeric(innings) + as.numeric(frac) * 1/3)) %>% 
  select(-c(innings, frac)) %>% 
  mutate(across(c(3:34,39), as.numeric)) %>% 
  group_by(player, team) %>% 
  summarise(across(where(is.numeric), 
                   .fns = sum),
            SOr = so / bf,
            BBr = (bb + hb) / bf,
            ERA = er / ip * 7,
            FIP = ((13 * hr_a) + (3 * (bb + hb)) - 2 * so) / ip) %>% 
  merge(opponent_offensive_stats, by = "team") %>% 
  merge(logos, by.x = "team", by.y = "home_team")

avg_era = mean(stats$ERA * stats$ip) / mean(stats$ip)
avg_fip = mean(stats$FIP * stats$ip) / mean(stats$ip)

fip_constant <- avg_era - avg_fip

stats$FIP <- stats$FIP + fip_constant

model <- lm(FIP ~ avg_opp_off_rating, data = stats)

pitcher_stats_upd <- stats %>% 
  filter(ip >= 50) %>% 
  mutate(wFIP = FIP + coef(model)[2] * avg_opp_off_rating) %>% 
  separate(player, c("last", "first"), ", ") %>% 
  mutate(player = paste(first, last),
         rank = rank(wFIP)) %>% 
  arrange(wFIP) %>% 
  mutate(ip = case_when(between(ip %% floor(ip),.3,.4) ~ as.numeric(paste(as.character(floor(ip)),1,sep = ".")),
                        between(ip %% floor(ip),.6,.7) ~ as.numeric(paste(as.character(floor(ip)),2,sep = ".")),
                        TRUE ~ ip)) %>% 
  select(team, player, ip, w, l, wFIP, ERA, SOr, BBr)

#### Read Yakkertech data ####

yakkertech <- read_csv("~/Projects/yakkertech_data/total_clean.csv")

pitchers <- yakkertech %>% 
  drop_na(SpinAxis) %>% 
  group_by(Pitcher) %>% 
  summarise(count = n(),
            top_speed = max(RelSpeed, na.rm=T)) %>% 
  filter(count >= 50 & Pitcher %in% pitcher_stats_upd$player) %>% 
  select(Pitcher, top_speed)

#### Combine Data ####

yakkertech_data <- yakkertech %>% 
  drop_na(SpinAxis) %>% 
  merge(pitchers, by = "Pitcher") %>% 
  separate(Tilt, c("hr","min","sec"), sep = ":", remove = FALSE) %>% 
  mutate(hr = ifelse(hr == "12","0",hr),
         adj_SpinAxis = (as.numeric(hr) + (as.numeric(min) / 60)) / 12 * 360,
         PitchType = TaggedPitchType)

combined_data <- merge(yakkertech_data, pitcher_stats_upd, by.x = "Pitcher", by.y = "player") %>% 
  filter(Pitcher %in% pitchers$Pitcher) %>% 
  select(Pitcher, PitchType, RelSpeed, PlateLocSide, PlateLocHeight, HorzBreak, InducedVertBreak, adj_SpinAxis, SpinRate, RelSide, RelHeight, wFIP, ERA, SOr, BBr)

#### Pitch Stats ####

pitch_stats <- combined_data %>% 
  group_by(Pitcher, PitchType, wFIP) %>% 
  summarise(across(is.numeric, mean)) %>% 
  ungroup()

# Nothing significant

write_csv(combined_data, "~/Projects/tmking2002.github.io/projects/pitching_metric_analysis/combined_pitcher_data.csv")
