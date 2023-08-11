#### Installing softballR ####

# This downloads the functions to your computer (you only need to run this once)
devtools::install_github("sportsdataverse/softballR")

# This loads the functions into your environment (run this every time you start a new R session)
library(softballR)

#### Accessing Data ####

## Scoreboard ##

# Run this line if you've never installed the 'tictoc', 'magrittr', or 'dplyr' packages
# install.packages("tictoc")
# install.packages("dplyr")
# install.packages("magrittr)

library(tictoc)
library(magrittr)
library(dplyr)

# This code saves the scores of every game in division 1 in 2023 to the 'scoreboard' object.
# It usually takes less than a second to run!
tictoc::tic()
scoreboard <- softballR::load_ncaa_softball_scoreboard(season = 2023,
                                                       division = "D1")
tictoc::toc()

# This sequence of code gives you a preview of the structure of the 'scoreboard' object
scoreboard %>%
  dplyr::select(home_team, home_team_runs, away_team, away_team_runs, game_date) %>%
  head(n = 10)

## Box Scores ##

# This code saves the hitting box score data from the entire 2023 division 2 season to the 'box' object
tictoc::tic()
box <- softballR::load_ncaa_softball_playerbox(season = 2023,
                                               division = "D2",
                                               category = "Hitting")
tictoc::toc()

# This sequence of code gives you a preview of the structure of the 'box' object
box %>%
  dplyr::select(game_id, team, opponent, player, pos, ab, h, x2b, x3b, hr) %>%
  head(n = 10)

#### Manipulating Data ####

## Creating RPI ##

# First reformat 'scoreboard' data to longer format
home_scoreboard <- scoreboard %>%
  dplyr::select(game_id, home_team, home_team_runs, away_team, away_team_runs) %>%
  `names<-`(c("game_id", "team", "runs", "opponent", "opponent_runs"))

away_scoreboard <- scoreboard %>%
  dplyr::select(game_id, away_team, away_team_runs, home_team, home_team_runs) %>%
  `names<-`(c("game_id", "team", "runs", "opponent", "opponent_runs"))

scoreboard_upd <- rbind(home_scoreboard, away_scoreboard) %>%
  dplyr::mutate(win = dplyr::case_when(runs > opponent_runs ~ 1,
                                       runs < opponent_runs ~ 0,
                                       TRUE ~ 0.5))

# Calculate win percentage by team

wp <- scoreboard_upd %>%
  dplyr::group_by(team) %>%
  dplyr::summarise(games = n(),
                   win_perc = mean(win)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(games >= 30) %>% # Filter to only include teams with >= 30 games
  dplyr::select(-games)

# Calculate opponents' win percentage by team

owp <- scoreboard_upd %>%
  merge(wp, by.x = "opponent", by.y = "team") %>%
  dplyr::group_by(team) %>%
  dplyr::summarise(opp_win_perc = mean(win_perc))

# Calculate opponents' opponents' win percentage by team

oowp <- scoreboard_upd %>%
  merge(owp, by.x = "opponent", by.y = "team") %>%
  dplyr::group_by(team) %>%
  dplyr::summarise(opp_opp_win_perc = mean(opp_win_perc))

# Combine all of these to create RPI

rpi <- merge(wp, owp, by = "team") %>% merge(oowp, by = "team") %>%
  dplyr::mutate(rpi = 0.25 * win_perc + 0.5 * opp_win_perc + 0.25 * opp_opp_win_perc,
                rank = rank(-rpi))

rpi %>%
  dplyr::select(rank, team, rpi) %>%
  dplyr::arrange(rank) %>%
  head(n = 10)

## Creating OPS

# Because every game is in there individually, you have to sum by player to get aggregate stats
stats <- box %>%
  dplyr::group_by(team, player) %>%
  dplyr::mutate(across(
    .cols = 4:76,
    .fns = \(col) as.numeric(str_remove(col, "\\/")))) %>% # Convert values to numeric type
  dplyr::summarise(pa = (sum(ab) + sum(bb_2) + sum(hbp) + sum(sh) + sum(sf)),
                   obp = (sum(h) + sum(bb_2) + sum(hbp)) / pa,
                   slg = sum(tb) / sum(ab),
                   ops = obp + slg) %>%
  dplyr::filter(pa >= 100) # Only include batters with >= 100 plate appearances

# Here are the top ten OPS leaders in the 2023 D2 season:
stats %>%
  arrange(desc(ops)) %>%
  head(n = 10)

#### Head Coach Wins Leaders Graphics ####

logos_d1 <- load_ncaa_softball_scoreboard(2023, "D1") %>% distinct(home_team, home_team_logo)
logos_d2 <- load_ncaa_softball_scoreboard(2023, "D2") %>% distinct(home_team, home_team_logo)
logos_d3 <- load_ncaa_softball_scoreboard(2023, "D3") %>% distinct(home_team, home_team_logo)

logos <- rbind(logos_d1, logos_d2, logos_d3) %>% 
  distinct() %>% 
  rename(team_logo = home_team_logo) %>% 
  mutate(home_team = str_replace_all(home_team, c("&amp;" =  "&",
                                                  "&#39;" = "'")))

teams <- load_ncaa_softball_team_info() %>%
  mutate(team_id = as.numeric(team_id)) %>% 
  distinct(team_name, team_id) %>% 
  merge(logos, by.x = "team_name", by.y = "home_team", all = T)

# install.packages("tidyr")
library(tidyr)

coaches <- load_ncaa_softball_team_info() %>% 
  filter(head_coach != "") %>% 
  select(team_id, season, head_coach, wins, losses, ties) %>% 
  mutate(ties = ifelse(!is.na(wins), replace_na(ties, 0), ties)) %>% 
  group_by(season, team_id, head_coach) %>% 
  mutate(coach_season_id = cur_group_id())

# install.packages("gt")
# install.packages("gtExtras")
library(gt)
library(gtExtras)

coaches %>% 
  group_by(head_coach, team_id) %>% 
  summarise(wins = sum(wins),
            losses = sum(losses),
            win_perc = wins / (wins + losses),
            first_season = min(season),
            last_season = max(season)) %>% 
  ungroup() %>% 
  mutate(seasons = paste(first_season, last_season, sep = "-")) %>% 
  merge(teams, by = "team_id") %>% 
  arrange(desc(wins)) %>% 
  head(n=10) %>% 
  select(team_logo, head_coach, wins, losses, win_perc, seasons) %>% 
  gt() %>% 
  cols_label(team_logo = "",
             head_coach = "Head Coach",
             wins = "W",
             losses = "L",
             win_perc = "Win%",
             seasons = "Seasons") %>% 
  gt_img_rows(team_logo) %>% 
  fmt_percent(win_perc, decimals = 1) %>% 
  tab_style(style = cell_fill(color = "#841617"),
            locations = cells_body(columns = 2:6,
                                   rows = head_coach == "Patty Gasso")) %>% 
  tab_style(style = cell_text(color = "white"),
            locations = cells_body(columns = 2:6,
                                   rows = head_coach == "Patty Gasso")) %>% 
  tab_style(style = cell_borders(sides = "bottom", color = "lightgrey"),
            locations = cells_body(rows = head_coach == "Margo Jonker")) %>% 
  tab_header(title = "Winningest Coaches With One Team All Time",
             subtitle = "(NCAA Softball)") %>% 
  tab_footnote(footnote = "Data from softballR") %>% 
  gt_theme_538() %>% 
  tab_options(heading.align = 'center')

# install.packages("nflplotR")
library(nflplotR)

# install.packages("ggplot2")
library(ggplot2)

wins_by_season <- coaches %>% 
  merge(teams, by = "team_id") %>% 
  filter(head_coach %in% c("Carol Hutchins", "Mike Candrea", "Patty Gasso", "Lori Meyer", "Diane Ninemire")) %>% 
  group_by(head_coach, team_logo) %>% 
  arrange(season) %>% 
  summarise(year = row_number(),
            wins = cumsum(wins))

ggplot(wins_by_season, aes(x = year, y = wins, group = head_coach)) +
  geom_from_path(data = wins_by_season %>% group_by(head_coach) %>% filter(year == max(year)),
                 aes(path = team_logo), width = 0.08) + 
  geom_line(aes(color = head_coach),  linewidth = 1) + 
  scale_color_manual(values = c("Patty Gasso" = "#841617")) +
  geom_hline(yintercept = max(wins_by_season$wins), linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Cumulative Wins by Season",
       subtitle = "(By Coach)",
       caption = "Data from softballR | Viz by @tking0426",
       x = "Season",
       y = "Wins")
