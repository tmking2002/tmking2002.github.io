install.packages("tidyverse")
install.packages("rvest")
install.packages("magrittr")
install.packages("janitor")
install.packages("anytime")

library(tidyverse)
library(rvest)
library(magrittr)
library(janitor)
library(anytime)

schedule_1 <- "https://tourneymachine.com/Public/Results/Division.aspx?IDTournament=h20230821170245450d5f59e3badd64b&IDDivision=h2023100220310056588c707e9408541" %>% 
  read_html() %>% 
  html_table()

max_rows <- -1
max_index <- -1

for (i in 1:length(schedule_1)) {
  n_rows <- nrow(schedule_1[[i]])
  if (n_rows > max_rows) {
    max_rows <- n_rows
    max_index <- i
  }
}

schedule_1 <- schedule_1 %>% 
  extract2(max_index) %>% 
  row_to_names(1) %>% 
  clean_names() %>% 
  filter(!(game %in% c("×\r\n                            \r\n                        \r\n                    \r\n                    \r\n                        \r\n                            \r\n                                ×",
                       "Game", "Saturday, November 04, 2023", "Sunday, November 05, 2023"))) %>% 
  separate(location, c("venue", "field_num"), " - Field ") %>% 
  separate(time, c("date", "time"), "\r\n                    \r\n                    ") %>% 
  mutate(date = str_remove(date, "Fri |Sat |Sun ")) %>% 
  rename(team_1 = team) %>% 
  mutate(team_1 = gsub(" Pool [A-Z]*|\\*", "", team_1),
         team_2 = gsub(" Pool [A-Z]*|\\*", "", team_2)) %>% 
  select(game, date, time, venue, field_num, team_1, team_2)

schedule_2 <- "https://tourneymachine.com/Public/Results/Division.aspx?IDTournament=h20230821170245450d5f59e3badd64b&IDDivision=h202310022031005913345b65e1bfb4c" %>% 
  read_html() %>% 
  html_table()

max_rows <- -1
max_index <- -1

for (i in 1:length(schedule_2)) {
  n_rows <- nrow(schedule_2[[i]])
  if (n_rows > max_rows) {
    max_rows <- n_rows
    max_index <- i
  }
}


schedule_2 <- schedule_2 %>% 
  extract2(max_index) %>% 
  row_to_names(1) %>% 
  clean_names() %>% 
  filter(!(game %in% c("×\r\n                            \r\n                        \r\n                    \r\n                    \r\n                        \r\n                            \r\n                                ×",
                       "Game", "Saturday, November 04, 2023", "Sunday, November 05, 2023"))) %>% 
  separate(location, c("venue", "field_num"), " - Field ") %>% 
  separate(time, c("date", "time"), "\r\n                    \r\n                    ") %>% 
  mutate(date = str_remove(date, "Fri |Sat |Sun ")) %>% 
  rename(team_1 = team) %>% 
  mutate(team_1 = gsub(" Pool [A-Z]*|\\*", "", team_1),
         team_2 = gsub(" Pool [A-Z]*|\\*", "", team_2)) %>% 
  select(game, date, time, venue, field_num, team_1, team_2)

teams_to_watch <- c("Rock Gold Manetta", "Georgia Impact Holcombe", "Birmingham Thunderbolts Premier 2027-Alford", "GA Impact Maher", 
                    "LLG Elite", "Tampa Mustangs PBell 2026/2027", "Top Gun 16U National - Slezak", "Indiana Magic Gold Bennett/Goddard", "Turnin2 Robeson / Long", "Starz Gold Bunn", 
                    "Unity meadows/Johnson", "LLG Elite", "Fury Platinum X - Hutchins", "Fury Platinum X Hutchins Helton", 
                    "Birmingham Thunderbolts Premier 2026 - Thompson", "Birmingham Thunderbolts Premier 2027-Alford", "Georgia Impact Premier Caymol",
                    "Birmingham Thunderbolts Premier 2025 Kemp", "Fury Platinum National Chiles- Powered by SCT", "GA Impact Taylor", "GA Impact Maher",
                    "Aces Fastpitch", "Hotshots National ATL", "Select Fastpitch", "TAMPA MUSTANGES RENE", "Tampa Mustangs Seymour",
                    "Birmingham Thunderbolts Premier 2027-Alford")

schedule_final <- rbind(schedule_1, schedule_2) %>% 
  filter(str_detect(team_1, paste(teams_to_watch, collapse = "|")) | 
         str_detect(team_2, paste(teams_to_watch, collapse = "|"))) %>% 
  mutate(date_time = paste(date, time),
         date_time = strptime(date_time, format = "%m/%d/%y %I:%M %p")) %>% 
  arrange(date_time, venue) %>% 
  select(-date_time)


write_csv(schedule_final, "projects/hs_schedule/scenic_city_schedule.csv")
