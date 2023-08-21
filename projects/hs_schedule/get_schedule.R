library(tidyverse)
library(rvest)
library(magrittr)
library(anytime)

get_schedule <- function(maxpreps_url, team_name) {
  
  raw <- maxpreps_url %>% 
    read_html()
  
  schedule <- data.frame(opponent = raw %>% html_elements(css = ".heading_100_semibold :nth-child(1)") %>% html_text2(),
                         date = raw %>% html_elements(css = ".jntfAA.heading_100_bold") %>% html_text2() %>% word(2) %>% paste0(., "/2023") %>% anydate(),
                         time = raw %>% html_elements(css = ".jntfAA.detail_90_reg") %>% html_text2(),
                         location = raw %>% html_elements(css = ".hjdrnO.detail_90_reg") %>% html_text2() %>% word(1)) %>% 
    rowwise() %>% 
    mutate(home_team = ifelse(location == "Home", team_name, opponent),
           away_team = ifelse(location == "Home", opponent, team_name),
           home_team = ifelse(location == "Neutral", pmin(team_name, opponent), home_team),
           away_team = ifelse(location == "Neutral", pmax(team_name, opponent), away_team)) %>% 
    select(home_team, away_team, date, time)
  
  
}


tattnall <- get_schedule("https://www.maxpreps.com/ga/macon/tattnall-square-academy-trojans/softball/fall/schedule/", "Tattnall Square Academy")
denmark <- get_schedule("https://www.maxpreps.com/ga/alpharetta/denmark/softball/fall/schedule/", "Denmark")
woodstock <- get_schedule("https://www.maxpreps.com/ga/woodstock/woodstock-wolverines/softball/fall/schedule/", "Woodstock")
buford <- get_schedule("https://www.maxpreps.com/ga/buford/buford-wolves/softball/fall/schedule/", "Buford")
cherokee <- get_schedule("https://www.maxpreps.com/ga/canton/cherokee-warriors/softball/fall/schedule/", "Cherokee")
east_coweta <- get_schedule("https://www.maxpreps.com/ga/sharpsburg/east-coweta-indians/softball/fall/schedule/", "East Coweta")
columbus <- get_schedule("https://www.maxpreps.com/ga/columbus/columbus-blue-devils/softball/fall/schedule/", "Columbus")
milton <- get_schedule("https://www.maxpreps.com/ga/milton/milton-eagles/softball/fall/schedule/", "Milton")
atl_christian <- get_schedule("https://www.maxpreps.com/ga/norcross/greater-atlanta-christian-spartans/softball/fall/schedule/", "Greater Atlanta Christian")
kell <- get_schedule("https://www.maxpreps.com/ga/marietta/kell-longhorns/softball/fall/schedule/", "Kell")
lowndes <- get_schedule("https://www.maxpreps.com/ga/valdosta/lowndes-vikings/softball/fall/schedule/", "Lowndes")
apalachee <- get_schedule("https://www.maxpreps.com/ga/winder/apalachee-wildcats/softball/fall/schedule/", "Apalachee")
north_hall <- get_schedule("https://www.maxpreps.com/ga/gainesville/north-hall-trojans/softball/fall/schedule/", "North Hall")
vidalia <- get_schedule("https://www.maxpreps.com/ga/vidalia/vidalia-indians/softball/fall/schedule/", "Vidalia")

total_schedule <- rbind(tattnall, denmark, woodstock, buford, cherokee, east_coweta, columbus, milton, atl_christian, kell, lowndes, apalachee, north_hall, vidalia) %>% 
  distinct() %>% 
  arrange(date, time)

total_schedule$date <- total_schedule$date + days(1)

write.csv(total_schedule, "~/Projects/tmking2002.github.io/Projects/hs_schedule/schedule.csv")
