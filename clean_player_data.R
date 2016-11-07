library(stringr)
library(tidyverse)

old.seasons <- read.csv("data/longitudinal_player_stats.csv", header = TRUE, stringsAsFactors = FALSE)
recent.seasons <- read.csv("data/recent_longitudinal_player_stats.csv", header = TRUE, stringsAsFactors = FALSE)

old.seasons <- tbl_df(old.seasons)
recent.seasons <- tbl_df(recent.seasons)

boom <- old.seasons %>%
    mutate_all(funs(gsub), pattern = "\n", replacement = "") %>%
    mutate_all(funs(trimws)) %>%
    mutate(season = substr(link, 50, 51)) %>%
    mutate(season = as.numeric(season)) %>%
    mutate(season = ifelse(season > 17, season + 1900, season + 2000)) %>%
    separate(games.played, sep = "-", into = c("games.played", "games.started")) %>%
    mutate(games.played = as.numeric(games.played)) %>%
    mutate(games.played = as.numeric(games.played)) %>%
    separate(minutes, sep = "-", into = c("minutes", "average.minutes")) %>%
    mutate(minutes = as.numeric(minutes)) %>%
    mutate(average.minutes = as.numeric(average.minutes)) %>%
    separate(fg.fga, sep = "-", into = c("field.goals", "field.goal.attempts")) %>%
    mutate(field.goals = as.numeric(field.goals)) %>%
    mutate(field.goal.attempts = as.numeric(field.goal.attempts)) %>%
    separate(three.pointers, sep = "-", into = c("three.pointers", "three.point.attempts")) %>%
    mutate(three.pointers = as.numeric(three.pointers)) %>%
    mutate(three.point.attempts = as.numeric(three.point.attempts)) %>%
    separate(ft.fta, sep = "-", into = c("free.throws", "free.throw.attempts")) %>%
    mutate(free.throws = as.numeric(free.throws)) %>%
    mutate(free.throw.attempts = as.numeric(free.throw.attempts)) %>%
    mutate(total.rebounds = ifelse(grepl("-", reb), NA, reb)) %>%
    separate(reb, sep = "-", into = c("offensive.rebounds", "defensive.rebounds", "total.temp")) %>%
    mutate(total.rebounds = ifelse(is.na(total.rebounds), total.temp, total.rebounds)) %>%
    mutate(offensive.rebounds = ifelse(is.na(defensive.rebounds), NA, offensive.rebounds))

zoom <- mutate(boom, zam = ifelse(total.rebounds == NA, total.temp, total.rebounds)) %>%
    select(total.rebounds)




