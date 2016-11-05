library(rvest)
library(stringr)
library(tidyverse)

long <- read.csv("data/longitudinal_player_stats.csv", header = TRUE, stringsAsFactors = FALSE)

long <- tbl_df(long)

boom <- long %>%
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
    separate(ft.fta, sep = "-", into = c("free.throws", "free.throw.attempts")) 