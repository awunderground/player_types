library(stringr)
library(tidyverse)

old.seasons <- read.csv("data/longitudinal_player_stats.csv", header = TRUE, stringsAsFactors = FALSE)
recent.seasons <- read.csv("data/recent_longitudinal_player_stats.csv", header = TRUE, stringsAsFactors = FALSE)

old.seasons <- tbl_df(old.seasons)
recent.seasons <- tbl_df(recent.seasons)

# Clean up all cells and seasons
old.seasons <- old.seasons %>%
    mutate_all(funs(gsub), pattern = "\n", replacement = "") %>%
    mutate_all(funs(trimws)) %>%
    mutate_all(funs(gsub), pattern = "^\\s+", replacement = "") %>%
    mutate(season = substr(link, 50, 51)) %>%
    mutate(season = as.numeric(season)) %>%
    mutate(season = ifelse(season > 17, season + 1900, season + 2000)) 

# Clean up games played and minutes
old.seasons <- old.seasons%>%
    separate(games.played, sep = "-", into = c("games.played", "games.started")) %>%
    mutate(games.played = as.numeric(games.played)) %>%
    mutate(games.started = as.numeric(games.started)) %>%
    separate(minutes, sep = "-", into = c("minutes", "average.minutes")) %>%
    mutate(minutes = as.numeric(minutes)) %>%
    mutate(average.minutes = as.numeric(average.minutes)) 

# Clean up field goals, three points, and free throws
old.seasons <- old.seasons%>%
    separate(fg.fga, sep = "-", into = c("field.goals", "field.goal.attempts")) %>%
    mutate(field.goals = as.numeric(field.goals)) %>%
    mutate(field.goal.attempts = as.numeric(field.goal.attempts)) %>%
    separate(three.pointers, sep = "-", into = c("three.pointers", "three.point.attempts")) %>%
    mutate(three.pointers = as.numeric(three.pointers)) %>%
    mutate(three.point.attempts = as.numeric(three.point.attempts)) %>%
    separate(ft.fta, sep = "-", into = c("free.throws", "free.throw.attempts")) %>%
    mutate(free.throws = as.numeric(free.throws)) %>%
    mutate(free.throw.attempts = as.numeric(free.throw.attempts))

# Clean up rebounds and fouls
old.seasons <- old.seasons %>%
    mutate(total.rebounds = ifelse(grepl("-", reb), NA, reb)) %>%
    separate(reb, sep = "-", into = c("offensive.rebounds", "defensive.rebounds", "total.temp")) %>%
    mutate(total.rebounds = ifelse(is.na(total.rebounds), total.temp, total.rebounds)) %>%
    mutate(offensive.rebounds = ifelse(is.na(defensive.rebounds), NA, offensive.rebounds)) %>%
    mutate(offensive.rebounds = as.numeric(offensive.rebounds)) %>%
    mutate(defensive.rebounds = as.numeric(defensive.rebounds)) %>%
    mutate(total.rebounds = as.numeric(total.rebounds)) %>%
    separate(pf.fo, sep = "-", into = c("fouls", "foul.outs")) %>%
    mutate(fouls = as.numeric(fouls)) %>%
    mutate(foul.outs = as.numeric(foul.outs)) %>%
    select(-total.temp)
    
# Clean up assists, turnovers, blocks, and steals
old.seasons <- old.seasons %>%
    rename(assists = ast, turnovers = to, blocks = blk, steals = stl) %>%
    mutate(assists = as.numeric(assists)) %>%
    mutate(turnovers = as.numeric(turnovers)) %>%
    mutate(blocks = as.numeric(blocks)) %>%
    mutate(steals = as.numeric(steals))

# Clean up points
old.seasons <- old.seasons %>%
    separate(pts.avg, sep = "-", into = c("points", "average.points")) %>%
    mutate(points = as.numeric(points)) %>%
    mutate(average.points = as.numeric(average.points))

# Bind older seasons and recent seasons
long <- bind_rows(old.seasons, recent.seasons)

##
## Create Data Frames 
##

players <- long %>%
    filter(player != "Totals" & player != "Opponents")

seasons <- long %>%
    filter(player == "Totals")

opponents <- long %>%
    filter(player == "Opponents")

