library(stringr)
library(tidyverse)

old.seasons <- read_csv("data/longitudinal_player_stats.csv", col_names = TRUE)
recent.seasons <- read_csv("data/recent_longitudinal_player_stats.csv", col_names = TRUE)

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

# Clean names
old.seasons <- old.seasons %>%
    separate(player, sep = ", ", into = c("last.name", "first.name"))

# Bind older seasons and recent seasons
long <- bind_rows(old.seasons, recent.seasons)

##
## Create Data Frames 
##

players <- long %>%
    filter(last.name != "Totals" & last.name != "Opponents")

seasons <- long %>%
    filter(last.name == "Totals")

opponents <- long %>%
    filter(last.name == "Opponents")

rm(long)

# Fix Typos
players <- players %>%
    mutate(last.name = ifelse(last.name == "Pischnalikov", "Pishchalnikov", last.name)) %>%
    mutate(last.name = ifelse(last.name == "Gywnn", "Gwynn", last.name))

# Merge roster by last name and season 
roster <- read_csv("data/longitudinal_roster.csv", col_names = TRUE)

players <- left_join(players, roster, by = c("last.name", "season"))
# This is close, but there are common names 

# Drop cases with different first names
errors <- players[!is.na(players$first.name.y) & !is.na(players$first.name.x), ]

key <- (errors$first.name.x != errors$first.name.y)
errors <- errors[key, ]

# Remove Improperly Merged Data
players <- players %>%
    filter(!(last.name == "Shropshire" & first.name.x == "G." & first.name.y == "Bobby")) %>%
    filter(!(last.name == "Shropshire" & first.name.x == "D." & first.name.y == "Greg")) %>%
    filter(!(last.name == "Brown" & first.name.x == "Fred" & first.name.y == "Michael")) %>%
    filter(!(last.name == "Brown" & first.name.x == "Michael" & first.name.y == "Fred")) %>%
    filter(!(last.name == "Robinson" & first.name.x == "Bruce" & first.name.y == "Alvin")) %>%
    filter(!(last.name == "Robinson" & first.name.x == "Alvin" & first.name.y == "Bruce")) %>%
    filter(!(last.name == "Brown" & first.name.x == "Bo" & first.name.y == "Domonic")) %>%
    filter(!(last.name == "Brown" & first.name.x == "Domonic" & first.name.y == "Bo")) %>%
    filter(!(last.name == "Taylor" & first.name.x == "LaMar" & first.name.y == "Willie")) %>%
    filter(!(last.name == "Taylor" & first.name.x == "Willie" & first.name.y == "LaMar"))

#errors <- players[!is.na(players$first.name.y) & !is.na(players$first.name.x), ]
#key <- (errors$first.name.x != errors$first.name.y)
#errors <- errors[key, ]

# Fix years where minutes are saved as average minutes instead of total minutes
players <- players %>%
    mutate(average.minutes = ifelse(season == 2006, minutes, average.minutes)) %>%
    mutate(minutes = ifelse(season == 2006, minutes * games.played, minutes)) %>%
    mutate(average.minutes = ifelse(season == 2009, minutes, average.minutes)) %>%
    mutate(minutes = ifelse(season == 2009, minutes * games.played, minutes))

# 
players <- players %>%
    mutate(average.minutes = ifelse(is.na(average.minutes), minutes / games.played, average.minutes)) %>%
    mutate(average.points = ifelse(is.na(average.points), points / games.played, average.points))


