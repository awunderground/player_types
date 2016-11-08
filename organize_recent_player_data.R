library(stringr)
library(tidyverse)

year <- 2010:2016
season <- rep("season", length(year))
key <- paste0("data/", year, season, ".csv")

long <- data_frame()
for (i in 1:length(key)) {
    
    temp <- read.csv(key[i], header = TRUE, stringsAsFactors = FALSE)
    
    temp <- mutate(temp, season = i + 2009)

    long <- bind_rows(long, temp)
    
}

long <- long %>%
    select(-Rk, -FG., -X2P, -X2PA, -X2P., -X3P., -FT.) %>%
    rename(player = Player, 
           games.played = G, 
           minutes = MP, 
           field.goals = FG, 
           field.goal.attempts = FGA,
           three.pointers = X3P,
           three.point.attempts = X3PA,
           free.throws = FT,
           free.throw.attempts = FTA,
           offensive.rebounds = ORB,
           defensive.rebounds = DRB,
           total.rebounds = TRB,
           assists = AST, 
           steals = STL,
           blocks = BLK,
           turnovers = TOV,
           fouls = PF,
           points = PTS)

long <- long %>%
    separate(player, sep = "\\\\", into = c("player", "trash")) %>%
    select(-trash)

write.csv(long, "data/recent_longitudinal_player_stats.csv", row.names = FALSE)