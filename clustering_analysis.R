# Analysis with `11 Theus
# Analysis with 4 years of Theus

library(NbClust)
library(tidyverse)
library(useful)

options(scipen = 999)

# Filer to 1998-2016
players <- read_csv("data/final_player_stats.csv") %>%
    filter(season >= 2000 & average.minutes >= 4 & games.played >= 5)

# Calculate team minutes per season
team.minutes <- players %>%
    select(season, minutes, field.goal.attempts, turnovers, free.throw.attempts) %>%
    group_by(season) %>%
    summarize(team.minutes = sum(minutes), 
              team.field.goal.attempts = sum(field.goal.attempts),
              team.turnovers = sum(turnovers),
              team.free.throw.attempts = sum(free.throw.attempts))

# Merge team minutes to player record
players <- left_join(players, team.minutes, by = "season")

# Variables to use for clustering analysis 
players <- players %>%
    mutate(usage = 100 * ((team.minutes / 5) * (field.goal.attempts + (0.44 * free.throw.attempts) + turnovers)) /
               (minutes * (team.field.goal.attempts + (0.44 * team.free.throw.attempts) + team.turnovers))) %>%
    mutate(points.per30 = 30 * (points / minutes)) %>%
    mutate(assists.per30 = 30 * (assists / minutes)) %>%
    mutate(turnovers.per30 = 30 * (turnovers / minutes)) %>%
    mutate(offensive.rebounds.per30 = 30 * (offensive.rebounds / minutes)) %>%
    mutate(defensive.rebounds.per30 = 30 * (defensive.rebounds / minutes)) %>%
    mutate(steals.per30 = 30 * (steals / minutes)) %>%
    mutate(blocks.per30 = 30 * (blocks / minutes)) %>%
    mutate(fouls.per30 = 30 * (fouls / minutes)) %>%
    mutate(pounds.per.inch = weight / height.inches) %>%
    mutate(true.shooting.attempts = field.goal.attempts + 0.44 * free.throw.attempts) %>%
    mutate(true.shooting.proportion = points / (2 * true.shooting.attempts)) %>%
    mutate(free.throw.rate = free.throw.attempts / true.shooting.attempts) %>%
    mutate(three.point.rate = three.point.attempts / true.shooting.attempts) %>%
    select(season, 
           last.name, 
           first.name.x, 
           average.minutes, 
           usage,
           games.played,
           points.per30,
           true.shooting.proportion,
           free.throw.rate,
           three.point.rate,
           assists.per30,
           turnovers.per30,
           offensive.rebounds.per30,
           defensive.rebounds.per30,
           steals.per30,
           blocks.per30,
           fouls.per30, 
           pounds.per.inch,
           height.inches)
    
# Replace undefined shooting proportions with zeroes
players <- players %>%
    mutate_all(funs(ifelse(is.nan(.), 0, .)))

# Standardize Variables
players.s <- players %>%
    mutate_each(funs(scale), average.minutes:height.inches)
    
# NbClust
NbClust(players.s[, 4:19], min.nc = 4, max.nc = 10, method = "kmeans")
# Three is the optimal number of clusters

players.clust <- (kmeans(players.s[, 4:19], centers = 4, nstart = 4)) 

plot.kmeans(players.clust, data = players.s)

# TODO(awunderground): Pick a minutes limit