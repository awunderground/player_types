# Analysis with `11 Theus
# Analysis with 4 years of Theus

library(tidyverse)

# Filer to 1998-2016
players <- read_csv("data/final_player_stats.csv") %>%
    filter(season >= 2000)

# Variables to use for clustering analysis 
players <- players %>%
    mutate(points.per.minute = points / minutes) %>%
    mutate(assists.per.minute = assists / minutes) %>%
    mutate(turnovers.per.minute = turnovers / minutes) %>%
    mutate(offensive.rebounds.per.minute = offensive.rebounds / minutes) %>%
    mutate(defensive.rebounds.per.minute = defensive.rebounds / minutes) %>%
    mutate(steals.per.minute = steals / minutes) %>%
    mutate(blocks.per.minute = blocks / minutes) %>%
    mutate(fouls.per.minute = fouls / minutes) %>%
    mutate(pounds.per.inch = weight / height.inches) %>%
    select(season, 
           last.name, 
           first.name.x, 
           average.minutes, 
           games.played,
           points.per.minute,
           two.point.prop,
           two.point.attempts, 
           three.point.prop,
           three.point.attempts,
           free.throw.prop,
           free.throw.attempts, 
           assists.per.minute,
           turnovers.per.minute,
           offensive.rebounds.per.minute,
           defensive.rebounds.per.minute,
           steals.per.minute,
           blocks.per.minute,
           fouls.per.minute, 
           pounds.per.inch)
    
# Replace undefined shooting proportions with zeroes
players <- players %>%
    mutate_all(funs(ifelse(is.nan(.), 0, .)))

# Standardize Variables




# TOD)(awunderground): add usage %
# TODO(awunderground): fix Jamal Shuler
# TODO(awunderground): Standardize all inputs