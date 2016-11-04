library(rvest)
library(stringr)
library(tidyverse)


# 71-72 Name GP FG-FGA Pct. FT-FTA Pct.Reb. Avg. PF Ast. Pts.-Avg.

# Task 1 get all names

# Web addresses for 197879 - 200910 Rosters
start.year <- 1971:2008
end.year <- start.year + 1

stats <- data_frame(start.year, end.year)

stats <- stats %>%
    mutate(start.year = as.character(start.year)) %>%
    mutate(end.year = as.character(end.year)) %>%
    mutate(key = substr(end.year, 3, 4)) %>%
    mutate(key = paste0(start.year, key)) %>%
    mutate(key = paste0(key, "_Stats")) %>%
    mutate(key = paste0("http://vcuathletics.com/sports/mbkb/archives/", key))

statScraperA <- function (link) {
    temp <- read_html(as.character(link))
    
    link <- as.character(link)

    player <- temp %>% 
        html_nodes("td:nth-child(1)") %>%
        html_text() 
    
    games.played <- temp %>% 
        html_nodes("td:nth-child(2)") %>%
        html_text() 
    
    field.goals <- temp %>% 
        html_nodes("td:nth-child(3)") %>%
        html_text() 
    
    free.throws <- temp %>% 
        html_nodes("td:nth-child(5)") %>%
        html_text() 
        
    rebounds <- temp %>% 
        html_nodes("td:nth-child(7)") %>%
        html_text() 
    
    fouls <- temp %>% 
        html_nodes("td:nth-child(9)") %>%
        html_text()       
    
    assists <- temp %>% 
        html_nodes("td:nth-child(10)") %>%
        html_text() 
    
    boom <- cbind(link, player, games.played, field.goals, free.throws, rebounds, 
                  fouls, assists)
    
    return(boom)
}

players <- NULL
for (i in c(1, 3,4)) {
    
    temp <- statScraperA(stats[i, 3])
    
    players <- rbind(players, temp)
}

statScraperB <- function (link) {
    temp <- read_html(as.character(link))
    
    link <- as.character(link)
    
    player <- temp %>% 
        html_nodes("td:nth-child(1)") %>%
        html_text() 
    
    games.played <- temp %>% 
        html_nodes("td:nth-child(2)") %>%
        html_text() 
    
    vector3 <- temp %>% 
        html_nodes("td:nth-child(3)") %>%
        html_text() 
    
    label <- rep(vector3[1], length(vector3))
    
    boom <- cbind(link, player, games.played, vector3, label)
    
    return(boom)
}

players <- NULL
for (i in 1:nrow(stats)) {
    
    temp <- statScraperB(stats[i, 3])
    
    players <- rbind(players, temp)
}

players %>%
    mutate(field.goals = ifelse("FG", grepl(vector3), ))

# scrape to list






# 74-75 3 is fgs 
# 72-73, 76-09 3 is minutes



field.goals <- temp %>% 
    html_nodes("td:nth-child(3)") %>%
    html_text() 









temp <- read_html("http://vcuathletics.com/sports/mbkb/archives/197172_Stats")

boom72 <- temp %>% 
    html_nodes("td:nth-child(2)") %>%
    html_text() 

















vcu2016 <- read_html("http://www.sports-reference.com/cbb/schools/virginia-commonwealth/2016.html#all_roster")

roster2016 <- vcu2016 %>% 
    html_nodes("#roster tbody th") %>%
    html_text() 

http://www.sports-reference.com/cbb/players/jordan-burgess-1/gamelog/2014/
    
moaliecox14 <- read_html("http://www.sports-reference.com/cbb/players/mo-alie-cox-1/gamelog/2014/")
moaliecox15 <- read_html("http://www.sports-reference.com/cbb/players/mo-alie-cox-1/gamelog/2015/")
moaliecox16 <- read_html("http://www.sports-reference.com/cbb/players/mo-alie-cox-1/gamelog/2016/")

date <- moaliecox16 %>% 
    html_nodes("tbody .right+ .left") %>%
    html_text() 

points <- moaliecox16 %>% 
    html_nodes("tbody .right:nth-child(30)") %>%
    html_text() 

moaliecox <- data_frame(date, points) 

# Rselenium