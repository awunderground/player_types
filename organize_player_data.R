library(rvest)
library(stringr)
library(tidyverse)


# Rosters


# Web addresses for 197879 - 200910 Rosters
start.year <- 1978:2009
end.year <- start.year + 1

rosters <- data_frame(start.year, end.year)

rosters <- rosters %>%
    mutate(start.year = as.character(start.year)) %>%
    mutate(end.year = as.character(end.year)) %>%
    mutate(key = substr(end.year, 3, 4)) %>%
    mutate(key = paste0(start.year, key)) %>%
    mutate(key = paste0(key, "_roster")) %>%
    mutate(key = paste0("http://vcuathletics.com/sports/mbkb/archives/", key))

# Pulls a single year's roster from the VCU Athletic's website
rosterScraper <- function (link) {
    temp <- read_html(as.character(link))
    
    link <- as.character(link)
    
    jersey <- temp %>% 
        html_nodes("td:nth-child(1) , tr+ tr td:nth-child(1) div") %>%
        html_text() 
    
    player <- temp %>% 
        html_nodes("td:nth-child(2) , tr+ tr td:nth-child(2) div") %>%
        html_text() 
    
    class <- temp %>% 
        html_nodes("td:nth-child(3) , tr+ tr td:nth-child(3) div") %>%
        html_text()  
        
    height <- temp %>% 
        html_nodes("td:nth-child(4) , tr+ tr td:nth-child(4) div") %>%
        html_text()      
    
    weight <- temp %>% 
        html_nodes("td:nth-child(5) , tr+ tr td:nth-child(5) div") %>%
        html_text()  
    
    boom <- cbind(link, jersey, player, class, height, weight)
    
    return(boom)
}

# Loop through __ and __ and pull name, 
# No 1986-1987
# No 1988-1989
# No 1994-1995
players <- NULL
for (i in 1:32) {
    
    temp <- rosterScraper(rosters[i, 3])
    
    players <- rbind(players, temp)
}

players <- tbl_df(players)

# Remove rows that contain "\n" which are the superfluous column headings and 
# duplicate players 
players <- players %>%
    filter(!grepl("\n", player)) %>%
    mutate(season = as.numeric(substr(link, 46, 49)) + 1)

# Web addresses for 2010 - 2016 seasons 
start.year <- 2010:2015
end.year <- start.year + 1

rostersb <- data_frame(start.year, end.year)

rostersb <- rostersb %>%
    mutate(start.year = as.character(start.year)) %>%
    mutate(end.year = as.character(end.year)) %>%
    mutate(key = substr(end.year, 3, 4)) %>%
    mutate(key = paste0("-", key)) %>%
    mutate(key = paste0(start.year, key)) %>%
    mutate(key = paste0(key, "/roster")) %>%
    mutate(key = paste0("http://vcuathletics.com/sports/mbkb/", key))

# Pulls a single year's roster from the VCU Athletic's website
rosterScraperb <- function (link) {
    temp <- read_html(as.character(link))
    
    link <- as.character(link)
    
    jersey <- temp %>% 
        html_nodes(".number") %>%
        html_text() 
    
    player <- temp %>% 
        html_nodes(".name") %>%
        html_text() 
    
    class <- temp %>% 
        html_nodes("tbody td:nth-child(5)") %>%
        html_text()  
    
    height <- temp %>% 
        html_nodes("tbody td:nth-child(6)") %>%
        html_text()      
    
    weight <- temp %>% 
        html_nodes("tbody td:nth-child(7)") %>%
        html_text()  
    
    boom <- data_frame(link, jersey, player, class, height, weight)
    
    return(boom)
}

# TODO(awunderground): scrape positions and impute them for the older players based
# on height, weight, and stats

playersb <- rbind(rosterScraperb(rostersb[1, 3]), 
rosterScraperb(rostersb[2, 3]),
rosterScraperb(rostersb[3, 3]),
rosterScraperb(rostersb[4, 3]),
rosterScraperb(rostersb[5, 3]),
rosterScraperb(rostersb[6, 3]))

playersb <- playersb %>%
    mutate(season = as.numeric(substr(link, 37, 40)) + 1)

players <- bind_rows(players, playersb)

# Clean up the players data frame

boom <- players %>%
    mutate_all(str_trim) %>%
    mutate(jersey = as.numeric(jersey))



# create redshirt dummy










# Historical Roster

# year | player name 1 | player name 2 (if hyphenated the hyphenate!) 


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