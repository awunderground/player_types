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
rosterScraperA <- function (link) {
    temp <- read_html(as.character(link))
    
    link <- as.character(link)
    
    jersey <- temp %>% 
        html_nodes("td:nth-child(1) , tr+ tr td:nth-child(1) div") %>%
        html_text() 
    
    if (link == "http://vcuathletics.com/sports/mbkb/archives/198182_roster" |
        link == "http://vcuathletics.com/sports/mbkb/archives/198384_roster" |
        link == "http://vcuathletics.com/sports/mbkb/archives/199091_roster" |
        link == "http://vcuathletics.com/sports/mbkb/archives/199192_roster" |
        link == "http://vcuathletics.com/sports/mbkb/archives/199596_roster") {
        short.vector <- temp %>% 
            html_nodes("td:nth-child(1) , tr+ tr td:nth-child(1) div") %>%
            html_text()  
        
        jersey <- c(short.vector[1], NA, short.vector[2:length(short.vector)])
    }
    
    player <- temp %>% 
        html_nodes("td:nth-child(2) , tr+ tr td:nth-child(2) div") %>%
        html_text() 
   
    if (link == "http://vcuathletics.com/sports/mbkb/archives/198182_roster") {
        short.vector <- temp %>% 
            html_nodes("td:nth-child(2) , tr+ tr td:nth-child(2) div") %>%
            html_text()  
        
        player <- c(short.vector[1], NA, short.vector[2:length(short.vector)])
    }
    
    class <- temp %>% 
        html_nodes("td:nth-child(3) , tr+ tr td:nth-child(3) div") %>%
        html_text()  
        
    height <- temp %>% 
        html_nodes("td:nth-child(4) , tr+ tr td:nth-child(4) div") %>%
        html_text()      
    
    if (link == "http://vcuathletics.com/sports/mbkb/archives/199293_roster") {
        short.vector <- temp %>% 
            html_nodes("tr+ tr td:nth-child(4)") %>%
            html_text()  
        
        height <- c(NA, NA, short.vector)
    }
    
    weight <- temp %>% 
        html_nodes("td:nth-child(5) , tr+ tr td:nth-child(5) div") %>%
        html_text()  
    
    hometown <- temp %>% 
        html_nodes("td:nth-child(6) , tr+ tr td:nth-child(6) div") %>%
        html_text()  
    
    boom <- cbind(link, jersey, player, class, height, weight, hometown)
    
    return(boom)
}

# Loop through __ and __ and pull name, 
# No 1986-1987 9
# No 1988-1989 11
# No 1994-1995 17
playersa <- NULL
for (i in c(1:8, 10, 12:16, 18:32)) {
    
    temp <- rosterScraperA(rosters[i, 3])
    
    playersa <- rbind(playersa, temp)
}

playersa <- tbl_df(playersa)

playersa <- playersa %>% 
    mutate(season = as.numeric(substr(link, 46, 49)) + 1) %>%
    filter(season != 1994)

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
rosterScraperB <- function (link) {
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
    
    hometown <- temp %>% 
        html_nodes("tbody td:nth-child(8)") %>%
        html_text()
    
    boom <- data_frame(link, jersey, player, class, height, weight, hometown)
    
    return(boom)
}

# TODO(awunderground): scrape positions and impute them for the older players based
# on height, weight, and stats

playersb <- rbind(rosterScraperB(rostersb[1, 3]), 
rosterScraperB(rostersb[2, 3]),
rosterScraperB(rostersb[3, 3]),
rosterScraperB(rostersb[4, 3]),
rosterScraperB(rostersb[5, 3]),
rosterScraperB(rostersb[6, 3]))

playersb <- playersb %>%
    mutate_all(funs(gsub), pattern = "\t", replacement = "") %>%
    mutate_all(funs(gsub), pattern = "\n", replacement = "") %>%
    mutate_all(funs(str_trim)) %>%
    mutate(season = as.numeric(substr(link, 37, 40)) + 1) 

# Fix 1994 
rosterScraper94 <- function (link) {
    temp <- read_html(as.character(link))
    
    link <- as.character(link)
    
    jersey <- temp %>% 
        html_nodes("td td:nth-child(1) , tr+ tr td:nth-child(1) div") %>%
        html_text() 
    
    player <- temp %>% 
        html_nodes("td:nth-child(3) , tr+ tr td:nth-child(3) div") %>%
        html_text() 
    
    class <- temp %>% 
        html_nodes("td:nth-child(4) , tr+ tr td:nth-child(4) div") %>%
        html_text()  
    
    height <- temp %>% 
        html_nodes("td:nth-child(5) , tr+ tr td:nth-child(5) div") %>%
        html_text()      
    
    weight <- temp %>% 
        html_nodes("td:nth-child(6) , tr+ tr td:nth-child(6) div") %>%
        html_text()  
    
    hometown <- temp %>% 
        html_nodes("td:nth-child(7) , tr+ tr td:nth-child(7) div") %>%
        html_text() 
    
    boom <- data_frame(link, jersey, player, class, height, weight, hometown)
    
    return(boom)
}

vcu1994 <- rosterScraper94(rosters[16, 3])

vcu1994 <- vcu1994 %>% 
    mutate(season = as.numeric(substr(link, 46, 49)) + 1)

players <- bind_rows(playersa, playersb, vcu1994)

##
## Clean up the players data frame
##

# Remove rows that contain "\n" which are the superfluous column headings and 
# duplicate players 
boom <- players %>%
    filter(!grepl("\n", player)) %>%
    mutate_all(str_trim) %>%
    mutate(jersey = as.numeric(jersey)) %>%
    mutate(redshirt = ifelse(grepl("R", class), 1, 0)) %>% # Redshirt Flag
    mutate(class.c = "") %>%
    mutate(class.c = ifelse(grepl("Fr", class), "Freshman", class.c)) %>%
    mutate(class.c = ifelse(grepl("So", class), "Sophomore", class.c)) %>%
    mutate(class.c = ifelse(grepl("Jr", class), "Junior", class.c)) %>%
    mutate(class.c = ifelse(grepl("Junior", class), "Junior", class.c)) %>%
    mutate(class.c = ifelse(grepl("sr", class), "Senior", class.c)) %>%
    mutate(class.c = ifelse(grepl("Sr", class), "Senior", class.c)) %>%
    mutate(class.c = ifelse(grepl("Senior", class), "Senior", class.c)) %>%
    filter(!(player == "Joey Rodriguez" & season < 2008)) %>%
    mutate(feet = as.numeric(substr(height, 1, 1))) %>%
    mutate(inches = as.numeric(substr(height, 3, 4))) %>%
    mutate(height.inches = feet * 12 + inches) %>%
    mutate(hometown = gsub("\n", " ", hometown))




strsplit(players$hometown, "/")



longitudinal.roster <- players %>%
    mutate(class = class.c) %>%
    select(season, jersey, player, class, height, height.inches, weight, redshirt)

write.csv(longitudinal.roster, "data/longitudinal_roster.csv", row.names = FALSE)

# Add position and then impute it for other players
# impute it again after adding statistics
# fix players a error

# Add hometown

