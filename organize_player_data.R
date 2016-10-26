library(rvest)
library(tidyverse)


# Rosters

# Objective 1: longitudinal roster record for VCU 1978-1979 to present 

start.year <- 1978:2014
end.year <- start.year + 1

rosters <- data_frame(start.year, end.year)

rosters <- rosters %>%
    mutate(start.year = as.character(start.year)) %>%
    mutate(end.year = as.character(end.year)) %>%
    mutate(key = substr(end.year, 3, 4)) %>%
    mutate(key = paste0(start.year, key)) %>%
    mutate(key = paste0(key, "_roster")) %>%
    mutate(key = paste0("http://vcuathletics.com/sports/mbkb/archives/", key))

rosterBuilder <- function () {
    vcu1979 <- read_html("http://vcuathletics.com/sports/mbkb/archives/197879_roster")
    
    roster2016 <- vcu2016 %>% 
        html_nodes("td:nth-child(2) , tr+ tr td:nth-child(2) div") %>%
        html_text() 
    
    }
    





vcu1979 <- read_html("http://vcuathletics.com/sports/mbkb/archives/197879_roster")

roster1979 <- vcu1979 %>% 
    html_nodes("td:nth-child(2) , tr+ tr td:nth-child(2) div") %>%
    html_text() 






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