library(rvest)
library(stringr)
library(tidyverse)

start.year <- 1971:2008
end.year <- start.year + 1

stats <- data_frame(start.year, end.year)

stats <- stats %>%
    mutate(start.year = as.character(start.year)) %>%
    mutate(end.year = as.character(end.year)) %>%
    mutate(link = substr(end.year, 3, 4)) %>%
    mutate(link = paste0(start.year, link)) %>%
    mutate(link = paste0(link, "_Stats")) %>%
    mutate(link = paste0("http://vcuathletics.com/sports/mbkb/archives/", link)) %>%
    mutate(key = paste0("season", end.year))

statScraperA <- function (link) {
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
    
    vector4 <- temp %>% 
        html_nodes("td:nth-child(4)") %>%
        html_text()
    
    vector5 <- temp %>% 
        html_nodes("td:nth-child(5)") %>%
        html_text()
    
    vector6 <- temp %>% 
        html_nodes("td:nth-child(6)") %>%
        html_text()
    
    vector7 <- temp %>% 
        html_nodes("td:nth-child(7)") %>%
        html_text()
    
    vector8 <- temp %>% 
        html_nodes("td:nth-child(8)") %>%
        html_text()
    
    vector9 <- temp %>% 
        html_nodes("td:nth-child(9)") %>%
        html_text()
    
    vector10 <- temp %>% 
        html_nodes("td:nth-child(10)") %>%
        html_text()

    boom <- data_frame(link, player, games.played, vector3, vector4, vector5, 
                       vector6, vector7, vector8, vector9, vector10)
    
    return(boom)
}

for (i in 1:nrow(stats)) {
    
    temp <- statScraperA(stats[i, 3])
    
    assign(paste0("season", i + 1971), temp)
    
}


td:nth-child(10)

td:nth-child(17)


