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

    vector11 <- temp %>% 
        html_nodes("td:nth-child(11)") %>%
        html_text()
    
    if (length(vector11) == 0) {
        vector11 <- rep(NA, length(vector10))
    }

    vector12 <- temp %>% 
        html_nodes("td:nth-child(12)") %>%
        html_text()
    
    if (length(vector12) == 0) {
        vector12 <- rep(NA, length(vector11))
    }
    
    vector13 <- temp %>% 
        html_nodes("td:nth-child(13)") %>%
        html_text()
    
    if (length(vector13) == 0) {
        vector13 <- rep(NA, length(vector12))
    }
    
    vector14 <- temp %>% 
        html_nodes("td:nth-child(14)") %>%
        html_text()
    
    if (length(vector14) == 0) {
        vector14 <- rep(NA, length(vector13))
    }
    
    vector15 <- temp %>% 
        html_nodes("td:nth-child(15)") %>%
        html_text()
    
    if (length(vector15) == 0) {
        vector15 <- rep(NA, length(vector14))
    }
    
    vector16 <- temp %>% 
        html_nodes("td:nth-child(16)") %>%
        html_text()
    
    if (length(vector16) == 0) {
        vector16 <- rep(NA, length(vector15))
    }
    
    vector17 <- temp %>% 
        html_nodes("td:nth-child(17)") %>%
        html_text()
    
    if (length(vector17) == 0) {
        vector17 <- rep(NA, length(vector16))
    }
    
    boom <- data_frame(link, player, games.played, vector3, vector4, vector5, 
                       vector6, vector7, vector8, vector9, vector10, vector11,
                       vector12, vector13, vector14, vector15, vector16, 
                       vector17)
    
    return(boom)
}

for (i in 1:nrow(stats)) {
    
    temp <- statScraperA(stats[i, 3])
    
    assign(paste0("season", i + 1971), temp)
    
}








td:nth-child(10)

td:nth-child(17)



short <- read_html("http://vcuathletics.com/sports/mbkb/archives/197475_Stats")

short.out <- short %>% 
    html_nodes("td:nth-child(11)") %>%
    html_text()

ifelse(length(short.out) < 0, "Yes", "No")

