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

    boom <- data_frame(link, player, games.played)
    
    return(boom)
}

# Scrape longitudinal record of players and games played. 
key <- NULL
for (i in 1:nrow(stats)) {
    
    temp <- statScraperA(stats[i, 3])
    
    key <- rbind(key, temp)
    rm(temp)
}

key <- key %>%
    filter(player != "Name" & player != "\nName\n")

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
        vector11 <- rep("vector11", length(vector10))
    }

    vector12 <- temp %>% 
        html_nodes("td:nth-child(12)") %>%
        html_text()
    
    if (length(vector12) == 0) {
        vector12 <- rep("vector12", length(vector11))
    }
    
    vector13 <- temp %>% 
        html_nodes("td:nth-child(13)") %>%
        html_text()
    
    if (length(vector13) == 0) {
        vector13 <- rep("vector13", length(vector12))
    }
    
    vector14 <- temp %>% 
        html_nodes("td:nth-child(14)") %>%
        html_text()
    
    if (length(vector14) == 0) {
        vector14 <- rep("vector14", length(vector13))
    }
    
    vector15 <- temp %>% 
        html_nodes("td:nth-child(15)") %>%
        html_text()
    
    if (length(vector15) == 0) {
        vector15 <- rep("vector15", length(vector14))
    }
    
    vector16 <- temp %>% 
        html_nodes("td:nth-child(16)") %>%
        html_text()
    
    if (length(vector16) == 0) {
        vector16 <- rep("vector16", length(vector15))
    }
    
    vector17 <- temp %>% 
        html_nodes("td:nth-child(17)") %>%
        html_text()
    
    if (length(vector17) == 0) {
        vector17 <- rep("vector17", length(vector16))
    }
    
    boom <- data_frame(link, player, games.played, vector3, vector4, vector5, 
                       vector6, vector7, vector8, vector9, vector10, vector11,
                       vector12, vector13, vector14, vector15, vector16, 
                       vector17)
    
    return(boom)
}

for (i in 1:nrow(stats)) {
    
    temp <- statScraperB(stats[i, 3])
    
    assign(paste0("season", i + 1971), temp)
    
}

seasons <- list(season1972, season1973, season1974, season1975, season1976, 
                season1977, season1978, season1979, season1980, season1981,
                season1982, season1983, season1984, season1985, season1986,
                season1987, season1988, season1989, season1990, season1991,
                season1992, season1993, season1994, season1995, season1996,
                season1997, season1998, season1999, season2000, season2001,
                season2002, season2003, season2004, season2005, season2006,
                season2007, season2008, season2009)

rm(season1972, season1973, season1974, season1975, season1976, 
   season1977, season1978, season1979, season1980, season1981,
   season1982, season1983, season1984, season1985, season1986,
   season1987, season1988, season1989, season1990, season1991,
   season1992, season1993, season1994, season1995, season1996,
   season1997, season1998, season1999, season2000, season2001,
   season2002, season2003, season2004, season2005, season2006,
   season2007, season2008, season2009)

namer <- function(df) {

    df[1, ] <- trimws(df[1, ])
    df[1, ] <- tolower(df[1, ])
    df[1, ] <- gsub("\\.", "", df[1, ])
    df[1, ] <- gsub("-", ".", df[1, ])
    df[1, ] <- gsub("min", "minutes", df[1, ])
    df[1, ] <- gsub("minutes.avg", "minutes", df[1, ])
    df[1, ] <- gsub("off.def.tot", "reb", df[1, ])
    
    names(df) <- df[1, ]
    df <- df[2:nrow(df), ]
    
    return(df)
}

seasons <- lapply(seasons, namer)


minutes <- data_frame()
for (i in 1:nrow(stats)) {
    if (sum(grepl("minutes", names(seasons[[i]]))) == 0) {
        temp <- data_frame(minutes = rep(NA, nrow(seasons[[i]])))
    } else {
        temp <- seasons[[i]][, "minutes"]
    }
    minutes <- rbind(minutes, temp)
}

field.goals <- data_frame()
for (i in 1:nrow(stats)) {
    if (sum(grepl("fg.fga", names(seasons[[i]]))) == 0) {
        temp <- data_frame(fg.fga = rep(NA, nrow(seasons[[i]])))
    } else {
        temp <- seasons[[i]][, "fg.fga"]
    }
    field.goals <- rbind(field.goals, temp)
}

# Three pointers
three.pointers <- data_frame()
for (i in 1:nrow(stats)) {
    if (sum(grepl("3fg.att", names(seasons[[i]]))) == 0) {
        temp <- data_frame(`3fg.att` = rep(NA, nrow(seasons[[i]])))
    } else {
        temp <- seasons[[i]][, "3fg.att"]
    }
    three.pointers <- rbind(three.pointers, temp)
    rm(temp)
}

free.throws <- data_frame()
for (i in 1:nrow(stats)) {
    if (sum(grepl("ft.fta", names(seasons[[i]]))) == 0) {
        temp <- data_frame(ft.fta = rep(NA, nrow(seasons[[i]])))
    } else {
        temp <- seasons[[i]][, "ft.fta"]
    }
    free.throws <- rbind(free.throws, temp)
    rm(temp)
}

rebounds <- data_frame()
for (i in 1:nrow(stats)) {
    if (sum(grepl("reb", names(seasons[[i]]))) == 0) {
        temp <- data_frame(reb = rep(NA, nrow(seasons[[i]])))
    } else  {
        temp <- seasons[[i]][, "reb"]
    } 
    rebounds <- rbind(rebounds, temp)
    rm(temp)
}

fouls <- data_frame()
for (i in 1:nrow(stats)) {
    if (sum(grepl("pf.fo", names(seasons[[i]]))) == 0) {
        temp <- data_frame(pf.fo = rep(NA, nrow(seasons[[i]])))
    } else {
        temp <- seasons[[i]][, "pf.fo"]
    } 
    fouls <- rbind(fouls, temp)
}

assists <- data_frame()
for (i in 1:nrow(stats)) {
    if (sum(grepl("ast", names(seasons[[i]]))) == 0) {
        temp <- data_frame(ast = rep(NA, nrow(seasons[[i]])))
    } else {
        temp <- seasons[[i]][, "ast"]
    } 
    assists <- rbind(assists, temp)
}

turnovers <- data_frame()
for (i in 1:nrow(stats)) {
    if (sum(grepl("to$", names(seasons[[i]]))) == 0) {
        temp <- data_frame(to = rep(NA, nrow(seasons[[i]])))
    } else {
        temp <- seasons[[i]][, "to"]
    } 
    turnovers <- rbind(turnovers, temp)
}

blocks <- data_frame()
for (i in 1:nrow(stats)) {
    if (sum(grepl("blk", names(seasons[[i]]))) == 0) {
        temp <- data_frame(blk = rep(NA, nrow(seasons[[i]])))
    } else {
        temp <- seasons[[i]][, "blk"]
    } 
    blocks <- rbind(blocks, temp)
}

steals <- data_frame()
for (i in 1:nrow(stats)) {
    if (sum(grepl("stl", names(seasons[[i]]))) == 0) {
        temp <- data_frame(stl =rep(NA, nrow(seasons[[i]])))
    } else {
        temp <- seasons[[i]][, "stl"]
    } 
    steals <- rbind(steals, temp)
}

points <- data_frame()
for (i in 1:nrow(stats)) {
    if (sum(grepl("pts.avg", names(seasons[[i]]))) == 0) {
        temp <- data_frame(pts.avg = rep(NA, nrow(seasons[[i]])))
    } else {
        temp <- seasons[[i]][, "pts.avg"]
    } 
    points <- rbind(points, temp)
}

long <- bind_cols(key, minutes, field.goals, three.pointers, free.throws, 
                  rebounds, fouls, assists, turnovers, blocks, steals, points)

write.csv(long, "data/longitudinal_player_stats.csv", row.names = FALSE)