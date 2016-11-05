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
    df[1, ] <- gsub("to", "turnovers", df[1, ])

    names(df) <- df[1, ]
    df <- df[2:nrow(df), ]
    
    return(df)
}

seasons <- lapply(seasons, namer)

field.goals <- data_frame()
for (i in 1:nrow(stats)) {
    
    if (sum(grepl("fg.fga", names(seasons[[i]]))) == 0) {
        
        temp <- rep(NA, nrow(seasons[[i]]))
        
    } else {
        
        temp <- seasons[[i]][, "fg.fga"]
        
    }
    
    field.goals <- rbind(field.goals, temp)
    
    rm(temp)
}


free.throws <- data_frame()
for (i in 1:nrow(stats)) {
    
    if (sum(grepl("ft.fta", names(seasons[[i]]))) == 0) {
        
        temp <- rep(NA, nrow(seasons[[i]]))
        
    } else {
        
        temp <- seasons[[i]][, "ft.fta"]
        
    }
    
    free.throws <- rbind(free.throws, temp)
    
    rm(temp)
}


minutes <- data_frame()
field.goals <- data_frame()
three.pointers <- data_frame()
free.throws <- data_frame()
rebounds <- data_frame()
fouls <- data_frame()
assists <- data_frame()
turnovers <- data_frame()
blocks <- data_frame()
steals <- data_frame()
points <- data_frame()

for (i in 1:nrow(stats)) {
    
    if (sum(grepl("min", names(seasons[[i]]))) == 0) {
        temp1 <- rep(NA, nrow(seasons[[i]]))
    } else {
        temp1 <- seasons[[i]][, "min"]
    }
    
    if (sum(grepl("fg.fga", names(seasons[[i]]))) == 0) {
        temp2 <- rep(NA, nrow(seasons[[i]]))
    } else {
        temp2 <- seasons[[i]][, "fg.fga"]
    }
    
    if (sum(grepl("3fg.att", names(seasons[[i]]))) == 0) {
        temp3 <- rep(NA, nrow(seasons[[i]]))
    } else {
        temp3 <- seasons[[i]][, "3fg.att"]
    }
    
    if (sum(grepl("ft.fta", names(seasons[[i]]))) == 0) {
        temp4 <- rep(NA, nrow(seasons[[i]]))
    } else {
        temp4 <- seasons[[i]][, "ft.fta"]
    }
    
    if (sum(grepl("off.def.tot", names(seasons[[i]]))) == 0) {
        temp5 <- rep(NA, nrow(seasons[[i]]))
    } else {
        temp5 <- seasons[[i]][, "off.def.tot"]
    }  
    
    if (sum(grepl("pf.fo", names(seasons[[i]]))) == 0) {
        temp6 <- rep(NA, nrow(seasons[[i]]))
    } else {
        temp6 <- seasons[[i]][, "pf.fo"]
    } 
    
    if (sum(grepl("ast", names(seasons[[i]]))) == 0) {
        temp7 <- rep(NA, nrow(seasons[[i]]))
    } else {
        temp7 <- seasons[[i]][, "ast"]
    } 
    
    if (sum(grepl("turnovers", names(seasons[[i]]))) == 0) {
        temp8 <- rep(NA, nrow(seasons[[i]]))
    } else {
        temp8 <- seasons[[i]][, "turnovers"]
    } 

    if (sum(grepl("blk", names(seasons[[i]]))) == 0) {
        temp9 <- rep(NA, nrow(seasons[[i]]))
    } else {
        temp9 <- seasons[[i]][, "blk"]
    } 
    
    if (sum(grepl("stl", names(seasons[[i]]))) == 0) {
        temp10 <- rep(NA, nrow(seasons[[i]]))
    } else {
        temp10 <- seasons[[i]][, "stl"]
    } 
    
    if (sum(grepl("pts.avg", names(seasons[[i]]))) == 0) {
        temp11 <- rep(NA, nrow(seasons[[i]]))
    } else {
        temp11 <- seasons[[i]][, "pts.avg"]
    } 

    minutes <- rbind(field.goals, temp1)
    field.goals <- rbind(field.goals, temp2)
    three.pointers <- data_frame(three.pointers, temp3)
    free.throws <- rbind(free.throws, temp4)
    rebounds <- rbind(rebounds, temp5)
    fouls <- rbind(fouls, temp6)
    assists <- rbind(assists, temp7)
    turnovers <- rbind(turnovers, temp8)
    blocks <- rbind(blocks, temp9)
    steals <- rbind(steals, temp10)
    points <- rbind(points, temp11)
    
    rm(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10, temp11)
}

# min
# fg.fga
# pct
# 3fg.att
# pct
# ft.fta
# pct
# off.def.tot
# pf.fo
# ast
# to
# blk
# stl
# pts.avg











hope <- if (sum(grepl("fg.fga", names(seasons[[1]]))) == 0) {
    
    rep(NA, nrow(seasons[[1]]))
    
} else {
    
    seasons[[1]][, "fg.fga"]
    
}















temp1 <- seasons[[1]]
temp2 <- seasons[[2]]

for (i in 1:2) {
    
    print(seasons[[i]][, "fg.fga"])
    
}













temp1 <- namer(season1972)
season1973 <- namer(season1973)

season1972[, "name"]
season1973[, "name"]


# TODO(awunderground): create long df with links, names, and games played. Loop
# through dataframes in list and merge different stats?

# player - good 
# games.played - good 
# minutes
# field.goals
# three.pointers
# free.throws
# rebounds
# assists
# turnovers
# blocks
# steals
# points


short <- read_html("http://vcuathletics.com/sports/mbkb/archives/197475_Stats")

short.out <- short %>% 
    html_nodes("td:nth-child(11)") %>%
    html_text()

ifelse(length(short.out) < 0, "Yes", "No")

