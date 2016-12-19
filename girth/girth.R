library(tidyverse)

options(scipen = 999)

# Filer to 1998-2016
players <- read_csv("data/final_player_stats.csv") %>%
    select(season, last.name, first.name.x, height, height.inches, weight)

# 2017 
players2017 <- read_csv("season, last.name, first.name.x, height, weight
        2017, Jenkins, De'Riante, 6-5, 185
        2017, Lewis, JeQuan, 6-1, 180
        2017, Doughty, Samir, 6-4, 185
        2017, Tillman, Justin, 6-7, 220
        2017, Brooks, Doug, 6-4, 200
        2017, Williams, Jonathan, 6-1, 185
        2017, Alie-Cox, Mo, 6-7, 250
        2017, Crowfield, Malik, 6-4, 180
        2017, Burston, Torey, 5-8, 180
        2017, Fraser, Marquell, 6-5, 205
        2017, Burgess, Jordan, 6-5, 225
        2017, Hamdy-Mohamed, Ahmed, 6-9, 240")

players2017 <- players2017 %>%
    separate(height, sep = "-", into = c("feet", "inches"), remove = FALSE) %>%
    mutate(height.inches = as.numeric(feet) * 12 + as.numeric(inches)) %>%
    select(season, last.name, first.name.x, height, height.inches, weight)

players <- union(players, players2017)

# Describe data set
filter(players, !is.na(height)) # 424 of 585
filter(players, !is.na(weight)) # 424 of 585


data.summary <- players %>%
    group_by(season) %>%
    mutate(dummy = ifelse(!is.na(height) & !is.na(weight), 1, 0)) %>%
    summarize(values = mean(dummy))

# Data
ggplot(data.summary, aes(season, values)) +
    geom_bar(stat = "identity") +
    labs(title = "Proportion of Players With Height and Weight by Season",
         caption = "585 Player Years from 1972-Present") +
    ylab("Percentage of Values") +
    xlab("Season")


# Height
players <- players %>%
    filter(!is.na(height) & !is.na(weight)) %>%
    mutate(pounds.per.inch = weight / height.inches) %>%
    mutate(height.rank = rank(-height.inches, ties = "min")) %>%
    mutate(weight.rank = rank(-weight, ties = "min")) %>%
    mutate(girth.rank = rank(-pounds.per.inch, ties = "min"))

# Height histogram
ggplot(players, aes(height.inches, fill = ifelse(height.inches == 79, "gold", "grey4"))) +
    geom_histogram(binwidth = 1) +
    theme(legend.position="none") +
    ylab("Count") +
    xlab("Height (Inches)") +
    labs(title = "Histogram of Player Heights")

# Weight 
ggplot(players, aes(weight, fill = ifelse(weight == 250, "gold", "grey4"))) +
    geom_histogram(binwidth = 5) +
    theme(legend.position="none") +
    ylab("Count") +
    xlab("Weight (Pounds)") +
    labs(title = "Histogram of Player Weights")

# height weight scatter plot



