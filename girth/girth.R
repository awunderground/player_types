library(tidyverse)
library(htmlTable)

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

# Calculate the % of players in each season with height and weight
data.summary <- players %>%
    group_by(season) %>%
    mutate(dummy = ifelse(!is.na(height) & !is.na(weight), 1, 0)) %>%
    summarize(values = mean(dummy))

# Data
ggplot(data.summary, aes(season, values)) +
    geom_bar(stat = "identity", fill = "gold1") +
    labs(title = "Recent Rosters Are More Complete",
         subtitle = "Proportion of Players With Height and Weight by Season",
         caption = "424 Player Years from 1972-Present") +
    ylab("Proportion of Values") +
    xlab("Season")
ggsave("girth/complete_values.png", height = 6, width = 9, units = "in")

# Height
players <- players %>%
    filter(!is.na(height) & !is.na(weight)) %>%
    mutate(pounds.per.inch = weight / height.inches) %>%
    mutate(height.rank = rank(-height.inches, ties = "min")) %>%
    mutate(weight.rank = rank(-weight, ties = "min")) %>%
    mutate(girth.rank = rank(-pounds.per.inch, ties = "min"))

# Height histogram
ggplot(players, aes(height.inches, fill = ifelse(height.inches == 79, "gold1", "grey4"))) +
    geom_histogram(binwidth = 1) +
    theme(legend.position="none") +
    ylab("Count") +
    xlab("Height (Inches)") +
    labs(title = "Mo Alie-Cox is a Normal Height",
         subtitle = "Histogram of VCU Men's Basketball Player Heights",
         caption = "424 Player Years from 1972-Present")
ggsave("girth/height_histogram.png", height = 6, width = 9, units = "in")

# Weight histogram
ggplot(players, aes(weight, fill = ifelse(weight == 250, "gold", "grey4"))) +
    geom_histogram(binwidth = 5) +
    theme(legend.position="none") +
    ylab("Count") +
    xlab("Weight (Pounds)") +
    labs(title = "Mo Alie-Cox is an Abnormal Weight",
         subtitle = "Histogram of VCU Men's Basketball Player Weights",
         caption = "424 Player Years from 1972-Present")
ggsave("girth/weight_histogram.png", height = 6, width = 9, units = "in")

# height-weight scatter plot
ggplot(players, aes(height.inches, weight)) +
    geom_jitter(size = 4, alpha = 0.5, color = "gold1") +
    geom_text(aes(label = ifelse(season == 2017, as.character(last.name), "")), size = 3) +
    ylab("Weight (Pounds)") +
    xlab("Height (Inches)") +
    labs(title = "Mo Alie-Cox is a Big Dude",
         subtitle = "Heights and Weights of VCU Men's Basketball Players",
         caption = "424 Player Years from 1972-Present")
ggsave("girth/weight_height_scatterplot.png", height = 6, width = 9, units = "in")

# html table of top players
mytable1 <- players %>%
    filter(girth.rank <= 10) %>%
    arrange(girth.rank) %>%
    mutate(Name = paste(first.name.x, last.name, sep = " ")) %>%
    mutate(pounds.per.inch = round(pounds.per.inch, 2)) %>%
    select(girth.rank, Season = season, Name, Weight = weight, Height = height, PoundsPerInch = pounds.per.inch)

mytableout <- htmlTable(mytable1, rnames = FALSE)
print(mytableout, type="html", useViewer=TRUE)

# html table of bottom players
mytable2 <- players %>%
    filter(girth.rank >= 415) %>%
    arrange(-girth.rank) %>%
    mutate(pounds.per.inch = round(pounds.per.inch, 2)) %>%
    select(girth.rank, Season = season, last.name, first.name.x, Weight = weight, Height = height)

mytableout <- htmlTable(mytable2, rnames = FALSE)
print(mytableout, type="html", useViewer=TRUE)
