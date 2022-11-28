# The point of this is to use NFL play by play data and generate expected fantasy point values based on run or pass and judge players on how they did.

# import our libraries
library(nflfastR)
library(dplyr)
# if we want to plot something
library(ggplot2)

# let's grab the seasons we want to build out our data from.
# for this i am going to use the last 6 seasons. 2017 - 2022
seasons <- load_pbp(2017:2022)

# now we need to add a fantasy point value to each play
seasons <- seasons %>% mutate (
    rush_yards_fp = ifelse(rush_attempt == 1 & !is.na(epa) & touchdown == 0, rushing_yards * .1, 0),
    rush_td_fp = ifelse(posteam == td_team & touchdown == 1 & rush_attempt == 1 & !is.na(epa), (rushing_yards * .1) + 6, 0),
    rush_fp = rush_yards_fp + rush_td_fp,
    receiving_yards_fp = ifelse(pass_attempt == 1 & !is.na(epa) & touchdown == 0, complete_pass + (receiving_yards * .1), 0),
    receiving_td_fp = ifelse(posteam == td_team & touchdown == 1 & pass_attempt == 1 & !is.na(epq), complete_pass + (receiving_yards * .1) + 6, 0),
    receiving_fp = receiving_yards_fp + receiving_td_fp,
)

