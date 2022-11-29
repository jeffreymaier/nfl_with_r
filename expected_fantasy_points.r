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
    receiving_td_fp = ifelse(posteam == td_team & touchdown == 1 & pass_attempt == 1 & !is.na(epa), complete_pass + (receiving_yards * .1) + 6, 0),
    receiving_fp = receiving_yards_fp + receiving_td_fp,
)

# Let's create our rushing and receiving expected points by yardline. This outputs with 3 columns [yardline, ave fantasy points, number of plays]
rush_fp_yardline <- seasons %>% filter(rush_attempt == 1 & !is.na(epa) & qb_kneel == 0 & qb_spike == 0) %>% group_by(yardline_100) %>% summarise(rush_fp_avg = mean(rush_fp, na.rm = T), plays = n())
receiving_fp_yardline <- season %>% filter(pass_attempt == 1 & !is.na(epa) & qb_kneel == 0 & qb_spike == 0 & sack == 0 & !is.na(fantasy_player_id)) %>% group_by(yardline_100) %>% summarise(rec_fp_avg = mean(receiving_fp, na.rm = T), plays = n())

# Let's separate rushing plays and receiving plays to make this easier to go over 
rushing_plays <- seasons %>% filter(rush_attempt == 1 & !is.na(epa) & qb_kneel == 0 & qb_spike == 0)
receiving_plays <- seasons %>% filter(pass_attempt == 1 & !is.na(epa) & qb_kneel == 0 & qb_spike == 0 & sack == 0 & !is.na(fantasy_player_id)) 




print("done")