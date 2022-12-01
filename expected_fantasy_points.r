# The point of this is to use NFL play by play data and generate expected fantasy point values based on run or pass and judge players on how they did.

# import our libraries
library(nflfastR)
library(dplyr)
# if we want to plot something
library(ggplot2)

# let's grab the seasons we want to build out our data from.
# for this i am going to use the last 5 seasons. 2017 - 2021
seasons <- load_pbp(2017:2021)

# now we need to add a fantasy point value to each play
seasons <- seasons %>% mutate (
    fp = (yards_gained * .1) + complete_pass + ((pass_touchdown + rush_touchdown) * 6),
)

# Let's create our rushing and receiving expected points by yardline. This outputs with 3 columns [yardline, ave fantasy points, number of plays]
rush_fp_yardline <- seasons %>% filter(rush_attempt == 1 & qb_kneel == 0) %>% group_by(yardline_100) %>% summarise(rush_fp_avg = mean(fp, na.rm = T), plays = n())
receiving_fp_yardline <- seasons %>% filter(pass_attempt == 1 & qb_spike == 0 & sack == 0) %>% group_by(yardline_100) %>% summarise(rec_fp_avg = mean(fp, na.rm = T), plays = n())


# getting ready to add in the advanced expected fantasy points to our rushing and receiving df's we add the column for it before looping over each to add
seasons$advanced_fp <- seq(1, nrow(seasons))

# iterate over each row, check if it's a rush or pass attempt and if it is add in expected points. 
for (i in 1:nrow(seasons)) {
  yardline <- seasons$yardline_100[i]
  if (is.na(seasons$rush_attempt[i]) | is.na(seasons$pass_attempt[i])) {
    next
  } else if (seasons$rush_attempt[i] == 1) {
    seasons$advanced_fp[i] <- unlist(rush_fp_yardline[yardline, 2])
  } else if (seasons$pass_attempt[i] == 1) {
    seasons$advanced_fp[i] <- unlist(receiving_fp_yardline[yardline, 2])
  }
}

# Here we group by player to see their totals. We then add fp over expected and then write the csv to whatever folder you run this from.
players_grouped <- seasons %>% group_by(fantasy_player_id, fantasy_player_name) %>% summarise(total_fp = sum(fp, na.rm = T), total_adv_fp = sum(advanced_fp, na.rm = T), plays = n()) %>% mutate(fp_over_exp = total_fp - total_adv_fp) %>% arrange(-total_fp)

write.csv(players_grouped, "players_grouped.csv")

print("done")