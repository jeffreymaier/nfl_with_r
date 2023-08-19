library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggthemes) # add-on themes and color schemes for plotting
library(wesanderson)

# we need to ask a few questions to get the right data
teams_in_league <- 12
players_on_roster <- 16
total_players_in_league <- teams_in_league * players_on_roster
auction_budget <- 200
total_money_in_league <- teams_in_league * auction_budget

qb_starters <- 1 * teams_in_league
qb_roster <- 1.5 * teams_in_league

rb_starters <- 2.5 * teams_in_league
rb_roster <- 5.5 * teams_in_league

wr_starters <- 3.5 * teams_in_league
wr_roster <- 7.5* teams_in_league

te_starters <- 1 * teams_in_league
te_roster <- 1.5 * teams_in_league

# the qb/rb/wr/te files come from nfl_position_averages file. they need to be loaded in.
qb_aves <- qb
rb_ave <- rb
wr_ave <- wr
te_ave <- te

# qb_aves <- qb_aves %>% mutate(roster = ifelse(qb_aves$fp_ave - qb_aves[25, 6] < 0, 0, qb_aves$fp_ave - qb_aves[25, 6]))

total_value <- function(df, roster, starters){
  subs <- as.integer((roster - starters) / 2 + starters)
  elites <- as.integer(starters / 2)
  df$roster <- unlist(df %>% select(fp_ave) %>% group_by(fp_ave) %>%  summarise(roster_points = fp_ave - df[roster, 6]) %>% select(roster_points) %>% arrange(-roster_points), use.names = FALSE)
  df$sub <- unlist(df %>% select(fp_ave) %>% group_by(fp_ave) %>%  summarise(sub_points = fp_ave - df[subs, 6]) %>% select(sub_points) %>% arrange(-sub_points), use.names = FALSE)
  df$start <- unlist(df %>% select(fp_ave) %>% group_by(fp_ave) %>%  summarise(start_points = fp_ave - df[starters, 6]) %>% select(start_points) %>% arrange(-start_points), use.names = FALSE)
  df$elite <- unlist(df %>% select(fp_ave) %>% group_by(fp_ave) %>%  summarise(elite_points = fp_ave - df[elites, 6]) %>% select(elite_points) %>% arrange(-elite_points), use.names = FALSE)
  df$roster[df$roster < 0] <- 0
  df$sub[df$sub < 0] <- 0
  df$start[df$start < 0] <- 0
  df$elite[df$elite < 0] <- 0
  df$tv <- df$roster + df$sub + df$start + df$elite
  df$pos_rk <- rep(1:length(df$tv))
  return(df)
}



rb_value <- total_value(rb_ave, rb_roster, rb_starters)
qb_value <- total_value(qb_aves, qb_roster, qb_starters)
wr_value <- total_value(wr_ave, wr_roster, wr_starters)
te_value <- total_value(te_ave, te_roster, te_starters)
total_test <- rbind(rb_value, qb_value, wr_value, te_value)

total_v <- total_test %>% select(position, pos_rk, tv) %>% arrange(-tv)
total_v$overall <- rep(1:length(total_v$tv))
layer1 <- ggplot(data = total_v, aes(x=overall, y=tv, label=position))
layer2 <- layer1 + geom_point()
layer3 <- layer2 + facet_wrap(~position)
layer2 + aes(color=position) + geom_text(hjust=0, vjust=0)

value_over <- total_v %>% filter(tv > 250)
l1 <- ggplot(data = value_over, aes(x=overall, y=tv, label=position))
l2 <- l1 + geom_point()
l2 + aes(color=position) + geom_text(hjust=0, vjust=0)

# this is the adp file you want to use. load it in as a csv
adp <- mixer
length(adp$Player)

qb_adp <- adp %>% filter(Position == "QB")
qb_adp$pos_rk <- rep(1:length(qb_adp$Player))
qbs <- merge(qb_adp, qb_value, by="pos_rk")
colnames(qbs)[colnames(qbs) == "...6"] <- 'notes'

rb_adp <- adp %>% filter(Position == "RB")
rb_adp$pos_rk <- rep(1:length(rb_adp$Player))
rbs <- merge(rb_adp, rb_value, by="pos_rk")
colnames(rbs)[colnames(rbs) == "...6"] <- 'notes'

wr_adp <- adp %>% filter(Position == "WR")
wr_adp$pos_rk <- rep(1:length(wr_adp$Player))
wrs <- merge(wr_adp, wr_value, by="pos_rk")
colnames(wrs)[colnames(wrs) == "...6"] <- 'notes'

te_adp <- adp %>% filter(Position == "TE")
te_adp$pos_rk <- rep(1:length(te_adp$Player))
tes <- merge(te_adp, te_value, by="pos_rk")
colnames(tes)[colnames(tes) == "...6"] <- 'notes'

every_adp <- rbind(qbs, rbs, wrs, tes)
all_value <- sum(every_adp$tv)
value_factor <- all_value / total_money_in_league

every_adp$dollars <- round(every_adp$tv / value_factor, 2)
names(every_adp)
every_adp <- every_adp %>% arrange(-tv)
every_adp$overall <- rep(1: length(every_adp$Player))

names(every_adp)
final_order <- every_adp %>% select(overall, Player, Team, Position, pos_rk, dollars)
colnames(final_order)[colnames(final_order) == "...9"] <- 'notes'

write.csv(final_order, "/Users/jeffreymaier/r/2023test.csv")









