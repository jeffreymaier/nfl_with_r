library(nflfastR)
library(dplyr)

league_name = "winemixer"
# bring in player data
last_3_years <- load_player_stats(2019:2022)
last_3_years$ppp <- (last_3_years$passing_yards / 25) + (last_3_years$completions * .5) + ((last_3_years$attempts - last_3_years$completions) * -.5) + (last_3_years$passing_tds * 5) + (last_3_years$interceptions * -2) + (last_3_years$sacks * -1) + ((last_3_years$sack_fumbles_lost + last_3_years$rushing_fumbles_lost + last_3_years$receiving_fumbles_lost) * -2) + (last_3_years$receptions * 1) + (last_3_years$receiving_yards / 10) + (last_3_years$receiving_tds * 6) + (last_3_years$rushing_yards / 10) + (last_3_years$rushing_tds * 6) + ((last_3_years$passing_2pt_conversions + last_3_years$receiving_2pt_conversions + last_3_years$rushing_2pt_conversions) * 2)
player_stats <- last_3_years %>% filter(season_type == "REG") %>% select(player_id, season, ppp) %>% group_by(player_id, season) %>% summarise(total_fp = sum(ppp), ave_fp = mean(ppp), sd_fp = sd(ppp), games = n())  
colnames(player_stats)[1] <- "gsis_id"
# roster data so we can add positions
last_rosters <- fast_scraper_roster(2019:2022)

# merging the roster and player data
merged_roster <- merge(last_rosters, player_stats, by=c("gsis_id", "season"))
# getting our sortable data
fp_position <- merged_roster %>% select(season, position, total_fp)

# qbs
qb2018 <- fp_position %>% filter(position == "QB" & season == 2019) %>% select(total_fp) %>% arrange(-total_fp)
qb2019 <- fp_position %>% filter(position == "QB" & season == 2020) %>% select(total_fp) %>% arrange(-total_fp)
qb2020 <- fp_position %>% filter(position == "QB" & season == 2021) %>% select(total_fp) %>% arrange(-total_fp)
qb2021 <- fp_position %>% filter(position == "QB" & season == 2022) %>% select(total_fp) %>% arrange(-total_fp)

qb2018 <- unlist(qb2018, use.names = FALSE)[1:40]
qb2019 <- unlist(qb2019, use.names = FALSE)[1:40]
qb2020 <- unlist(qb2020, use.names = FALSE)[1:40]
qb2021 <- unlist(qb2021, use.names = FALSE)[1:40]

qb_sorted <- data.frame(y2018 = qb2018, y2019 = qb2019, y2020 = qb2020, y2021 = qb2021)
qb_sorted$fp_ave <- (qb_sorted$y2018 + qb_sorted$y2019 + qb_sorted$y2020 + qb_sorted$y2021) / 4
qb_sorted$position <- "QB"

# rbs
rb2018 <- fp_position %>% filter(position == "RB" & season == 2019) %>% select(total_fp) %>% arrange(-total_fp)
rb2019 <- fp_position %>% filter(position == "RB" & season == 2020) %>% select(total_fp) %>% arrange(-total_fp)
rb2020 <- fp_position %>% filter(position == "RB" & season == 2021) %>% select(total_fp) %>% arrange(-total_fp)
rb2021 <- fp_position %>% filter(position == "RB" & season == 2022) %>% select(total_fp) %>% arrange(-total_fp)

rb2018 <- unlist(rb2018, use.names = FALSE)[1:150]
rb2019 <- unlist(rb2019, use.names = FALSE)[1:150]
rb2020 <- unlist(rb2020, use.names = FALSE)[1:150]
rb2021 <- unlist(rb2021, use.names = FALSE)[1:150]

rb_sorted <- data.frame(y2018 = rb2018, y2019 = rb2019, y2020 = rb2020, y2021 = rb2021)
rb_sorted$fp_ave <- (rb_sorted$y2018 + rb_sorted$y2019 + rb_sorted$y2020 + rb_sorted$y2021) / 4
rb_sorted$position <- "RB"

# wrs
wr2018 <- fp_position %>% filter(position == "WR" & season == 2019) %>% select(total_fp) %>% arrange(-total_fp)
wr2019 <- fp_position %>% filter(position == "WR" & season == 2020) %>% select(total_fp) %>% arrange(-total_fp)
wr2020 <- fp_position %>% filter(position == "WR" & season == 2021) %>% select(total_fp) %>% arrange(-total_fp)
wr2021 <- fp_position %>% filter(position == "WR" & season == 2022) %>% select(total_fp) %>% arrange(-total_fp)

wr2018 <- unlist(wr2018, use.names = FALSE)[1:150]
wr2019 <- unlist(wr2019, use.names = FALSE)[1:150]
wr2020 <- unlist(wr2020, use.names = FALSE)[1:150]
wr2021 <- unlist(wr2021, use.names = FALSE)[1:150]

wr_sorted <- data.frame(y2018 = wr2018, y2019 = wr2019, y2020 = wr2020, y2021 = wr2021)
wr_sorted$fp_ave <- (wr_sorted$y2018 + wr_sorted$y2019 + wr_sorted$y2020 + wr_sorted$y2021) / 4
wr_sorted$position <- "WR"

# te
te2018 <- fp_position %>% filter(position == "TE" & season == 2019) %>% select(total_fp) %>% arrange(-total_fp)
te2019 <- fp_position %>% filter(position == "TE" & season == 2020) %>% select(total_fp) %>% arrange(-total_fp)
te2020 <- fp_position %>% filter(position == "TE" & season == 2021) %>% select(total_fp) %>% arrange(-total_fp)
te2021 <- fp_position %>% filter(position == "TE" & season == 2022) %>% select(total_fp) %>% arrange(-total_fp)

te2018 <- unlist(te2018, use.names = FALSE)[1:30]
te2019 <- unlist(te2019, use.names = FALSE)[1:30]
te2020 <- unlist(te2020, use.names = FALSE)[1:30]
te2021 <- unlist(te2021, use.names = FALSE)[1:30]

te_sorted <- data.frame(y2018 = te2018, y2019 = te2019, y2020 = te2020, y2021 = te2021)
te_sorted$fp_ave <- (te_sorted$y2018 + te_sorted$y2019 + te_sorted$y2020 + te_sorted$y2021) / 4
te_sorted$position <- "TE"

write.csv(qb_sorted, "/Users/jeffreymaier/r/winemixer/qb.csv")
write.csv(rb_sorted, "/Users/jeffreymaier/r/winemixer/rb.csv")
write.csv(wr_sorted, "/Users/jeffreymaier/r/winemixer/wr.csv")
write.csv(te_sorted, "/Users/jeffreymaier/r/winemixer/te.csv")

last_3_years %>% filter(passing_yards > 300) %>%  select(player_name, passing_yards, passing_tds, sacks, interceptions, ppp) %>% arrange(-ppp)


