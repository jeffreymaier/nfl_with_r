library(nflfastR)
library(tidyverse)

season <- nflfastR::load_player_stats(2022)

offensive_players <- season %>% filter(position_group == "QB" | position_group == "RB" | position_group == "WR" | position_group == "TE")

qb_totals <- offensive_players %>% 
  filter(position_group == "QB") %>% 
  group_by(player_id, player_display_name, position_group, recent_team) %>% 
  summarise(games = n(),
            att = sum(attempts, na.rm = T),
            comp = sum(completions, na.rm = T),
            comp_percent = comp / att,
            pass_yards = sum(passing_yards, na.rm = T),
            ypa = pass_yards / att,
            pass_td = sum(passing_tds, na.rm = T),
            td_att = pass_td / att,
            int = sum(interceptions, na.rm = T),
            int_att = int / att,
            sack = sum(sacks, na.rm = T),
            sack_yard = sum(sack_yards, na.rm = T),
            yps = sack_yard / sack,
            sack_fum = sum(sack_fumbles, na.rm = T),
            sack_fum_lost = sum(sack_fumbles_lost, na.rm = T),
            pass_air_yards = sum(passing_air_yards, na.rm = T),
            aypa = pass_air_yards / att,
            ay_conversion = pass_yards / pass_air_yards,
            pass_yac = sum(passing_yards_after_catch, na.rm = T),
            pyacpa = pass_yac / att,
            rush_att = sum(carries, na.rm = T),
            rush_yards =  sum(rushing_yards, na.rm = T),
            rush_td = sum(rushing_tds, na.rm = T),
            rush_ypa = rush_yards / rush_att,
            rush_1d = sum(rushing_first_downs, na.rm = T),
            rush_1dpa = rush_1d / rush_att,
            fp = sum(fantasy_points_ppr, na.rm = T),
            fppg = fp / games,
  )

qualifying_qb <- qb_totals %>% filter(att >= 150)
