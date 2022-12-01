# nfl_with_r

## The goal of this project is do some basic data analysis using R on NFL data

I am using nflfastR to gather the nfl data.

### Expected Fantasy Points

This program is written to gather a single year or multiple year's worth of play by play data.

Here is what it then does:

    1. Adds a fantasy points column
    2. Creates two tables. 1 for rushing and 1 for receiving expected fantasy points. 
    3. Iterate over each row, check if it's a rush or pass attempt and if it is add in expected points.
    4. Group by Player then total fantasy points, expected points, add in a column for fantasy points over expected and sort by total fantasy points.
    5. Writes to csv as "players_grouped.csv" in the current folder.

### Uses

This is descriptive stat and not predictive. So it's best use is likely in the offseason when multiple players may be ranked similarly you may use the fantasy points over expected to differentiate between them. I could also see this used in season to evaluate players that may seem 'hot' or 'cold'. If someone is being used and expected to score 15-20 points a game but they are actually scoring 10 it might be worth to stick with them and on the other side if someone is expected to score 10 but actually scoring 20 it may be beneficial to sell them off. 