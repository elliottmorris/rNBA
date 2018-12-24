# rNBA

This is a repo to house code for parsing NBA data, inclduing computing time-series ELO scores, box scores, team contribution, etc. I may one day use this data to make a Bayesian forecasting model for NBA games.... but for now, it's more of a learning tool than anything. 

Data in the `data/` directory:

* Scores for every game played in the `/data/schedules` directory, which is organized by season.
* Player box scores and play-by-play data for every game ever, in the `data/game data` directory, organized by game. **This data is stored in the `.Rdata` format, due to having lists of data frames for each game in each season.

 
