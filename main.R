rm(list=ls())
source("~/setup_elliott.R")


# download data -----------------------------------------------------------
## saves schedule csvs with results and .RDS for box scores, play-by-play of every game
source("scripts/scrape_gamedata.R")

# get current + historical elo --------------------------------------------
## returns object `elo_overtime`
source("scripts/calculate_team_elo.R")

# what's the final elo?
final_elo <- elo_overtime[[length(elo_overtime)]][[2]] %>%
  arrange(desc(elo))

final_elo


# what about the history of each team (not accounting for moving, name changes, etc)
elo_game_history <- lapply(1:length(elo_overtime),
                           function(x){
                             return(elo_overtime[[x]][[1]])
                           }
) %>% do.call('rbind',.)


# sort teams by final elo
elo_game_history$team <- factor(elo_game_history$team,final_elo$team)

# complete history
ggplot(elo_game_history,# %>% filter(team %in% top_elo$team),
       aes(x=ymd(date),y=elo,col=team)) +
  geom_step() +
  #geom_label_repel(data = top_elo,aes(x=ymd(date),y=elo,col=team,label=team),alpha=0.9) +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~team)


# complete history only for teams that are active now (and only for their current name)
ggplot(elo_game_history %>% filter(team %in% elo_game_history[elo_game_history$date>ymd('2018-10-01'),]$team),
       aes(x=ymd(date),y=elo,col=team)) +
  geom_step() +
  #geom_label_repel(data = top_elo,aes(x=ymd(date),y=elo,col=team,label=team),alpha=0.9) +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~team)


# just for this season
ggplot(elo_game_history %>% filter(date>ymd("2018-09-01")),
       aes(x=ymd(date),y=elo,col=team)) +
  geom_step() +
  #geom_label_repel(data = top_elo,aes(x=ymd(date),y=elo,col=team,label=team),alpha=0.9) +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~team)


# improvement this season
improvement <- 
  elo_game_history %>% 
  filter(date>ymd("2018-09-01")) %>%
  group_by(team) %>%
  summarise(delta = last(elo) - first(elo))

ggplot(improvement, aes(y=reorder(team,delta),x=delta,color=delta)) +
  geom_lollipop(horizontal = TRUE,size=1) 

# improvement this month
improvement <- 
  elo_game_history %>% 
  filter(date>ymd(Sys.Date())-30) %>%
  group_by(team) %>%
  summarise(delta = last(elo) - first(elo))


ggplot(improvement, aes(y=reorder(team,delta),x=delta,color=delta)) +
  geom_lollipop(horizontal = TRUE,size=1) 

# play-by-play ------------------------------------------------------------
## returns functions...
##    `get_single_game_stats` which plots the home team adv over time
##    `get_team_shot_x_time` which returns a team's avg shot adv by quarter
source("scripts/analyze_play_by_play.R")

get_single_game_stats(2019,"201812250GSW")

preview(gg + geom_vline(xintercept = 1690) + 
  annotate('text',x=1690-50,y=-30,label="LeBron Out",angle=90,hjust=0) + geom_vline(xintercept = 2734) + 
  annotate('text',x=2734-50,y=-17,label="Curry, KD Out",angle=90,hjust=0))

get_single_game_stats(2019,"201812250BOS")

# avg shots over time
get_team_shot_x_time(2018,"Boston Celtics"); gg1
get_team_shot_x_time(2016,"Golden State Warriors"); gg1
get_team_shot_x_time(2019,"Philadelphia 76ers")
get_team_shot_x_time(2019, "Oklahoma City Thunder")

# box scores --------------------------------------------------------------
# wrangle game-by-game box scores from the downloaded .RDS files for each player, add to a master df
source("scripts/player_box_scores.R")

# use bayesian stats to compute best estimate shooting pctages, VORP, etc, then compute a similarity matrix for nrow(Players) Players
source("scripts/bayes_stats_and_clustering.R")



# determine value of team with roster and bayesian stats ------------------
# gotta use VORP? archtypes approach? dk where to go from here...




