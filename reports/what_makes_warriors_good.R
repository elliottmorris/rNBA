source("scripts/analyze_play_by_play.R")

# in games where x team is down 5 going into the third quarter, how often win? 

season <- read.csv("data/schedules/NBA-2018_schedule.csv",stringsAsFactors = F)

teams <- unique(season$home_team_name)

get_winrate_for_3q_down <-  function(season, period_in, team){
  # season <- 2018;team= "Brooklyn Nets";period_in=3;threshold=-10
  team_pbp <- get_team_shot_x_time(season, team)
  
  game_by_period <- team_pbp %>% 
    group_by(game_id,period) %>%
    summarise(starting=first(cum_adv),
              ending=last(cum_adv)) %>%
    as.data.frame() 
    
  game_by_period <- game_by_period %>%
    group_by(game_id) %>%
    mutate(win = last(ending>0,T,F)) 
  
  third_p_down <- game_by_period %>%
    filter(period == period_in) %>%
    filter(starting > 10)
  
  # get win rate
  win_pct <- prop.table(table(third_p_down$win)) %>% 
    as.data.frame() %>% 
    filter(Var1==T) %>% 
    pull(Freq)
  
  if(length(win_pct)==0){win_pct<-0}
  
  # get decrease in score for quarter
  change_in_period <- third_p_down %>% 
    mutate(change = ending - starting) %>%
    pull(change) %>% 
    median()
  
  return(data.frame(team,
                    win_pct,
                    change_in_period,
                    stringsAsFactors = F))
  # rm(season);rm(team);rm(period_in);rm(threshold)
}

# get win rates
stats <- lapply(teams,
       function(x){
         stats =  winrate=get_winrate_for_3q_down(season=2018,period_in=3,team=x)
         
         print(stats)
         return(stats)
       }
) %>% do.call('rbind',.)


# chance of winning game
ggplot(stats, aes(y=reorder(team,win_pct),x=win_pct)) +
  geom_lollipop(horizontal = T,size=1)


# change in score in x quarter
ggplot(stats, aes(y=reorder(team,change_in_period),x=change_in_period)) +
  geom_lollipop(horizontal = T,size=1)
