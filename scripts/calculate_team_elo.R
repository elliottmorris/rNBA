# this file calculates teams' elo ratings after each game played in a season. 
# initial elo is set to 1300, the average over time per 538
# the formula for elo is: 
#     R[i+1] = R[i] + K * (S[a] - E[a]) 
#             where
#     e[a] = 1 / 1 + 10^((elo[b] - elo[a]) / 400)
#             and
#     K = 20 * (MoV[a] + 3)^0.8 / 7.5 + 0.006 * elo_difference[a]
# where R[i] = previous elo, K = 20, S = 1 if team a wins or 0 if it loses, and E[a] is the expected outcome for team a. 

# master var: starting elo:
init_elo <- 1505

# function to calculating elo
calculate_elo = function(elo_a, elo_b, pts_a, pts_b){
  
  #elo_a = 1618;elo_b = 1500;pts_a = 94;pts_b = 90
  
  # home-field adv
  elo_a = elo_a + 100
  
  # calc mov multiplier, times k
  if(pts_a > pts_b){
    elo_w = elo_a
    elo_l = elo_b
  } else if (pts_a < pts_b){
    elo_w = elo_b
    elo_l = elo_a
  }
  
  MOV_mult =  (((abs(pts_a - pts_b) + 3)^0.8) / (7.5 + 0.006 * (elo_w - elo_l)))
  MOV_mult
  
  K = 20 * MOV_mult
  
  # calc expectation
  E <- 1 / (10^((elo_b - elo_a) / 400) + 1)
  
  elo_update =  K * (ifelse(pts_a > pts_b,1,0) - E) 
  
  return(elo_update)
}

calculate_elo(elo_a = 1618,elo_b = 1500,pts_a = 94,pts_b = 90)


# elo for current season, set at 1505 initially ------------------------------
# compute elo for a year's worth of games
year <- 2019

season <- read.csv(sprintf("data/schedules/NBA-%s_schedule.csv",year),stringsAsFactors = F)

season <- season %>% 
  arrange(ymd(date_game)) %>% 
  filter(!is.na(home_pts))

elo_df <- data.frame(team = unique(season$home_team_name),
                     elo = init_elo,
                     date = 'pre',
                     stringsAsFactors = F)

# update after each game
elo_games_played <- 
  lapply(1:nrow(season),
       function(game_idx){
         game <- season[game_idx,]
        
        # elo, either from lag or set to 1300
        elo_a = elo_df[elo_df$team == game$home_team_name,]$elo
        
        elo_b = elo_df[elo_df$team == game$visitor_team_name,]$elo
        
        # update
        elo_update <- calculate_elo(elo_a = elo_a, 
                                    elo_b = elo_b,
                                    pts_a = game$home_pts,
                                    pts_b = game$visitor_pts)
        
        elo_a_new <- elo_a + elo_update
        
        elo_b_new <- elo_b - elo_update
        
        
        # update the elo
        elo_df[elo_df$team == game$home_team_name,]$elo <<- elo_a_new
        elo_df[elo_df$team == game$visitor_team_name,]$elo <<- elo_b_new
        
        # return a df for graphing
        elo_df.a <- data.frame(team = game$home_team_name,
                               elo = elo_a_new,
                               date = game$date_game,
                               stringsAsFactors = F)
        
        elo_df.b <- data.frame(team = game$visitor_team_name,
                               elo = elo_b_new,
                               date = game$date_game,
                               stringsAsFactors = F)
        
        return(rbind(elo_df.a,elo_df.b))
       }
) %>% do.call('rbind',.)


top_elo <- elo_games_played %>% 
  arrange(desc(ymd(date)),desc(elo)) %>%
  group_by(team) %>%
  summarise(elo = first(elo),date=first(date)) %>%
  as.data.frame() %>% 
  arrange(desc(elo)) 

top_elo

elo_games_played$team = factor(elo_games_played$team,top_elo$team)

ggplot(elo_games_played,# %>% filter(team %in% top_elo$team),
       aes(x=ymd(date),y=elo,col=team)) +
  geom_step() +
  #geom_label_repel(data = top_elo,aes(x=ymd(date),y=elo,col=team,label=team),alpha=0.9) +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(~team)



# compute historical elo --------------------------------------------------
# start in 1950 with everyone set at 1950, loop through every game, at the end of each season carrying over elo = to (final elo * 0.75) + (1505*0.25), per 538

years <- substr(dir("data/schedules/"),5,8) %>% as.numeric()
years <- 1950:2019
elo_overtime <- vector('list',length(years))


print("####################################")
print("CALCULATE HISTORICAL ELO:")
print("####################################")

for (year_idx in 1:length(years)){
  year <- years[[year_idx]]
  print(sprintf("Getting elo for %s",year))
  
  # get schedule
  season <- read.csv(sprintf("data/schedules/NBA-%s_schedule.csv",year),stringsAsFactors = F)
  
  season <- season %>% 
    arrange(ymd(date_game)) %>% 
    filter(!is.na(home_pts))
  
  # initialize elo at 1505 for 1950, else take recent season's elo * 1950
  if(year == min(years)){
    elo_df <- data.frame(team = unique(season$home_team_name),
                         elo = init_elo,
                         date = 'pre',
                         stringsAsFactors = F)
  } else {
    # if new team, append at 1505, else take the carryover
    elo_df <- data.frame(team = unique(season$home_team_name),
                         elo = init_elo,
                         date = 'pre',
                         stringsAsFactors = F)
    
    elo_df[elo_df$team %in% elo_carryover$team,]$elo <- elo_carryover[
      match(elo_df[elo_df$team %in% elo_carryover$team,]$team,
            elo_carryover$team),]$elo
    
    
  }
  
  
  # update after each game
  elo_games_played <- 
    lapply(1:nrow(season),
           function(game_idx){
             game <- season[game_idx,]
             
             # elo, either from lag or set to 1300
             elo_a = elo_df[elo_df$team == game$home_team_name,]$elo
             
             elo_b = elo_df[elo_df$team == game$visitor_team_name,]$elo
             
             # update
             elo_update <- calculate_elo(elo_a = elo_a, 
                                         elo_b = elo_b,
                                         pts_a = game$home_pts,
                                         pts_b = game$visitor_pts)
             
             elo_a_new <- elo_a + elo_update
             
             elo_b_new <- elo_b - elo_update
             
             
             # update the elo
             elo_df[elo_df$team == game$home_team_name,]$elo <<- elo_a_new
             elo_df[elo_df$team == game$visitor_team_name,]$elo <<- elo_b_new
             
             # return a df for graphing
             elo_df.a <- data.frame(team = game$home_team_name,
                                    elo = elo_a_new,
                                    date = game$date_game,
                                    stringsAsFactors = F)
             
             elo_df.b <- data.frame(team = game$visitor_team_name,
                                    elo = elo_b_new,
                                    date = game$date_game,
                                    stringsAsFactors = F)
             
             return(rbind(elo_df.a,elo_df.b))
           }
    ) %>% do.call('rbind',.)
  
  # get final elo for this season
  elo_carryover <- elo_df %>%
    mutate(elo = (elo*0.75) + (1505*0.25),
           date = 'post')
  
  # return the final elo, and every game played
  elo_overtime[[year_idx]] <- list(elo_games_played,elo_carryover)
}

