# Model a player's offensive and defensive +/-, updating each game, prior is average for 'similar' players, determined by the k-means of a bunch of different stats, using RF and elastic nets
# step 1: for a specific season, get all the game ids when player x actually played, regardless of the team they were on



# get boxe scores from all a player’s games -------------------------------
# helper function for finding the game in a list
find_game_in_season_list <- function(box_pbp_list,game_id_str){
  games <- 
    lapply(box_pbp_list,
           function(x){
             return(x$game_id)
           }
    ) %>% unlist()
  
  idx <- match(game_id_str,games)
  
  return(box_pbp_list[[idx]])
}


# function to get the box scores
get_player_stats <- function(season, player_name){
  # read in rds of season play-by-play
  box_pbp_list <- readRDS(sprintf("data/game data/NBA-%s_game_data.rds",season))
  
  # get game ids with that player
  stats <- 
    lapply(box_pbp_list,
           function(x){
             # print(x$game_id)
             # if in visitor, return stats
             if(tolower(player_name) %in% tolower(x$visitor_basic_boxscore$Player)){
               box <- x$visitor_basic_boxscore %>%
                 filter(tolower(Player) == tolower(player_name)) 
               
               adv <- x$visitor_adv_boxscore %>%
                 filter(tolower(Player) == tolower(player_name))
               
               stats <- box %>% 
                 left_join(adv %>% 
                             dplyr::select(-c(MP,Role)),by='Player') %>%
                 mutate(game_id = x$game_id,
                        home = "F") 
               
               return(stats)
             } else if(tolower(player_name) %in% tolower(x$home_basic_boxscore$Player)){
               box <- x$home_basic_boxscore %>%
                 filter(tolower(Player) == tolower(player_name))
               
               adv <- x$home_adv_boxscore %>%
                 filter(tolower(Player) == tolower(player_name))
               
               stats <- box %>% 
                 left_join(adv %>% 
                             dplyr::select(-c(MP,Role)),by='Player') %>%
                 mutate(game_id = x$game_id,
                        home = "F") 
               
               return(stats)
             } 
           }
    ) %>% do.call('rbind',.)
  
  # filter out rows with NA minutes played
  stats <- stats %>% filter(!is.na(FG))
  
  # IF DF IS EMPTY NOW, MEANS THEY HAVE NOT PLAYED -- RETURN NULL
  if(nrow(stats)==0){return(NULL)}
  
  # filter out rows with no significant play time 
  stats$MP <- (as.numeric(str_split(stats$MP,":",simplify = T)[,1]) * 60 + 
    as.numeric(str_split(stats$MP,":",simplify = T)[,2]) ) / 60 
  
  #stats <- stats %>% filter(MP >= 2) # only keep game if player played more than 2 mintues
  
  # rename some vars
  stats <- stats %>%
    dplyr::rename(FG_pct = `FG%`,
                  TP_pct = `3P%`,
                  TP = `3P`,
                  TPA = `3PA`,
                  FT_pct = `FT%`,
                  BPM = `+/-`,
                  TS_pct = `TS%`,
                  eFG_pct = `eFG%`,
                  TPAr = `3PAr`,
                  ORB_pct = `ORB%`,
                  DRB_pct = `DRB%`,
                  TRB_pct = `TRB%`,
                  AST_pct = `AST%`,
                  STL_pct = `STL%`,
                  BLK_pct = `BLK%`,
                  TOV_pct = `TOV%`,
                  USG_pct = `USG%`)
  
  # code role as numeric
  stats <- stats %>% 
    mutate(Role = case_when(stats$Role == "Starter" ~ 1,stats$Role == "Reserve" ~ 0)) %>% 
    dplyr::rename(starter = Role)
  
  # put qual vars first, quant second
  quant_vars <- names(stats)[!names(stats) %in% c("Player","home","game_id","starter")]
  
  stats <- stats %>% 
    select(Player,home,game_id,starter,quant_vars)
  
  return(stats)
}


# get entire roster for a season ------------------------------------------
get_players_season <- function(season){
  
  schedule <- read.csv(sprintf("data/schedules/NBA-%s_schedule.csv",season),stringsAsFactors = F)
  
  teams <- unique(schedule$visitor_team_name)
  roster <- vector('list',length(teams))
  
  # get a list of all active NBA players, from f.e. http://www.espn.com/nba/team/roster/_/name/gs/golden-state-warriors
  
  get_team_roster <- function(season,team_name){
    # read in data files
    game_df <- read.csv(sprintf("data/schedules/NBA-%s_schedule.csv",season),stringsAsFactors = F)
    master_list <- readRDS(sprintf("data/game data/NBA-%s_game_data.rds",season))
    
    # can only run for games already played, so chop out unpalyed games
    game_df <- game_df %>%
      filter(!is.na(home_pts))
    
    # find any game one team is a participant in
    game_rows <- which(game_df$visitor_team_name == team_name |
                         game_df$home_team_name == team_name)
    
    game_ids <- game_df[game_rows, "game_id"]
    
    
    # set up game data frame and play-by-play list
    df <- data.frame(matrix(NA, ncol = 3, nrow = length(game_ids)))
    names(df) <- c("game_id", "home", "win")
    df$game_id <- game_ids
    df$home <- game_df[game_rows, "home_team_name"] == team_name
    
    roster_list <- list()
    for (i in 1:nrow(df)) {
      id <- game_ids[i]
      game <- find_game_in_season_list(master_list,id)
      
      if (df[i, "home"]) { # if team is home team
        roster_temp <- game$home_basic_boxscore$Player
      } else {
        roster_temp <- game$visitor_basic_boxscore$Player
      }
      roster_list[[i]] <- roster_temp
    }
    
    roster <- roster_list %>% 
      do.call('c',.) %>% 
      unique()
    
    # the name "Team Totals" gets generated. Exclude it.
    return(roster[roster != "Team Totals"])
    
  }
  
  full_nba_roster <- list()
  
  roster_l <- lapply(teams,
         function(x){
           
           full_nba_roster[[x]] <<- get_team_roster(season,x)
         })
  
  return(full_nba_roster)
}
  

# get each games' stats for every player ----------------------------------------------
get_all_players_stats <- function(season){
  full_nba_roster <- get_players_season(season)
  
  full_roster_stats <- 
    pblapply(1:length(full_nba_roster),
         function(idx){
           roster <- full_nba_roster[[idx]]
           
           #print(names(full_nba_roster)[idx])
           
           roster_stats <- lapply(roster,
                  function(player){
                    #print(player)
                    return(get_player_stats(season,player))
                  }
           ) %>% do.call('rbind',.)
           
           return(data.frame(team = names(full_nba_roster)[[idx]],
                             roster_stats))
           
           }
         ) %>% do.call('rbind',.)
}


# get stats for every player… ever ----------------------------------------
# get all available .RDS data files
years <- as.numeric(substr(dir("data/game data/"),5,8)) %>% rev()

# get box score data from every year
full_roster_stats <- 
  lapply(years,
       function(x){
         print("################################")
         print(sprintf("GETTING STATS FOR %s",x))
         return(data.frame(season=x,
                           get_all_players_stats(x)))
       }) %>% 
  do.call('rbind',.)

# save it, so that we only re-run for new years (implement in function -- read .csv, filter somehow, conditionals??)
write.csv(full_roster_stats,"output/full_gamestats_history.csv",row.names = F)



