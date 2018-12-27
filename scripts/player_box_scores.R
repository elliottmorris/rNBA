# Model a player's offensive and defensive +/-, updating each game, prior is average for 'similar' players, determined by the k-means of a bunch of different stats, using RF and elastic nets
# step 1: for a specific season, get all the game ids when player x actually played, regardless of the team they were on
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
  
  # filter out rows with no significant play time -- convert to seconds
  stats$MP <- as.numeric(str_split(stats$MP,":",simplify = T)[,1]) * 60 + 
    as.numeric(str_split(stats$MP,":",simplify = T)[,2])
  
  stats <- stats %>% filter(MP >= 5*60) # only keep game if player played more than 5 mintues
  
  # rename some vars
  stats <- stats %>%
    dplyr::rename(FG_pct = `FG%`,
                  TP_pct = `3P%`,
                  TP = `3P`,
                  TPA = `3PA`,
                  FT_pct = `FT%`,
                  plus_minus = `+/-`,
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
  
  roster_l <- pblapply(teams,
         function(x){
           
           full_nba_roster[[x]] <<- get_team_roster(season,x)
         })
  
  return(full_nba_roster)
}
  

season_yr <- 2019
full_nba_roster <- get_players_season(season_yr)

# get each games' stats for every player ----------------------------------------------
full_roster_stats <- 
  pblapply(1:length(full_nba_roster),
       function(idx){
         roster <- full_nba_roster[[idx]]
         
         #print(names(full_nba_roster)[idx])
         
         roster_stats <- lapply(roster,
                function(player){
                  #print(player)
                  return(get_player_stats(season_yr,player))
                }
         ) %>% do.call('rbind',.)
         
         return(data.frame(team = names(full_nba_roster)[[idx]],
                           roster_stats))
         
         }
       ) %>% do.call('rbind',.)



# compute summary stats ---------------------------------------------------
## for now, take the mean. in the end, this needs to update via empirical bayes
# summarise by player
players <- full_roster_stats %>% 
  group_by(team,Player) %>%
  summarise_if(is.numeric, mean, na.rm=T) %>%
  as.data.frame()

# fill in NA with mean
for(i in 3:ncol(players)){
  players[is.na(players[,i]), i] <- mean(players[,i], na.rm = TRUE)
}


# feature selection -------------------------------------------------------
# set.seed(7)
# # load the library
# library(mlbench)
# library(caret)
# # prepare training scheme
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# # train the model
# model <- train(plus_minus~., data=players[3:length(players)], 
#                method="rf", preProcess="scale", trControl=control,importance=T)
# # estimate variable importance
# importance <- varImp(model, scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)
# 

# RESULTS:
#          Importance   
# DRtg     44.913
# ORtg     26.809
# STL      10.757
# MP       10.106
# DRB       9.765
# AST       8.700
# TPAr      8.357
# ORB_pct   8.238
# TOV       8.108
# ORB       7.387
# FGA       6.479
# TPA       6.331
# TRB       6.142
# TP        5.845
# starter   5.703
# PTS       5.565
# AST_pct   5.049
# PF        5.028
# FT_pct    5.017
# DRB_pct   4.844


# cluster analysis --------------------------------------------------------
set.seed(123)

# Compute and plot wss for k = 2 to k = 15.
k.max <- 50
data <- players[3:length(players)]
wss <- sapply(2:k.max, 
              function(k){
                kmeans(data, k)$tot.withinss
                }
              )

calib <- data.frame(tot.within = wss) %>%
  mutate(change = tot.within - lag(tot.within))

plot(2:k.max, calib$tot.within,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# optimal is 10
cluster <- kmeans(players[,3:length(players)],centers = 25)

players$cluster <- 
  cluster$cluster

# !!! SANITY CHECK -- which one is LeBron in? Who else?
LeBronCluster <- players[match("LeBron James",players$Player),]$cluster

players %>% 
  filter(cluster == LeBronCluster)

# !!!  some players get transferred mid-season. Scrap them. Just keep the first observation for now. Later, figure out where they actually are. (Probably by using a better source for the rosters...)
players <- players %>%
  group_by(Player) %>%
  summarise_all(first) %>% 
  as.data.frame()


# similarity scores -------------------------------------------------------
# rescale the data
players.reg <- players

# rescale each column
for(i in 3:ncol(players.reg)){
  players.reg[,i] <- rescale(players.reg[,i],to = c(0,1))
}

# select the most important variables
players.reg <- players.reg %>%
  select(team, Player, starter,
         
         FGA, eFG_pct, FTA, FT_pct, TOV_pct, DRB_pct, ORB_pct, PF,
         
         DRtg, ORtg, plus_minus)

# calc similarity scores for every combination -- IN PARALLEL
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

cl <- makeCluster(detectCores()-1,type="FORK")

dist.matrix <- pblapply(1:nrow(players.reg),cl = cl,
       function(player_a_idx){
         a <- players.reg[player_a_idx,]
         
         # similarity between every player
         similarities <- 
           lapply(1:nrow(players.reg),
                function(player_b_idx){
                  b <- players.reg[player_b_idx,]
                  
                  dist <- euc.dist(a[3:length(a)], b[3:length(b)])
           
         }) %>% do.call('c',.)
         
         return(similarities)
         
       }) %>% 
  do.call('cbind',.)

stopCluster(cl)

# scale distances to be similarity
similarity <- 1 - dist.matrix / max(dist.matrix)

similarity <- as.data.frame(similarity)

rownames(similarity) <- players.reg$Player
colnames(similarity) <- players.reg$Player


similarity %>% select_("`Kevin Durant`") %>% View()





