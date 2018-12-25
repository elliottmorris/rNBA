# helpers -----------------------------------------------------------------
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



# get stats from a single game --------------------------------------------
get_single_game_stats <- function(season,game_id){
  
  # read in rds of season play-by-play
  box_pbp <- readRDS(sprintf("data/game data/NBA-%s_game_data.rds",season))
  
  # return the list item with `box_pbp[[x]]$game_id` == game_id
  game_stats <- find_game_in_season_list(box_pbp_list = box_pbp,game_id_str = game_id)
  
  # vis play-by-play
  pbp <- game_stats$pbp_df
  
  # calc diff
  pbp <- pbp %>% 
    mutate(home_adv = home - visitor,
           win = home>visitor)
  
  # chart
  ggplot(pbp, aes(x=time/60,y=home_adv)) + 
    geom_hline(yintercept=0) +
    geom_step(aes(col=home_adv>0,group=1)) +
    labs(x="Minute",y="Home Scoring Adv.") +
    facet_wrap(~period,nrow=1,scales='free_x')
  
}



# get point differential for a team ---------------------------------------

get_team_shot_x_time <- function(season, team_name){
  # read in data files
  game_df <- read.csv(sprintf("data/schedules/NBA-%s_schedule.csv",season),stringsAsFactors = F)
  master_list <- readRDS(sprintf("data/game data/NBA-%s_game_data.rds",season))
  
  # can only run for games already played, so chop out unpalyed games
  game_df <- game_df %>%
    filter(!is.na(home_pts))
    
  # find any game one team is a participant in
  #team_name <- "Golden State Warrios"
  
  game_rows <- which(game_df$visitor_team_name == team_name |
                       game_df$home_team_name == team_name)
  
  game_ids <- game_df[game_rows, "game_id"]
  
  # set up game data frame and play-by-play list
  df <- data.frame(matrix(NA, ncol = 3, nrow = length(game_ids)))
  names(df) <- c("game_id", "home", "win")
  df$game_id <- game_ids
  df$home <- game_df[game_rows, "home_team_name"] == team_name
  df$win <- (df$home & game_df[game_rows, "home_pts"] > game_df[game_rows, "visitor_pts"]) |
    ((!df$home) & game_df[game_rows, "home_pts"] < game_df[game_rows, "visitor_pts"])
  
  pbp_list <- list()
  for (i in 1:nrow(df)) {
    id <- game_ids[i]
    game <- find_game_in_season_list(master_list,id)
    pbp <- game$pbp_df
    if (df[i, "home"]) { # if team is home team
      names(pbp) <- c("time", "opp", "team", "period")
    } else {
      names(pbp) <- c("time", "team", "opp", "period")
    }
    pbp_list[[id]] <- pbp
  }
  
  
  parse_pbp <- function(pbp) {
    pbp <- rbind(0, pbp)
    new_pbp <- pbp
    
    # get points scored in the period
    last_opp <- 0; last_team <- 0; last_period <- 0
    for (i in 2:nrow(pbp)) {
      if (pbp[i, "period"] > last_period + 1) {
        last_period <- last_period + 1
        last_opp <- pbp[i-1, "opp"]
        last_team <- pbp[i-1, "team"]
      }
      new_pbp$opp[i] <- pbp$opp[i] - last_opp
      new_pbp$team[i] <- pbp$team[i] - last_team
    }
    
    # add extra rows to denote beginning and end of periods
    num_period <- max(new_pbp$period)
    for (i in 1:num_period) {
      end_row <- new_pbp[max(which(new_pbp$period == i)), ]
      end_row[1] <- 12 * 60 * min(i, 4) + 5 * 60 * max(i-4, 0) 
      beg_row <- c(0, 0, 0, i)
      beg_row[1] <- 12 * 60 * min(i-1, 4) + 5 * 60 * max(i-1-4, 0) 
      new_pbp <- rbind(new_pbp, beg_row)
      new_pbp <- rbind(new_pbp, end_row)
    }
    new_pbp <- new_pbp[order(new_pbp$time), ]
    new_pbp$adv <- with(new_pbp, team - opp)
    new_pbp[-1, ]
  }
  
  pbp_list <- lapply(pbp_list, parse_pbp)
  
  # build up a master play-by-play data frame
  pbp_df <- data.frame(matrix(ncol = 6, nrow = 0))
  names(pbp_df) <- c("game_id", "home", "win", "time", "period", "adv")
  for (i in 1:length(pbp_list)) {
    xx <- pbp_list[[i]]
    xx$game_id <- df$game_id[i]
    xx$home <- df$home[i]
    xx$win <- df$win[i]
    pbp_df <- rbind(pbp_df, xx)
  }
  
  periods <- unique(pbp_df$period)
  x_value <- ifelse(periods <= 4, 12 * 60 * periods, 
                    12 * 60 * 4 + 5 * 60 * (periods - 4))
  x_label <- ifelse(periods <= 4, paste0("Q", periods), 
                    paste0("OT", periods - 4))
  
  
  # cumulative period chart -- highlight medium
  gg1 <- ggplot(pbp_df, aes(x = time, y = adv)) +
    geom_line(aes(col = win, group = interaction(game_id, period)), 
              lwd = 0.1) +
    geom_smooth(method='gam',aes(group = period), col = "black", se = FALSE) +
    scale_x_continuous(breaks = x_value, labels = x_label) +
    scale_color_manual(values = c("#ff6600", "#3366ff")) +
    coord_cartesian(ylim = c(-10, 10)) +
    labs(title = paste("Point Advantage by Quarter,", team_name)) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.grid.minor.x = element_blank(), 
          panel.grid.minor.y = element_blank(),
          legend.position = "bottom")
  
  # with win indicator -- highlight medium
  gg2 <- ggplot(pbp_df, aes(x = time, y = adv)) +
    geom_line(aes(col = win, group = interaction(game_id, period)), 
              lwd = 0.1) +
    geom_smooth(method='gam',aes(group = period), col = "black", se = FALSE) +
    scale_x_continuous(breaks = x_value, labels = x_label) +
    scale_color_manual(values = c("#ff6600", "#3366ff")) +
    coord_cartesian(ylim = c(-10, 10)) +
    facet_grid(win ~ .) +
    labs(title = paste("Point Advantage by Quarter,", team_name)) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          panel.grid.minor.x = element_blank(), 
          panel.grid.minor.y = element_blank(),
          legend.position = "bottom")
  
  grid.arrange(gg1, gg2)
}


