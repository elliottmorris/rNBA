#This script scrapes top-level NBA schedule and results from basketball-reference.com.
# User can set year and list of months to determine the window of games to scrape.
# At the end of the script, I reconstruct the conference standings based on W-L
# percentage.


# Scrape schedule and scores ----------------------------------------------
# identify years already downloaded
already_downloaded <- lapply(dir('data/schedules/'),function(x){substr(x,5,8)}) %>% unlist()

# download all years
years <- as.character(seq(2018,1950,-1)) # earliest is 1950

years <- years[!years %in% already_downloaded]

# redo this season every time
#years <- append('2019',years)


# input year as string
download_yearly_game_data <- function(year){
  print("##############################")
  print(sprintf("Scraping %s...",year))
  
  ########
  # PARAMETERS
  ########
  monthList <- tolower(month.name)
  playoff_startDate <- ymd("2018-04-14")
  
  ########
  # SCRIPT FOR SCRAPING DATA STARTS HERE
  ########
  df <- data.frame()
  for (month in monthList) {
    # get webpage
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, 
                  "_games-", month, ".html")
    
    webpage <- try(read_html(url),silent = TRUE)
    
    if(isTRUE(class(webpage)=="try-error")){next}
    
    # get column names
    col_names <- webpage %>% 
      html_nodes("table#schedule > thead > tr > th") %>% 
      html_attr("data-stat")    
    col_names <- c("game_id", col_names)
    
    # extract dates column
    # note that in april, there is a break in the table which just says 
    # "Playoffs". this messes with the data merging later, so we get rid of it
    dates <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > th") %>% 
      html_text()
    dates <- dates[dates != "Playoffs"]
    
    # extract game id
    # we need to remove the NA that is due to the "Playoffs" row in april
    game_id <- webpage %>% 
      html_nodes("table#schedule > tbody > tr > th") %>%
      html_attr("csk")
    game_id <- game_id[!is.na(game_id)]
    
    # extract all columns (except date)
    data <- try(webpage %>% 
      html_nodes("table#schedule > tbody > tr > td") %>% 
      html_text() %>%
      matrix(ncol = length(col_names) - 2, byrow = TRUE),silent = T)
    
    if(isTRUE(class(data)=="try-error")){next}
    
    # combine game IDs, dates and columns in dataframe for this month, add col names
    month_df <- as.data.frame(cbind(game_id, dates, data), stringsAsFactors = FALSE)
    names(month_df) <- col_names
    
    # add to overall dataframe
    df <- rbind(df, month_df)
  }
  
  # change columns to the correct types
  df$visitor_pts <- as.numeric(df$visitor_pts)
  df$home_pts    <- as.numeric(df$home_pts)
  df$attendance  <- as.numeric(gsub(",", "", df$attendance))
  df$date_game   <- mdy(df$date_game)
  
  # add column to indicate if regular season or playoff
  #df$game_type <- with(df, ifelse(date_game >= playoff_startDate, 
  #                                "Playoff", "Regular"))
  
  # drop boxscore column
  df$box_score_text <- NULL
  
  # sort by date
  df <- df %>% arrange(ymd(date_game))
  
  # save to file
  write.csv(df,
            sprintf("data/schedules/NBA-%s_schedule.csv",year),
            row.names = F)
  
  # return file
  return(df)
}

#download_yearly_game_data(year='2019')

# loop to scrape all years!
print("####################################")
print("GET ALL YEARLY SCHEDULES:")
print("####################################")

for (x in years){
  download_yearly_game_data(year=x)
  
  print("Sleeping...")
  Sys.sleep(10)
}



# Scrape box scores and pbp -----------------------------------------------------
# This script scrapes the box scores for all NBA games in the 2017-18 season.
# Results are saved into a list called `master`. The key for a game is its
# game_id (which we can get from the output of 2018-12-11_nba_game_data.R).
# master$game_id is itself a list of 5 items:
#   1. visitor basic box score
#   2. visitor advanced box score
#   3. home basic box score
#   4. home advanced box score
#   5. score profile against time (like data frame in nba_play_by_play_single_game.R)
#
# To use this script for other seasons, run 2018-12-11_nba_game_data.R for a 
# different year, then use that output file as the input for this script.

# remove the +'s from play-by-play score
parseScore <- function(x) {
  if (startsWith(x, "+")) {
    return(str_sub(x, 3, str_length(x)))
  } else if (endsWith(x, "+")) {
    return(str_sub(x, 1, str_length(x) - 1))
  } else {
    return(x)
  }
}

# helper function to get raw HTML box scores in better shape
parseBoxScore <- function(xx) {
  names(xx) <- c("Player", xx[1,][-1])  # get correct col names
  xx[xx == "Did Not Play"] <- NA
  
  # new col to say who started and who was reserve
  xx$Role <- "Reserve"
  xx$Role[1:6] <- "Starter"
  xx$Role[nrow(xx)] <- NA
  
  # remove old column headings, coerce statistics to numeric type
  xx <- xx[c(-1, -7), ]
  for (j in 3:(ncol(xx)-1)) {
    xx[, j] <- as.numeric(xx[, j])
  }
  xx
}

# identify years already downloaded
already_downloaded <- lapply(dir('data/game data/'),function(x){substr(x,5,8)}) %>% unlist()

# download all years
years <- as.character(seq(2018,1950,-1)) # earliest is 1950

years <- years[!years %in% already_downloaded]

# for this season, make sure to update new games -- have to read the schedule file and game file and compare
#years <- append('2019',years)

# scraping function
scrape_box_scores <- function(year){
  print("##############################")
  print(sprintf("Scraping %s...",year))
  
  season <- sprintf("NBA-%s_schedule.csv",year)

  game_df <- read.csv(sprintf('data/schedules/%s',season),stringsAsFactors = F)
  
  # trim to only finished games, duh
  game_df <- game_df %>% filter(!is.na(home_pts))
  
  # get every game score, in parallel
  master <- list()
  
  cl = makeCluster(detectCores()-1,type="FORK")
  
  pblapply(game_df$game_id,cl=cl,
           function(current_id){
             print(current_id)
    
            ##########
            # get box scores
            ##########
            url <- paste0("https://www.basketball-reference.com/boxscores/", current_id,
                          ".html")
            webpage <- read_html(url)
            
            tables <- webpage %>% html_nodes("table") %>%
              html_table()
            names(tables) <- c("visitor_basic_boxscore", "visitor_adv_boxscore",
                               "home_basic_boxscore", "home_adv_boxscore")
            tables <- lapply(tables, parseBoxScore)
            
            ##########
            # get play-by-play score profile
            ##########
            url <- paste0("https://www.basketball-reference.com/boxscores/pbp/", current_id,
                          ".html")
            webpage <- read_html(url)
            
            # pull out the events from the play-by-play table
            events <- webpage %>% 
              html_nodes("#pbp") %>%
              html_nodes("tr") %>% 
              html_text()
            
            # get event times & scores
            times  <- str_extract(events, "^\\d+:\\d+.\\d+")
            scores <- str_extract(events, "[\\+]*\\d+-\\d+[\\+]*")
            scores <- ifelse(str_detect(scores, "\\+"), scores, NA)
            
            pdp_df <- data.frame(time = times, score = scores, stringsAsFactors = FALSE) %>%
              na.omit()
            pdp_df$score <- sapply(pdp_df$score, parseScore)
            
            # split score into visitor and home score, get home advantage
            pdp_df <- pdp_df %>% 
              separate(score, into = c("visitor", "home"), sep = "-") %>%
              mutate(visitor = as.numeric(visitor), 
                     home = as.numeric(home),
                     time = ms(time))
            
            # get period of play (e.g. Q1, Q2, ...)
            pdp_df$period <- NA
            period <- 0
            prev_time <- ms("0:00")
            for (i in 1:nrow(pdp_df)) {
              curr_time <- pdp_df[i, "time"]
              if (prev_time < curr_time) {
                period <- period + 1
              }
              pdp_df[i, "period"] <- period
              prev_time <- curr_time
            }
            
            # convert time such that it runs upwards. regular quarters are 12M long, OT 
            # periods are 5M long
            pdp_df <- pdp_df %>% mutate(time = ifelse(period <= 4, 
                                                      as.duration(12 * 60) - as.duration(time),
                                                      as.duration(5  * 60) - as.duration(time))) %>%
              mutate(time = ifelse(period <= 4,
                                   time + as.duration(12 * 60 * (period - 1)),
                                   time + as.duration(12 * 60 * 4) + 
                                     as.duration(5 * 60 * (period - 5))
              ))
            
            tables$pdp_df <- pdp_df
            
            master[[current_id]] <- tables
          }
  )
  
  stopCluster(cl)
  
  saveRDS(master, sprintf("data/game data/NBA-%s_game_data.rds",year))
  
  
}

# loop to scrape all years!
print("####################################")
print("SCRAPE BOX SCORES AND PBP:")
print("####################################")

for (x in years){
  scrape_box_scores(year=x)
  
  print("Sleeping...")
  Sys.sleep(10)
}

