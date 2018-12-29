full_roster_stats <- read_csv("output/full_gamestats_history.csv")

# compute player-game VORP -------------------------------------------------
# formula: [BPM – (-2.0)] * (% of minutes played)*(team games/82)
full_roster_stats <- full_roster_stats %>%
  mutate(VORP = (BPM - (-2.0)) * (MP/48)*(1)) %>%
  select(season, Player, starter,
         
         PTS, FG, FGA, eFG_pct, FT, FTA, FT_pct, TP, TPA, TP_pct, TOV_pct, DRB_pct, ORB_pct, TRB_pct, PF,
         
         DRtg, ORtg, BPM,
         
         VORP)


# use bayes to estimate a player’s box scores -----------------------------
## Use ebbr empirical bayes for shooting pctages
library(ebbr)

# three pointers
threes_est <- full_roster_stats %>% 
  group_by(Player) %>% 
  summarise(TP = max(sum(TP),1), 
            TPA = max(sum(TPA),1),
            season = round(mean(season))) %>%
  mutate(TP_pct = TP / TPA) %>% 
  add_ebb_estimate(TP, TPA, method="gamlss",
                   mu_predictors = ~ season + round(log(TPA)),
                   sigma_predictors = ~ season + round(log(TPA))) %>% 
  dplyr::select(Player,TP, TPA,TP_pct=.fitted)

# field goals
fg_est <- full_roster_stats %>% 
  group_by(Player) %>% 
  summarise(FG = max(sum(FG),1), 
            FGA = max(sum(FGA),1),
            season = round(mean(season))) %>%
  mutate(FG_pct = FG / FGA) %>% 
  add_ebb_estimate(FG, FGA, method="gamlss",
                   mu_predictors = ~ season + round(log(FGA)),
                   sigma_predictors = ~ season + round(log(FGA))) %>% 
  dplyr::select(Player,FG, FGA, FG_pct=.fitted)


# free throws
ft_est <- full_roster_stats %>% 
  group_by(Player) %>% 
  summarise(FT = max(sum(FT),1), 
            FTA = max(sum(FTA),1),
            season = round(mean(season))) %>%
  mutate(FT_pct = FT / FTA) %>% 
  add_ebb_estimate(FT, FTA, method="gamlss",
                   mu_predictors = ~ season + round(log(FTA)),
                   sigma_predictors = ~ season + round(log(FTA))) %>% 
  dplyr::select(Player,FT, FTA, FT_pct=.fitted)

# brms? takes too long. use lme4 to update each stat based on number of observations
library(lme4)
estimate_mean_bayes <- function(data,group_in,var_in){
  model_data <- data %>% 
    select(group=group_in,var=var_in) %>%
    group_by(group) %>%
    mutate(n=n()) %>% 
    as.data.frame() %>%
    mutate(n = n/max(n))
  
  model.lmer <- 
    lmer(var ~ (1|group),
         weights = n,
         data= model_data)
  
  player.means <- as.data.frame(lme4::ranef(model.lmer)) %>%
    mutate(var_in = condval + as.numeric(fixef(model.lmer)[1])) %>%
    select(group_in = grp,var_in)
  
  names(player.means) <- c(group_in,var_in)
  
  return(player.means)
}

vars <- names(full_roster_stats)[! names(full_roster_stats) %in% c("season","Player")]

bayes_estimates <- 
  pblapply(vars,
       function(x){
         return(estimate_mean_bayes(full_roster_stats,"Player",x))
       })

bayes_estimates <- 
  bayes_estimates %>% 
  plyr::join_all(by="Player") 


# check them stats
full_roster_stats %>% 
  group_by(Player) %>% 
  summarise(BPM.raw = mean(BPM),games=n()) %>% 
  left_join(bayes_estimates) %>%
  mutate(diff = BPM - BPM.raw)%>%
  ggplot(.,aes(x=games,y=abs(diff))) +
  geom_point() +
  geom_smooth(method='loess')


# FINAL PLAYER DATAST --  empirical update for shooting & shooting pctages, lmer updates for all other stats
Players <- threes_est %>%
  left_join(fg_est) %>%
  left_join(ft_est) %>%
  left_join(bayes_estimates %>%
              select(-c(TP,TPA,TP_pct,FG,FGA,FT,FTA,FT_pct)))
  

# finally, if a field is NA, set value to mean
for(i in names(players)){
  if(length(players[is.na(players[[i]]),][[i]])>0){
    players[is.na(players[[i]]),][[i]] <- mean(players[[i]],na.rm=T)  
  }
}


# write the stats
write.csv(Players,"output/player_stats.csv",row.names = F)

# feature selection -------------------------------------------------------
# set.seed(7)
# # load the library
# library(mlbench)
# library(caret)
# # prepare training scheme
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# # train the model
# model <- train(PTS~., data=players[4:length(players)],
#                method="rf", preProcess="scale", trControl=control,importance=T)
# # estimate variable importance
# importance <- varImp(model, scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)

# 
# rf variable importance
# 
# Overall
# FG      45.62691
# TP      25.19834
# TPA     23.63687
# FGA     18.65551
# FT      12.11729
# ORtg     9.47279
# FTA      8.37935
# ORB_pct  4.45972
# eFG_pct  4.35761
# DRtg     3.94272
# VORP     2.38062
# PF       2.31274
# DRB_pct  2.12753
# BPM      2.05545
# FT_pct   1.35750
# TP_pct   0.92339
# TOV_pct  0.69641
# TRB_pct  0.09517

# cluster analysis --------------------------------------------------------
set.seed(123)

# Compute and plot wss for k = 2 to k = 15.
k.max <- 50
data <- players[4:length(players)]
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
cluster <- kmeans(players[,4:length(players)],centers = 26)

players$cluster <- 
  cluster$cluster

players %>% group_by(cluster) %>% summarise(n=n())

# !!! SANITY CHECK -- which one is LeBron in? Who else?
LeBronCluster <- players[match("LeBron James",players$Player),]$cluster

players %>% 
  filter(cluster == LeBronCluster) %>% View()


# similarity scores -------------------------------------------------------
# rescale the data
players.reg <- players %>% as.data.frame()

# rescale each column
for(i in 4:ncol(players.reg)){
  players.reg[,i] <- rescale(players.reg[,i],to = c(0,1))
}

# similarity from kd-trees using the `RANN` package
library(RANN)

num_matches <- nrow(players.reg) # add one b/c the first row always returns the player himself as the most similar

neighbors.rann <- nn2(data=players.reg[,4:length(players.reg)],
    k=num_matches,
    treetype='kd')

matches <- apply(X=as.data.frame(neighbors.rann[[1]]),
                   MARGIN=2,
                   FUN=function(x){players.reg$Player[x]}
                   ) %>%
  as.data.frame(stringsAsFactors=F) %>%
  setNames(c('Player',paste0('match-',(2:num_matches)-1)))

rownames(matches) <- NULL

scores <- as.data.frame(neighbors.rann[[2]])

# pblapply through names, index similarity
cl = makeCluster(detectCores()-1,type = "FORK")

similarity <- 
  pblapply(1:length(players$Player),cl=cl,
       function(i){
         a <- players$Player[i]
         
         idx <- match(players$Player,
                      as.character(matches[matches$Player==a,]))
         
         a_scores <- scores[i,][,idx]
         
         
         return(a_scores)
         }
       ) 

similarity <- rbindlist(similarity) %>% as.data.frame()

stopCluster(cl)

nrow(similarity)
length(similarity)

# scale similarity to be from 0 to 1
similarity <- 1 - (similarity/max(similarity))

# analyze similarity
rownames(similarity) <- players.reg$Player
colnames(similarity) <- players.reg$Player

similarity %>% select(`LeBron James`) %>% View()
similarity %>% select(`Stephen Curry`) %>% View()
similarity %>% select(`Kevin Durant`) %>% View()
similarity %>% select(`Kobe Bryant`) %>% View()


data.frame(Lebron = similarity %>% pull(`LeBron James`),
           Curry = similarity %>% pull(`Stephen Curry`),
           KD = similarity %>% pull(`Kevin Durant`),
           Kobe = similarity %>% pull(`Kobe Bryant`)) %>%
  gather(player,similarity,1:4) %>%
  ggplot(.,aes(x=similarity,fill=player)) +
  geom_density(alpha=0.5)

# write similarity matrix
write.csv(similarity,'output/similarity.csv')




