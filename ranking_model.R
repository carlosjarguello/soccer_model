# Bradley Terry model for International Soccer

library(tidyverse)

soccer_data = read_csv("soccer_stats.csv",
                       cols(
                         date = col_date(format = ""),
                         home_team = col_character(),
                         away_team = col_character(),
                         home_score = col_integer(),
                         away_score = col_integer(),
                         tournament = col_character(),
                         city = col_character(),
                         country = col_character(),
                         neutral = col_logical()
                       ), col_names = TRUE)


minimum_date = "2015-01-01"

# Model without ties:

{
soccer_data_no_ties = soccer_data %>%
  filter(home_score != away_score, date > minimum_date) %>%
  mutate(w_loc = ifelse(neutral, 0, ifelse(home_score>away_score, 1, -1)),
         home_team = as.factor(home_team),
         away_team = as.factor(away_team),
         outcome = ifelse(home_score > away_score, 1, 0))

all_teams = union(levels(soccer_data_no_ties$home_team), levels(soccer_data_no_ties$away_team))

soccer_data_no_ties = soccer_data_no_ties %>%
  mutate(home_team = factor(home_team, levels=all_teams),
         away_team = factor(away_team, levels=all_teams)) %>%
  select(home_team, away_team, outcome)

X_design = model.matrix(data = soccer_data_no_ties, ~ home_team,
                        contrasts.arg=list(home_team=contrasts(soccer_data_no_ties$home_team, contrasts=F))) -
  model.matrix(data = soccer_data_no_ties, ~ away_team,
               contrasts.arg=list(away_team=contrasts(soccer_data_no_ties$away_team, contrasts=F)))

X_design = cbind(X_design[,-2], soccer_data_no_ties$outcome)
X_design[,1] = 1
colnames(X_design) <- c("home_adv", levels(soccer_data_no_ties$home_team)[-1], "game_outcome")
design_data_no_ties = as.data.frame(X_design)


# model = glm.fit(X_design, soccer_data_no_ties$outcome, family = binomial())
model_no_ties = glm(data = design_data_no_ties, game_outcome ~ . -1, family = binomial())
skill_coef_no_ties = model_no_ties$coefficients[order(model_no_ties$coefficients, decreasing = TRUE)]
summary(model_no_ties)
skill_coef_no_ties
}


# Model with ties - Coin flip for ties:

{
  soccer_data_ties_coin_flip = soccer_data %>%
    filter(date > minimum_date) %>%
    mutate(w_loc = ifelse(neutral, 0, ifelse(home_score>away_score, 1, -1)),
           home_team = as.factor(home_team),
           away_team = as.factor(away_team),
           outcome = ifelse(home_score == away_score, round(runif(n=nrow(soccer_data_ties_coin_flip))),
                            ifelse(home_score > away_score, 1, 0)))
  
  all_teams = union(levels(soccer_data_ties_coin_flip$home_team), levels(soccer_data_ties_coin_flip$away_team))
  
  soccer_data_ties_coin_flip = soccer_data_ties_coin_flip %>%
    mutate(home_team = factor(home_team, levels=all_teams),
           away_team = factor(away_team, levels=all_teams)) %>%
    select(home_team, away_team, outcome)
  
  X_design = model.matrix(data = soccer_data_ties_coin_flip, ~ home_team,
                          contrasts.arg=list(home_team=contrasts(soccer_data_ties_coin_flip$home_team, contrasts=F))) -
    model.matrix(data = soccer_data_ties_coin_flip, ~ away_team,
                 contrasts.arg=list(away_team=contrasts(soccer_data_ties_coin_flip$away_team, contrasts=F)))
  
  X_design = cbind(X_design[,-2], soccer_data_ties_coin_flip$outcome)
  X_design[,1] = 1
  colnames(X_design) <- c("home_adv", levels(soccer_data_ties_coin_flip$home_team)[-1], "game_outcome")
  design_data_ties_coin_flip = as.data.frame(X_design)
  
  
  # model = glm.fit(X_design, soccer_data_no_ties$outcome, family = binomial())
  model_ties_coin_flip = glm(data = design_data_ties_coin_flip, game_outcome ~ . -1, family = binomial())
  skill_coef_coin_flip = model_ties_coin_flip$coefficients[order(model_ties_coin_flip$coefficients, decreasing = TRUE)]
  summary(model_ties_coin_flip)
  skill_coef_coin_flip
}


# With ties:
# Multinomial regression using Poisson trick:
soccer_data_after_2000 = soccer_data %>% filter(date >= "2016-01-01")

soccer_data_with_ties = soccer_data %>%
  filter(date > "2016-01-01") %>%
  group_by(home_team) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  group_by(away_team) %>%
  filter(n() >= 3) %>%
  ungroup() %>%
  filter(home_team %in% away_team) %>%
  filter(away_team %in% home_team)

all_teams = union(unique(soccer_data_after_2000$home_team), unique(soccer_data_after_2000$away_team))

soccer_data_after_2000 = soccer_data_after_2000 %>%
  rowwise() %>%
  mutate("home_away" = paste0(home_team,"_",away_team))

soccer_data_after_2000_grouped = soccer_data_after_2000 %>%
  group_by(home_away) %>%
  summarise(
    "home_wins" = sum(home_score > away_score),
    "away_wins" = sum(home_score < away_score),
    "ties" = sum(home_score == away_score)
  ) %>%
  ungroup() %>%
  separate(home_away, sep = "_", remove = FALSE, into = c("home","away")) %>%
  mutate(home = factor(home, levels = all_teams),
         away = factor(away, levels = all_teams))

soccer_data_after_2000_poiss = data.frame("home_away" = rep(soccer_data_after_2000_grouped$home_away,3), 
                  "n_games" = c(soccer_data_after_2000_grouped$home_wins, soccer_data_after_2000_grouped$ties, 
                                soccer_data_after_2000_grouped$away_wins),
                  "result_home" = c(rep("w", nrow(soccer_data_after_2000_grouped)), 
                                    rep("t", nrow(soccer_data_after_2000_grouped)), 
                                    rep("l", nrow(soccer_data_after_2000_grouped)))) %>% 
  arrange(home_away) %>% 
  separate(home_away, sep = "_", remove = FALSE, into = c("home","away")) %>%
  mutate(home = factor(home, levels = all_teams),
         away = factor(away, levels = all_teams))

aij_matrix = model.matrix(data = temp, ~ home_away)
y_obs = temp$n_games
ai_matrix = model.matrix(data = temp, ~home)*rep(c(1,0,0.5), nrow(soccer_data_after_2000_poiss))
aj_matrix = model.matrix(data = temp, ~away)*rep(c(0,1,0.5), nrow(soccer_data_after_2000_poiss))

design_matrix = cbind(ai_matrix + aj_matrix, -aij_matrix)
design_matrix[,1] = 1
design_matrix[,1] = design_matrix[,1]*c(0,0,1) 

model = glm.fit(x = design_matrix, y = y_obs, family = poisson())


# With ties:


# soccer_data_with_ties = soccer_data %>%
#   filter(date > "2016-01-01") %>%
#   group_by(home_team) %>%
#   filter(n() >= 3) %>%
#   ungroup() %>%
#   group_by(away_team) %>%
#   filter(n() >= 3) %>%
#   ungroup() %>%
#   filter(home_team %in% away_team) %>%
#   filter(away_team %in% home_team)


loglik = function(team_skill_vec, data_games) {
  team_skill_vec[["Afghanistan"]] = 1 # Use first team as reference
  total_llik = 0
  delta = team_skill_vec[["home_adv"]]
  data_games = data_games %>%
    mutate(log_p_home_win = team_skill_vec[[home]] - log(exp(team_skill_vec[[home]]) + 
                                                      exp(team_skill_vec[[away]]) + 
                                                      exp(delta)*exp(0.5*(team_skill_vec[[home]]+
                                                                            team_skill_vec[[away]]))),
           log_p_away_win = team_skill_vec[[away]] - log(exp(team_skill_vec[[home]]) + 
                                                      exp(team_skill_vec[[away]]) + 
                                                      exp(delta)*exp(0.5*(team_skill_vec[[home]]+
                                                                            team_skill_vec[[away]]))),
           log_p_tie = (delta+0.5*(team_skill_vec[[home]]+team_skill_vec[[away]]) - 
                      log(exp(team_skill_vec[[home]]) + exp(team_skill_vec[[away]]) + 
                             exp(delta)*exp(0.5*(team_skill_vec[[home]] + team_skill_vec[[away]])))),
           partial_log_lik = home_wins*log_p_home_win+away_wins*log_p_away_win+ties*log_p_tie) 
  
  return(sum(data_games$partial_log_lik))
}

# Gotta dummify the teams. We'll use Albania as reference (alpha_1 = 0)

num_teams = length(levels(soccer_data_after_2000_poiss[['home']]))
team_skill_vec = rep(0, num_teams+1)  # num_teams + delta , no home adv
names(team_skill_vec) = c("home_adv", levels(soccer_data_after_2000_poiss[['home']]))

ptm = proc.time()
result = optim(team_skill_vec, loglik, 
               data_games=soccer_data_after_2000_grouped,
               method='BFGS', 
               control=list('fnscale'=-1))
toc = proc.time() - ptm

team_rankings = data.frame(teams, strength = result$par[-1]) %>%
  arrange(desc(strength))

print(team_rankings)

