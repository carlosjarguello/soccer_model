# Bradley Terry model for International Soccer

library(tidyverse)

soccer_data = read_csv("soccer_stats_0623.csv",
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
                       ), col_names = TRUE) %>%
  filter(date <= "2018-06-17")

minimum_date = "2016-01-01"

# Model without ties:

{
soccer_data_no_ties = soccer_data %>%
  filter(home_score != away_score, date > minimum_date) %>%
  mutate(w_loc = ifelse(neutral, 0, ifelse(home_score>away_score, 1, -1)),
         home_team = as.factor(home_team),
         away_team = as.factor(away_team),
         outcome = ifelse(home_score > away_score, 1, 0),
         tournament_value = ifelse(tournament=="Friendly", 0.3, 1)) %>%
    arrange(date)

all_teams = union(levels(soccer_data_no_ties$home_team), levels(soccer_data_no_ties$away_team))

soccer_data_no_ties = soccer_data_no_ties %>%
  mutate(home_team = factor(home_team, levels=all_teams),
         away_team = factor(away_team, levels=all_teams)) %>%
  select(date, home_team, away_team, outcome, tournament_value)

X_design = model.matrix(data = soccer_data_no_ties, ~ home_team,
                        contrasts.arg=list(home_team=contrasts(soccer_data_no_ties$home_team, contrasts=F))) -
  model.matrix(data = soccer_data_no_ties, ~ away_team,
               contrasts.arg=list(away_team=contrasts(soccer_data_no_ties$away_team, contrasts=F)))

X_design = cbind(X_design[,-2], soccer_data_no_ties$outcome)
X_design[,1] = 1
colnames(X_design) <- c("home_adv", levels(soccer_data_no_ties$home_team)[-1], "game_outcome")
design_data_no_ties = as.data.frame(X_design)

decay_constant = 0.5*1/365
weights_time = exp(-decay_constant*as.numeric(tail(soccer_data_no_ties$date,1)-
                                                soccer_data_no_ties$date))*soccer_data_no_ties$tournament_value

# model = glm.fit(X_design, soccer_data_no_ties$outcome, family = binomial())
model_no_ties = glm(data = design_data_no_ties, game_outcome ~ . -1, family = binomial(), weights = weights_time)
skill_coef_no_ties = model_no_ties$coefficients[order(model_no_ties$coefficients, decreasing = TRUE)]
summary(model_no_ties)
skill_coef_no_ties
}

# Model with ties - Coin flip for ties - (should execute several cycles and average?):

{
  soccer_data_ties_coin_flip = soccer_data %>%
    filter(date > minimum_date) 
  
  soccer_data_ties_coin_flip = soccer_data_ties_coin_flip %>%
    mutate(w_loc = ifelse(neutral, 0, ifelse(home_score>away_score, 1, -1)),
           home_team = as.factor(home_team),
           away_team = as.factor(away_team),
           outcome = ifelse(home_score == away_score, round(runif(n=nrow(soccer_data_ties_coin_flip))),
                            ifelse(home_score > away_score, 1, 0)),
           tournament_value = ifelse(tournament=="Friendly", 0.5, 1)) %>%
    arrange(date)
  
  all_teams = union(levels(soccer_data_ties_coin_flip$home_team), levels(soccer_data_ties_coin_flip$away_team))
  
  soccer_data_ties_coin_flip = soccer_data_ties_coin_flip %>%
    mutate(home_team = factor(home_team, levels=all_teams),
           away_team = factor(away_team, levels=all_teams)) %>%
    select(date, home_team, away_team, outcome, tournament_value)
  
  X_design = model.matrix(data = soccer_data_ties_coin_flip, ~ home_team,
                          contrasts.arg=list(home_team=contrasts(soccer_data_ties_coin_flip$home_team, contrasts=F))) -
    model.matrix(data = soccer_data_ties_coin_flip, ~ away_team,
                 contrasts.arg=list(away_team=contrasts(soccer_data_ties_coin_flip$away_team, contrasts=F)))
  
  X_design = cbind(X_design[,-2], soccer_data_ties_coin_flip$outcome)
  X_design[,1] = 1
  colnames(X_design) <- c("home_adv", levels(soccer_data_ties_coin_flip$home_team)[-1], "game_outcome")
  design_data_ties_coin_flip = as.data.frame(X_design)
  
  decay_constant = 0.5*1/365
  weights_time = (exp(-decay_constant*as.numeric(tail(soccer_data_ties_coin_flip$date,1)-
                                                  soccer_data_ties_coin_flip$date))*
                    soccer_data_ties_coin_flip$tournament_value)
  
  # model = glm.fit(X_design, soccer_data_no_ties$outcome, family = binomial())
  model_ties_coin_flip = glm(data = design_data_ties_coin_flip, game_outcome ~ . -1, 
                             family = binomial(), weights = weights_time)
  skill_coef_coin_flip = model_ties_coin_flip$coefficients[order(model_ties_coin_flip$coefficients, decreasing = TRUE)]
  summary(model_ties_coin_flip)
  skill_coef_coin_flip
}

# Model with ties - Split each tie in two, with one game going to each team:

{
  soccer_data_ties_split = soccer_data %>%
    filter(date > minimum_date) 
  
  soccer_data_ties_split_no_ties = soccer_data_ties_split %>%
    mutate(w_loc = ifelse(neutral, 0, ifelse(home_score>away_score, 1, -1)),
           home_team = as.factor(home_team),
           away_team = as.factor(away_team)) %>%
    filter(home_score != away_score) %>%
    mutate(outcome = ifelse(home_score > away_score, 1, 0), 
           tournament_value = ifelse(tournament=="Friendly", 0.3, 1)) %>%
    arrange(date)
  
  soccer_data_ties_split_only_ties =  soccer_data_ties_split %>%
    mutate(w_loc = ifelse(neutral, 0, ifelse(home_score>away_score, 1, -1)),
           home_team = as.factor(home_team),
           away_team = as.factor(away_team)) %>%
    filter(home_score == away_score) 
  
  soccer_data_ties_split = bind_rows(soccer_data_ties_split_no_ties, 
                                     soccer_data_ties_split_only_ties %>% 
                                       mutate(w_loc = ifelse(neutral, 0, 1),
                                              outcome = 1),
                                     soccer_data_ties_split_only_ties %>% 
                                       mutate(w_loc = ifelse(neutral, 0, -1),
                                              outcome = 0)) %>%
    arrange(date)
  
  all_teams = union(levels(soccer_data_ties_split$home_team), levels(soccer_data_ties_split$away_team))
  
  soccer_data_ties_split = soccer_data_ties_split %>%
    mutate(home_team = factor(home_team, levels=all_teams),
           away_team = factor(away_team, levels=all_teams)) %>%
    select(date, home_team, away_team, outcome, tournament_value)
  
  X_design = model.matrix(data = soccer_data_ties_split, ~ home_team,
                          contrasts.arg=list(home_team=contrasts(soccer_data_ties_split$home_team, contrasts=F))) -
    model.matrix(data = soccer_data_ties_split, ~ away_team,
                 contrasts.arg=list(away_team=contrasts(soccer_data_ties_split$away_team, contrasts=F)))
  
  X_design = cbind(X_design[,-2], soccer_data_ties_split$outcome)
  X_design[,1] = 1
  colnames(X_design) <- c("home_adv", levels(soccer_data_ties_split$home_team)[-1], "game_outcome")
  design_data_ties_split_ties = as.data.frame(X_design)
  
  decay_constant = 0.5*1/365
  weights_time = (exp(-decay_constant*as.numeric(tail(soccer_data_ties_split$date,1)-
                                                   soccer_data_ties_split$date))*
                    soccer_data_ties_split$tournament_value)
  
  # model = glm.fit(X_design, soccer_data_no_ties$outcome, family = binomial())
  model_ties_split_ties = glm(data = design_data_ties_split_ties, game_outcome ~ . -1, 
                              family = binomial(), weights = weights_time)
  skill_coef_split_ties = model_ties_split_ties$coefficients[order(model_ties_split_ties$coefficients, 
                                                                   decreasing = TRUE)]
  summary(model_ties_split_ties)
  model_ties_split_ties
  skill_coef_split_ties
}

# Model with ties - Trinomial MLE:

{

soccer_data_compact = soccer_data %>%
  filter(date > minimum_date)
  
all_teams = sort(union(unique(soccer_data_compact$home_team), unique(soccer_data_compact$away_team)))

soccer_data_compact = soccer_data_compact %>%
  rowwise() %>%
  mutate("home_away" = paste0(home_team,"_",away_team)) %>%
  group_by(home_away) %>%
  summarise(
    "home_wins" = sum(home_score > away_score),
    "away_wins" = sum(home_score < away_score),
    "ties" = sum(home_score == away_score),
    "total_games" = home_wins+away_wins+ties
  ) %>%
  ungroup() %>%
  separate(home_away, sep = "_", remove = FALSE, into = c("home_team","away_team")) %>%
  mutate(home_team = factor(home_team, levels = all_teams),
         away_team = factor(away_team, levels = all_teams))

num_teams = length(levels(soccer_data_compact$home_team))

soccer_data_compact = soccer_data_compact %>%
  rowwise %>% 
  mutate(home_team_index = which(all_teams == home_team), 
         away_team_index = which(all_teams == away_team))

# Gotta dummify the teams. We'll use first team as reference (alpha_1 = 0)
team_skill_vec = rep(0,num_teams+2)  # num_teams + delta + home adv

loglik = function(theta, data_games) {
  total_llik = 0
  Home=data_games$home_team_index
  Away=data_games$away_team_index
  home_wins = data_games$home_wins
  away_wins = data_games$away_wins
  ties = data_games$ties
  theta[3] = 1
  delta_ties = theta[1]
  eta_home_adv = 0*theta[2]
  for (row_n in seq(1,nrow(data_games))) {
    alpha_i = theta[Home[row_n]+2]
    alpha_j = theta[Away[row_n]+2]
    Aij = log(exp(alpha_i) + exp(alpha_j+eta_home_adv) + exp(delta_ties + 0.5*(alpha_i+alpha_j+eta_home_adv))) 
    log_pij1 = alpha_i - Aij
    log_pij2 = alpha_j + eta_home_adv - Aij 
    log_pij3 = delta_ties+0.5*(alpha_i+alpha_j+eta_home_adv) - Aij 
    total_llik = total_llik + home_wins[row_n]*log_pij1 + away_wins[row_n]*log_pij2 + ties[row_n]*log_pij3
  }
  return(total_llik)
}

result = optim(team_skill_vec, loglik, 
               data_games=soccer_data_compact,
               method='BFGS', 
               control=list('fnscale'=-1))

skill_coef_with_ties = data.frame(all_teams, relative_skill = result$par[-(1:2)]) %>% arrange(desc(relative_skill))
names(skill_coef_with_ties)[1] = "Team"
print(skill_coef_with_ties)
wc_teams = read_csv("WC_teams.csv")
wc_skill = skill_coef_with_ties %>%
  right_join(wc_teams, by="Team")
wc_skill
}

# With ties:
# Multinomial regression using Poisson trick (not working)

{



soccer_data_poiss = as.tibble(data.frame("home_away" = rep(soccer_data_compact$home_away,3), 
                  "n_games" = c(soccer_data_compact$home_wins, soccer_data_compact$ties, 
                                soccer_data_compact$away_wins),
                  "result_home" = c(rep("w", nrow(soccer_data_compact)), 
                                    rep("t", nrow(soccer_data_compact)), 
                                    rep("l", nrow(soccer_data_compact)))) %>% 
  arrange(home_away) %>% 
  separate(home_away, sep = "_", remove = FALSE, into = c("home","away")) %>%
  mutate(home = factor(home, levels = all_teams),
         away = factor(away, levels = all_teams)))

aij_matrix = model.matrix(data = soccer_data_poiss, ~ home_away)
y_obs = soccer_data_poiss$n_games
ai_matrix = model.matrix(data = soccer_data_poiss, ~home)*rep(c(1,0,0.5), nrow(soccer_data_poiss))
aj_matrix = model.matrix(data = soccer_data_poiss, ~away)*rep(c(0,1,0.5), nrow(soccer_data_poiss))

design_matrix = cbind(ai_matrix + aj_matrix, -aij_matrix)
design_matrix[,1] = 1
design_matrix[,1] = design_matrix[,1]*c(0,0,1) 

model = glm.fit(x = design_matrix, y = y_obs, family = poisson())
temp = sort(model$coefficients[1:200], decreasing = TRUE)
print(temp[1:20])
print(head(skill_coef_with_ties,20))
}

