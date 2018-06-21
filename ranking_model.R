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


# Model without ties:

# soccer_data_no_ties = soccer_data %>%
#   filter(home_score != away_score, date > "2015-01-01") %>%
#   mutate(w_loc = ifelse(neutral, 0, ifelse(home_score>away_score, 1, -1)),
#          home_team = as.factor(home_team),
#          away_team = as.factor(away_team),
#          outcome = ifelse(home_score > away_score, 1, 0))
# 
# all_teams = union(levels(soccer_data_no_ties$home_team), levels(soccer_data_no_ties$away_team))
# 
# soccer_data_no_ties = soccer_data_no_ties %>%
#   mutate(home_team = factor(home_team, levels=all_teams),
#          away_team = factor(away_team, levels=all_teams)) %>%
#   select(home_team, away_team, outcome)
# 
# X_design = model.matrix(data = soccer_data_no_ties, ~ home_team, 
#                         contrasts.arg=list(home_team=contrasts(soccer_data_no_ties$home_team, contrasts=F))) -
#   model.matrix(data = soccer_data_no_ties, ~ away_team, 
#                contrasts.arg=list(away_team=contrasts(soccer_data_no_ties$away_team, contrasts=F)))
# 
# X_design = cbind(X_design[,-2], soccer_data_no_ties$outcome)
# X_design[,1] = 1
# colnames(X_design) <- c("home_adv", levels(soccer_data_no_ties$home_team)[-1], "game_outcome")
# design_data_no_ties = as.data.frame(X_design)
# 
# # model = glm.fit(X_design, soccer_data_no_ties$outcome, family = binomial())
# model = glm(data = design_data_no_ties, game_outcome ~ . -1, family = binomial())
# skill_coef = model$coefficients[order(model$coefficients, decreasing = TRUE)]
# summary(model)
# skill_coef

# With ties:
# Multinomial regression using Poisson trick:
soccer_data_after_2000 = soccer_data %>% filter(date >= "2015-01-01")

all_teams = union(unique(soccer_data_after_2000$home_team), unique(soccer_data_after_2000$away_team))

soccer_data_after_2000 = soccer_data_after_2000 %>%
  rowwise() %>%
  mutate("home_away" = paste0(home_team,"_",away_team))

soccer_data_after_2000_poiss = soccer_data_after_2000 %>%
  group_by(home_away) %>%
  summarise(
    "home_wins" = sum(home_score > away_score),
    "away_wins" = sum(home_score < away_score),
    "ties" = sum(home_score == away_score)
  ) %>%
  ungroup() 

soccer_data_after_2000_poiss = data.frame("home_away" = rep(soccer_data_after_2000_poiss$home_away,3), 
                  "n_games" = c(soccer_data_after_2000_poiss$home_wins, soccer_data_after_2000_poiss$ties, 
                                soccer_data_after_2000_poiss$away_wins),
                  "result_home" = c(rep("w", length(soccer_data_after_2000_poiss$home_away)), 
                                    rep("t", length(soccer_data_after_2000_poiss$home_away)), 
                                    rep("l", length(soccer_data_after_2000_poiss$home_away)))) %>% 
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
  for (row in 1:nrow(data_games)) {
    partial_llik = 0
    outcome = data_games[row, "result_home"]
    alpha_i = team_skill_vec[[data_games[row, "home"]]]
    alpha_j = team_skill_vec[[data_games[row, "away"]]]
    delta = team_skill_vec[["home_adv"]]
    Aij = log(exp(alpha_i) + exp(alpha_j) + exp(delta)*exp(0.5*(alpha_i+alpha_j)))
    log_pij1 = alpha_i - Aij
    log_pij2 = alpha_j - Aij
    log_pij3 = delta+0.5*(alpha_i+alpha_j) - Aij
    if (outcome == "t") {
      partial_llik = log_pij3
    } else if (outcome == "w") {
      partial_llik = log_pij1
    } else {
      partial_llik = log_pij2
    }
    total_llik = total_llik + data_games[row, "n_games"]*partial_llik
  }
  return(total_llik)
}

# Gotta dummify the teams. We'll use Albania as reference (alpha_1 = 0)

num_teams = length(levels(soccer_data_after_2000_poiss[['home']]))
team_skill_vec = rep(0, num_teams+1)  # num_teams + delta , no home adv
names(team_skill_vec) = c("home_adv", levels(soccer_data_after_2000_poiss[['home']]))

result = optim(team_skill_vec, loglik, 
               data_games=soccer_data_after_2000_poiss,
               method='BFGS', 
               control=list('fnscale'=-1))

team_rankings = data.frame(teams, strength = result$par[-1]) %>%
  arrange(desc(strength))

print(team_rankings)

