# Bradley Terry model for International Soccer

library(tidyverse)

soccer_data = read_csv("results.csv",
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

soccer_data_no_ties = soccer_data %>%
  filter(home_score != away_score, date > "2015-01-01") %>%
  mutate(w_loc = ifelse(neutral, 0, ifelse(home_score>away_score, 1, -1))) 

soccer_data_no_ties = soccer_data_no_ties %>% 
  group_by(home_team) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  group_by(away_team) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  filter(home_team %in% away_team) %>%
  filter(away_team %in% home_team) %>% 
  mutate(outcome = ifelse(home_score > away_score, 1, 0))

soccer_data_no_ties = soccer_data_no_ties %>%
  mutate(outcome = ifelse(home_score > away_score, 1, 0),
         home_team = as.factor(home_team),
         away_team = as.factor(away_team)) %>%
  select(home_team, away_team, outcome)

X_design = model.matrix(data = soccer_data_no_ties, ~ home_team) - 
  model.matrix(data = soccer_data_no_ties, ~ away_team)

X_design[,1] = 1
colnames(X_design) <- c("home_Adv", as.character(sort(unique(soccer_data_no_ties$home_team))[-1]))


model = glm.fit(X_design, soccer_data_no_ties$outcome, family = binomial())
res = model$coefficients[order(model$coefficients, decreasing = TRUE)]

res

# With ties:

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

soccer_data_with_ties = soccer_data_with_ties  %>% 
  mutate(outcome = ifelse(home_score == away_score, "tie", 
                          ifelse(home_score > away_score, "win", "loss")))

teams = sort(unique(soccer_data_with_ties$home_team))
soccer_data_with_ties = soccer_data_with_ties %>% 
  rowwise %>%
  mutate(home_team_index = which(teams == home_team),
         away_team_index = which(teams == away_team))

loglik = function(theta, Home, Away, Y) {
  total_log_lik = 0
  for (row_n in seq(1,length(Y))) {
    outcome = Y[row_n]
    alpha_i = theta[Home[row_n]+1]
    alpha_j = theta[Away[row_n]+1]
    theta[2] = 1 # Use first team as reference
    delta = theta[1]
    Aij = log(exp(alpha_i) + exp(alpha_j) + exp(delta)*exp(0.5*(alpha_i+alpha_j)))
    log_pij1 = alpha_i - Aij
    log_pij2 = alpha_j - Aij
    log_pij3 = delta+0.5*(alpha_i+alpha_j) - Aij
    part_lik = 0
    if (outcome == "tie") {
      part_lik = log_pij3
    } else if (outcome == "win") {
      part_lik = log_pij1
    } else {
      part_lik = log_pij2
    }
    total_log_lik = total_log_lik + part_lik
  }
  return(total_log_lik)
}


# Gotta dummify the teams. We'll use Albania as reference (alpha_1 = 0)

num_teams = length(unique(soccer_data_with_ties$home_team))
theta0 = rep(0, num_teams+1)  # num_teams + delta , no home adv

result = optim(theta0, loglik, 
               Home=soccer_data_with_ties$home_team_index, 
               Away=soccer_data_with_ties$away_team_index, 
               Y=soccer_data_with_ties$outcome,
               method='BFGS', 
               control=list('fnscale'=-1))

team_rankings = data.frame(teams, strength = result$par[-1]) %>%
  arrange(desc(strength))

print(team_rankings)

# Miltinomial regression using Poisson trick:

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

require(prefmod)

countries_SA = c("Argentina", 
                 "Brasil", 
                 "Colombia", 
                 "Peru", 
                 "Chile", 
                 "Ecuador",
                 "Bolivia",
                 "Venezuela",
                 "Uruguay",
                 "Paraguay")

soccer_south_america = soccer_data_with_ties %>% 
  filter(home_team %in% countries_SA & away_team %in% countries_SA)
  
soccer_south_america%>% View()

des=llbt.design(soccer_south_america, nitems=10)


# baseball example (Agresti, 2002, p. 437)

# pseudodata for generating a design matrix
d1 <- c(rep(0, 21), 1)
d2 <- c(1, rep(0, 20), 2)
d  <- data.frame(rbind(d1, d2))
names(d) <- c(paste0("v", 1:21), "cov")

# design matrix
des5 <- llbt.design(d, nitems = 7,
                    objnames = c("MIL", "DET", "TOR", "NY", "BOS", "CLE", "BAL"),
                    cat.scov = "cov")
des5$y  <- baseball
des5$mu <- gl(42, 2)
dpos     <- c(rep(1:0, 21), rep(0:1, 21))

# fit model and display results
res5 <- gnm(y ~ MIL+DET+TOR+NY+BOS+CLE+BAL + pos,
            eliminate = mu, data = des5, family = poisson)
w5   <- llbt.worth(res5)
plot(w5)


