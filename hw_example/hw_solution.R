library(tidyverse)

table = read.csv('hw_example/NBA_record.csv')
teams = read.csv('hw_example/teams.txt', header=FALSE, as.is=TRUE) 
num_games = nrow(table)
num_teams = nrow(teams)

loglik = function(theta, Home, Away, Y) {
  alpha = theta[1]
  beta = c(0, theta[-1])
  params = alpha + beta[Home] - beta[Away] 
  return(sum(Y * params - log(1 + exp(params))))
}

theta0 = rep(0, num_teams) 
result = optim(theta0, loglik, 
               Home=table$Home, 
               Away=table$Away, 
               Y=table$Y,
               method='BFGS', 
               control=list('fnscale'=-1))

coefs = c(0, result$par[-1])
ranking = order(coefs, decreasing=TRUE) 
data.frame(team=teams[ranking[1:30],], score=coefs[ranking[1:30]])

# home advantage:
result$par[1]

# No home advantage:
loglik_noalpha = function(theta, Home, Away, Y) { 
  beta = c(0, theta)
  params = beta[Home] - beta[Away]
  return(sum(Y * params - log(1 + exp(params))))
}
theta0 = rep(0, num_teams - 1)
result_noalpha = optim(theta0, loglik_noalpha,
                       Home=table$Home,
                       Away=table$Away,
                       Y=table$Y, 
                       method='BFGS', 
                       control=list('fnscale'=-1))

# generalized likelihood ratio test, using a x2 cutoff with 1 degree of freedom.

statistic = -2 * (result_noalpha$value - result$value) 
p_value = 1 - pchisq(statistic, df=1)
print(statistic)
print(p_value)

# Logistic regression:

# Build design matrix. Home = 1, away = -1
X = matrix(0, nrow=num_games, ncol=num_teams) 

for (m in 1:num_games) {
  X[m, 1] = 1
  home = table$Home[m] 
  if (home != 1) X[m, home] = 1 
  away = table$Away[m] 
  if (away != 1) X[m, away] = -1
}

model = glm.fit(X, table$Y, family=binomial()) 

data.frame(label=c("intercept", teams[-1,1]), optim=result$par, glm=model$coefficients)


table_fac = table %>%
  mutate(Home = as.factor(teams[Home,1]),
         Away = as.factor(teams[Away,1]))


X_2 = model.matrix(~ Home, table_fac)
X_2a = model.matrix(~ Away, table_fac)
X = X_2 - X_2a
X[1,] = 1


model_3 = glm(table$Y ~ X, family = binomial)

model_3$coefficients
model$coefficients
