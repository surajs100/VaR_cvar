data <- read.csv("C:/Users/Worksatation/Downloads/ROBO2.csv")
data$dates <- as.Date(data$dates,format='%Y-%m-%d')
returns <- data$today/100
h_var <- function(returns, significance) {
  return(quantile(returns, significance))
}
p_var <- function(returns, significance) {
  mean_ret <- mean(returns)
  sd_ret <- sd(returns)
  z_score <- qnorm(significance)
  return(mean_ret + sd_ret * z_score)
}
mc_var <- function(returns, significance, simulations) {
  set.seed(123)
  mean_ret <- mean(returns)
  sd_ret <- sd(returns)
  simulated_returns <- rnorm(simulations,mean_ret,sd_ret)
  return(quantile(simulated_returns,significance))
}
var_historical <- h_var(returns, 0.05)
var_parametric <- p_var(returns, 0.05)
var_mc <- mc_var(returns,0.05,100000)

print(paste("Historical VaR: ", var_historical))
print(paste("Parametric VaR: ", var_parametric))
print(paste("Monte Carlo VaR: ", var_mc))


h_cvar <- function(returns, significance) {
  var <- quantile(returns, significance)
  return(mean(returns[returns < var]))
}
p_cvar <- function(returns, significance) {
  mean_ret <- mean(returns)
  sd_ret <- sd(returns)
  z_score <- qnorm(significance)
  density_function_z_score <- dnorm(z_score)
  return(mean_ret - (density_function_z_score / significance) * sd_ret)
}
mc_cvar <- function(returns, significance, simulations) {
  set.seed(123)
  mean_ret <- mean(returns)
  sd_ret <- sd(returns)
  simulated_returns <- rnorm(simulations,mean_ret,sd_ret)
  simulated_var <- quantile(simulated_returns,significance)
  return(mean(simulated_returns[as.list(simulated_returns) < simulated_var]))
}
cvar_historical <- h_cvar(returns, 0.05)
cvar_parametric <- p_cvar(returns, 0.05)
cvar_mc <- mc_cvar(returns,0.05,100000)

print(paste("Historical cVaR: ", cvar_historical))
print(paste("Parametric cVaR: ", cvar_parametric))
print(paste("Monte Carlo cVaR: ", cvar_mc))
