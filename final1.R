data <- read.csv("C:/Users/Worksatation/Downloads/ROBO2.csv")
data$dates <- as.Date(data$dates,format='%Y-%m-%d')
returns <- data$today/100
#question1
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

#question2
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

#question3(i)
summary(data[-1])

library(ggplot2)
library(zoo)
library(dplyr)
#boxplot of returns
data$years <- format(as.Date(data$dates,format="%Y-%m-%d"),"%Y")
ggplot(data,aes(x=years,y=today))+
  geom_boxplot(fill="blue",color="black")+
  ggtitle("Boxplot of Daily Returns")+
  xlab("Year")+
  ylab("Returns")
#scatter plot of risk and return
plot(data$hvt30d,data$today,xlab = "Volatility",
     ylab = "Return",main = "Risk-Return")
#plot of Moving averages and price
df <- data %>%
  mutate(sma_50 = zoo::rollmean(price, k = 50, fill = NA), 
         sma_100 = zoo::rollmean(price, k = 100, fill = NA),
         sma_200 = zoo::rollmean(price, k = 200, fill = NA))
ggplot(data = df, aes(x = dates)) +
  geom_line(aes(y = price, color = "Price"), size = 0.7) +
  geom_line(aes(y = sma_50, color = "50 day MA"), size = 0.5) +
  geom_line(aes(y = sma_100, color = "100 day MA"), size = 0.5) +
  geom_line(aes(y = sma_200, color = "200 day MA"), size = 0.5) +
  labs(title = "Price with Moving Averages", x = "Date", y = "Price") +
  scale_color_manual(values = c("Price" = "black", "50 day MA" = "red",
                                "100 day MA" = "green", "200 day MA" = "blue"))
#plot to compare yesterday's returns and volume
data$Type <- "Volume"
data$Value <- data$volume.yesterday

data_returns <- data
data_returns$Type <- "Return"
data_returns$Value <- data$yesterday

combined_data <- rbind(data, data_returns)

ggplot(combined_data, aes(x = dates, y = Value)) +
  geom_bar(data = subset(combined_data, Type == "Volume"), stat = "identity", 
           aes(fill = "red")) +
  geom_line(data = subset(combined_data, Type == "Return"), 
            aes(color = "black"), size = 1) +
  labs(title = "Daily returns and volume", x = "Date", y = "Values") +
  facet_grid(Type ~ ., scales = "free_y")
