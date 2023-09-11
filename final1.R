data <- read.csv("C:/Users/Worksatation/Downloads/ROBO2.csv")
data$dates <- as.Date(data$dates,format='%Y-%m-%d')
returns <- data$today/100
#Function to get VaR using Historic Method
h_var <- function(returns, significance) {
  return(quantile(returns, significance))
}
#Function to get VaR using Parametric Method
p_var <- function(returns, significance) {
  mean_ret <- mean(returns)
  sd_ret <- sd(returns)
  z_score <- qnorm(significance)
  return(mean_ret + sd_ret * z_score)
}
#Function to get VaR using Monte Carlo Simulation
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

#Function to get CVaR using Historic Method
h_cvar <- function(returns, significance) {
  var <- quantile(returns, significance)
  return(mean(returns[returns < var]))
}
#Function to get CVaR using Parametric Method
p_cvar <- function(returns, significance) {
  mean_ret <- mean(returns)
  sd_ret <- sd(returns)
  z_score <- qnorm(significance)
  density_function_z_score <- dnorm(z_score)
  return(mean_ret - (density_function_z_score / significance) * sd_ret)
}
#Function to get CVaR using Monte Carlo Simulation
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

#This will give the summary analysis of data excluding the dates
summary(data[-1])

library(ggplot2)
library(zoo)
library(dplyr)
#Calculating the Moving averages
df <- data %>%
  mutate(sma_50 = zoo::rollmean(price, k = 50, fill = NA), 
         sma_100 = zoo::rollmean(price, k = 100, fill = NA),
         sma_200 = zoo::rollmean(price, k = 200, fill = NA))
#Plotting the MA and Prices
ggplot(data = df, aes(x = dates)) +
  geom_line(aes(y = price, color = "Price"), size = 0.7) +
  geom_line(aes(y = sma_50, color = "50 day MA"), size = 0.5) +
  geom_line(aes(y = sma_100, color = "100 day MA"), size = 0.5) +
  geom_line(aes(y = sma_200, color = "200 day MA"), size = 0.5) +
  labs(title = "Price with Moving Averages", x = "Date", y = "Price",
       color="Legend") +
  scale_color_manual(values = c("Price" = "black", "50 day MA" = "red",
                                "100 day MA" = "green", "200 day MA" = "blue"))

#Box plot of Daily Returns
data$years <- format(as.Date(data$dates,format="%Y-%m-%d"),"%Y")
ggplot(data,aes(x=years,y=today))+
  geom_boxplot(fill="blue",color="black")+
  ggtitle("Boxplot of Daily Returns")+
  xlab("Year")+
  ylab("Returns")

#Risk-Reward scatter plot
plot(data$hvt30d,data$today,xlab = "Volatility",ylab = "Return",
     main = "Risk-Return")

#Plot to compare daily returns with traded volume
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

#Calculating the monthly rolling VaR and CVaR using the above created 
#Parametric Functions of VaR and CVaR

# Setting window width for 2 years (approx. 503 trading days)
window_width <- 24*21-1

# Initialize storage for results
rolling_var <- numeric(nrow(data) - window_width + 1)
rolling_cvar <- numeric(nrow(data) - window_width + 1)
dates <- character(nrow(data) - window_width + 1)

# Monthly rolling loop
i <- 1
j <- 1
while (i <= (nrow(data) - window_width + 1)) {
  window_data <- data[i:(i + window_width - 1), ]
  window_returns <- window_data$today/100
  rolling_var[j] <- p_var(window_returns, 0.05)
  rolling_cvar[j] <- p_cvar(window_returns, 0.05)
  dates[j] <- as.character(window_data[nrow(window_data), "dates"])
  
  # Find the next month's start
  current_month <- as.POSIXlt(window_data[1, "dates"])$mon
  next_month_positions <- which(as.POSIXlt(data$dates[(i+1):nrow(data)])$
                                  mon != current_month)
  
  # If we can't find the next month in the remaining data, break the loop
  if (length(next_month_positions) == 0) {
    break
  }
  
  i <- i + next_month_positions[1]
  j <- j + 1
}

# Trim the resulting vectors
rolling_var <- rolling_var[1:j-1]
rolling_cvar <- rolling_cvar[1:j-1]
dates <- dates[1:j-1]

# The final results are stored in rolling_var, rolling_cvar, and dates vectors
roll_df=data.frame(date = as.Date(dates,format = "%Y-%m-%d"),
                   VaR= as.matrix(rolling_var),
                   CVaR= as.matrix(rolling_cvar))
#internal check
print(head(roll_df))
print(tail(roll_df))
#Plotting Rolling VaR and CVaR
ggplot(data = roll_df, aes(x = date)) +
  geom_line(aes(y = VaR, color = "VaR"), size = 0.7) +
  geom_line(aes(y = CVaR, color = "CVaR"), size = 0.7)+
  labs(title = "Rolling VaR and CVaR", x = "Date", y = "Price",color="Legend")+
  scale_color_manual(values = c("VaR" = "green", "CVaR" = "red"))

print(roll_df)
