#***********************************************
# Alicia Nobles
# State ARIMA models for IUD searches
# Created: 11/08/17
# Mod: Added analysis for election outcome on 11/9/17
#***********************************************

library(forecast)
library(ggplot2)


# load data
setwd('~/Documents/GoogleTrend/Data/ProcessedData/States')
df_states <- read.csv('state_iud.csv')

# check for duplicate entries
df_states <- df_states[!duplicated(df_states$timestamp),] # no dups
# drop last observation
df_states = df_states[-(nrow(df_states)),]

# remove any states with only zeros - could automate this by saving to a list
for(i in names(df_states)){
  print(i)
  print(all(df_states[,i] == 0))
}
# AK, AR, DE, HI, ME, MT, ND, NH, NM, RI, SD, VT, WV, WY
df_states <- subset(df_states, select=-c(timestamp, AK, AR, DE, HI, ME, MT, ND, NH, NM, RI, SD, VT, WV, WY))

output_states  <- NULL

print.ests <- function(x, r=2) print(paste0(round(x[1],r)*100,"% (95% CI: ", round(x[2],r)*100, "-", round(x[3],r)*100, ")"))

for(i in names(df_states)){
  print(i)
  
  # create univariate time series
  qfts_state <- ts(df_states[,i], frequency = 12, start = c(2004, 1))
  
  # create training (pre-election) and test (post-election) sets
  training_state <- window(qfts_state, end = c(2016, 10))
  test_state <- window(qfts_state, start = c(2016, 11))
  
  # fit training data
  fit_state <- auto.arima(training_state)
  
  # check residuals
  checkresiduals(fit_state, plot = TRUE)
  tsdisplay(residuals(fit_state), lag.max = 12)
  
  # forecast and save
  preds_state <- forecast(fit_state, length(test_state))
  
  # calculate simple bootstrap confidence intervals 
  B <- replicate(1000, expr={
    # take a random sample of size length(test) with replacement
    ix <- sample(1:length(test_state), length(test_state), replace=T)
    # calculate percentage difference
    (sum(test_state[ix])-sum(preds_state$mean[ix]))/sum(preds_state$mean[ix])
  })
  
  # mean and CIs for difference between forecast & observed
  temp_out <- print.ests(c((sum(test_state)-sum(preds_state$mean))/sum(preds_state$mean), quantile(B, c(0.025, 0.975))))
  output_states <- rbind(output_states, temp_out)
}

output_states <- as.data.frame(output_states[,1])
output_states$state <- names(df_states)
setwd("~/Documents/GoogleTrend/Data/ProcessedData/States/State_Difference_ROutput")
write.csv(output_states, file = "output_diff_states.csv", fileEncoding = "UTF-8", row.names = FALSE)


#*************************************
# Election Outcome
#*************************************

# load data
setwd('~/Documents/GoogleTrend/Data/ProcessedData/States/State_Election_Outcome')
outcome <- read.csv('state_diff_election_outcome.csv')

# subset clinton & trump
trump <- subset(outcome, vote == 'trump')
clinton <- subset(outcome, vote == 'clinton')

# is my data normal?
# look at density plot
plot(density(trump$mean))
plot(density(clinton$mean))

# shapiro test
shapiro.test(trump$mean) # normal
shapiro.test(clinton$mean) # normal

# qq-plots
qqnorm(trump$mean);qqline(trump$mean, col = 2)
qqnorm(clinton$mean);qqline(clinton$mean, col = 2)

# median
mean(trump$mean)
# 14.4
mean(clinton$mean)
# 13.4

# t test
t.test(trump$mean,clinton$mean)

library(boot)
bs.mean <- function(x, i)
{
  return (mean(x[i]))
}
boot.out <- boot(trump$mean, bs.mean, R=1000)
boot.ci(boot.out, 0.95, type = c("norm", "basic", "perc", "bca"))

# using normal CIs because normal distribution...
# clinton: mean = 13.4 95%CI = [10.8, 16.1]
# trump: mean = 14.4 95%CI = [12.7, 16.1]
