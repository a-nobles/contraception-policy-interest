#***********************************************
# Alicia Nobles
# ARIMA model for IUD searches
# Created: 09/07/17
# Last Modified: 09/24/17
# Mod: 9/24/17: Added state-level code
# Mod: 10/23/17: Added bc pills and condom analysis
# Mod: 11/6/17: Clean up & add final figs
#***********************************************

library(forecast)
library(ggplot2)

load("~/Documents/GoogleTrend/Workspace/IUD_Analysis.RData")

#************************************************
# Load and prep data
#************************************************
setwd("~/Documents/GoogleTrend/Data/ProcessedData/National")

# read monthly data
monthly <- read.csv("national.csv")

# code if needed...
# drop last observation
monthly = monthly[-(nrow(monthly)),]
# delete duplicate timestamps
monthly <- monthly[!duplicated(monthly$timestamp),]

# create univariate time series
iud_qfts <- ts(monthly$iud, frequency = 12, start = c(2004, 1))
autoplot(iud_qfts)

pill_qfts <- ts(monthly$pill, frequency = 12, start = c(2004, 1))
autoplot(pill_qfts)

condom_qfts <- ts(monthly$condom, frequency = 12, start = c(2004, 1))
autoplot(condom_qfts)

# code if needed...
# decompose time series
#decomp = decompose(qfts, type = "multiplicative")
#plot(decomp)
#plot(stl(qfts, "periodic"))
#decomp = decompose(qfts, type = "additive")
#plot(decomp)

# create training (pre-election) and test (post-election) sets
iud_training <- window(iud_qfts, end = c(2016, 10))
iud_test <- window(iud_qfts, start = c(2016, 11))
autoplot(iud_training)
autoplot(iud_test)

pill_training <- window(pill_qfts, end = c(2016, 10))
pill_test <- window(pill_qfts, start = c(2016, 11))
autoplot(pill_training)
autoplot(pill_test)

condom_training <- window(condom_qfts, end = c(2016, 10))
condom_test <- window(condom_qfts, start = c(2016, 11))
autoplot(condom_training)
autoplot(condom_test)


#************************************************
# Fit training data
#************************************************
# fit training data
iud_fit <- auto.arima(iud_training, trace=TRUE)
summary(iud_fit)
checkresiduals(iud_fit, plot = TRUE)
tsdisplay(residuals(iud_fit), lag.max = 12)

pill_fit <- auto.arima(pill_training, trace=TRUE)
summary(pill_fit)
checkresiduals(pill_fit, plot = TRUE)
tsdisplay(residuals(pill_fit), lag.max = 12)

condom_fit <- auto.arima(condom_training, trace=TRUE)
summary(condom_fit)
checkresiduals(condom_fit, plot = TRUE)
tsdisplay(residuals(condom_fit), lag.max = 12)


#************************************************
# Forecast
#************************************************
iud_preds <- forecast(iud_fit, length(iud_test))
pill_preds <- forecast(pill_fit, length(pill_test))
condom_preds <- forecast(condom_fit, length(condom_test))

# saving preds and observations to df/csv so it can be viewed later
setwd('~/Documents/GoogleTrend/Tables')
df_preds <- data.frame(iud_preds)
df_preds$observed <- monthly[155:166, 'iud']
write.csv(df_preds, 'iud_preds_obs.csv')

df_preds <- data.frame(pill_preds)
df_preds$observed <- monthly[155:166, 'pill']
write.csv(df_preds, 'pill_preds_obs.csv')

df_preds <- data.frame(condom_preds)
df_preds$observed <- monthly[155:166, 'condom']
write.csv(df_preds, 'condom_preds_obs.csv')


#************************************************
# Bootstrapped CIs of the Mean
#************************************************
# function to print estimates
print.ests <- function(x, r=2) print(paste0(round(x[1],r)*100,"% (95% CI: ", round(x[2],r)*100, "-", round(x[3],r)*100, ")"))

#-----------------------------
# IUDs
#-----------------------------
# calculate simple bootstrap confidence intervals 
B <- replicate(1000, expr={
  # take a random sample of size length(test) with replacement
  ix <- sample(1:length(iud_test), length(iud_test), replace=T)
  # calculate percentage difference
  (sum(iud_test[ix])-sum(iud_preds$mean[ix]))/sum(iud_preds$mean[ix])
})

# mean and CIs for difference between forecast & observed
print.ests(c((sum(iud_test)-sum(iud_preds$mean))/sum(iud_preds$mean), quantile(B, c(0.025, 0.975))))
# result: 15% (95% CI [10,20])

# difference in November 16
100*(iud_test[1] - iud_preds$mean[1])/iud_preds$mean[1]
100*(iud_test[1] - iud_preds$lower[1])/iud_preds$lower[1]
100*(iud_test[1] - iud_preds$upper[1])/iud_preds$upper[1]
#30% 95% Prediction Interval: [23,37]

# difference in January 17
100*(iud_test[3] - iud_preds$mean[3])/iud_preds$mean[3]
100*(iud_test[3] - iud_preds$lower[3])/iud_preds$lower[3]
100*(iud_test[3] - iud_preds$upper[3])/iud_preds$upper[3]
#20% 95% Prediction Interval: 13-28

# difference in May 17
100*(iud_test[7] - iud_preds$mean[7])/iud_preds$mean[7]
100*(iud_test[7] - iud_preds$lower[7])/iud_preds$lower[7]
100*(iud_test[7] - iud_preds$upper[7])/iud_preds$upper[7]
#31% 95% Prediction Interval: 20-43

# difference in Oct 17
100*(iud_test[12] - iud_preds$mean[12])/iud_preds$mean[12]
100*(iud_test[12] - iud_preds$lower[12])/iud_preds$lower[12]
100*(iud_test[12] - iud_preds$upper[12])/iud_preds$upper[12]
#9% 95% Prediction Interval: -1-23

#-----------------------------
# BC Pills
#-----------------------------
# calculate simple bootstrap confidence intervals 
B <- replicate(1000, expr={
  # take a random sample of size length(test) with replacement
  ix <- sample(1:length(pill_test), length(pill_test), replace=T)
  # calculate percentage difference
  (sum(pill_test[ix])-sum(pill_preds$mean[ix]))/sum(pill_preds$mean[ix])
})

# mean and CIs for difference between forecast & observed
print.ests(c((sum(pill_test)-sum(pill_preds$mean))/sum(pill_preds$mean), quantile(B, c(0.025, 0.975))))
# result: 0% (95% CI: [-2,1]

# difference in November 16
100*(pill_test[1] - pill_preds$mean[1])/pill_preds$mean[1]
100*(pill_test[1] - pill_preds$lower[1])/pill_preds$lower[1]
100*(pill_test[1] - pill_preds$upper[1])/pill_preds$upper[1]
#1% 95% Prediction Interval: [-7, 10]

# difference in January 17
100*(pill_test[3] - pill_preds$mean[3])/pill_preds$mean[3]
100*(pill_test[3] - pill_preds$lower[3])/pill_preds$lower[3]
100*(pill_test[3] - pill_preds$upper[3])/pill_preds$upper[3]
#6% 95% Prediction Interval: [-3,16]

# difference in May 17
100*(pill_test[7] - pill_preds$mean[7])/pill_preds$mean[7]
100*(pill_test[7] - pill_preds$lower[7])/pill_preds$lower[7]
100*(pill_test[7] - pill_preds$upper[7])/pill_preds$upper[7]
#0% 95% Prediction Interval: [-9, 12]

#-----------------------------
# Condoms
#-----------------------------
# calculate simple bootstrap confidence intervals 
B <- replicate(1000, expr={
  # take a random sample of size length(test) with replacement
  ix <- sample(1:length(condom_test), length(condom_test), replace=T)
  # calculate percentage difference
  (sum(condom_test[ix])-sum(condom_preds$mean[ix]))/sum(condom_preds$mean[ix])
})

# mean and CIs for difference between forecast & observed
print.ests(c((sum(condom_test)-sum(condom_preds$mean))/sum(condom_preds$mean), quantile(B, c(0.025, 0.975))))
# result: -4% (95% CI: [-5, -2]

# difference in November 16
100*(condom_test[1] - condom_preds$mean[1])/condom_preds$mean[1]
100*(condom_test[1] - condom_preds$lower[1])/condom_preds$lower[1]
100*(condom_test[1] - condom_preds$upper[1])/condom_preds$upper[1]
#-5% 95% Prediction Interval: [-13, 5]

# difference in January 17
100*(condom_test[3] - condom_preds$mean[3])/condom_preds$mean[3]
100*(condom_test[3] - condom_preds$lower[3])/condom_preds$lower[3]
100*(condom_test[3] - condom_preds$upper[3])/condom_preds$upper[3]
#-3% 95% Prediction Interval: [-14, 10]

# difference in May 17
100*(condom_test[7] - condom_preds$mean[7])/condom_preds$mean[7]
100*(condom_test[7] - condom_preds$lower[7])/condom_preds$lower[7]
100*(condom_test[7] - condom_preds$upper[7])/condom_preds$upper[7]
#-2% 95% Prediction Interval: [-15, 14]


#********************************************
# Plots: Observed vs. Predicted since 2008
#********************************************
setwd('~/Documents/GoogleTrend/Images')

# iud query fraction plot
#png('iud_diff.png')
plot(iud_preds, PI=FALSE, fcol = 'black', flwd = 1, xlim=c(2008,2018), ylim=c(0,1000), ylab="Query Fraction", main="QF: IUDs")
lines(x=iud_test)
grid()
iud_x <- time(iud_test)
iud_y_obs = monthly[155:166, 'iud']
iud_y_pred = as.numeric(iud_preds$mean)
polygon(c(iud_x,rev(iud_x)),c(iud_y_obs,rev(iud_y_pred)),col = 'grey85', border = NA)
#dev.off()

# pill query fraction plot
#png('pill_diff.png')
plot(pill_preds, PI=FALSE, fcol = 'black', flwd = 1, xlim=c(2008,2018), ylim=c(0,1000), ylab="Query Fraction", main="QF: Birth Control Pills")
lines(x=pill_test)
grid()
pill_x <- time(pill_test)
pill_y_obs = monthly[155:166, 'pill']
pill_y_pred = as.numeric(pill_preds$mean)
polygon(c(pill_x,rev(pill_x)),c(pill_y_obs,rev(pill_y_pred)),col = 'grey85', border = NA)
#dev.off()

# condom query fraction plot
#png('condom_diff.png')
plot(condom_preds, PI=FALSE, fcol = 'black', flwd = 1, xlim=c(2008,2018), ylim=c(0,1600), ylab="Query Fraction", main="QF: Condoms")
lines(x=condom_test)
grid()
condom_x <- time(condom_test)
condom_y_obs = monthly[155:166, 'condom']
condom_y_pred = as.numeric(condom_preds$mean)
polygon(c(condom_x,rev(condom_x)),c(condom_y_obs,rev(condom_y_pred)),col = 'grey85', border = NA)
#dev.off()


#************************************************
# Plots: Observed vs. Predicted since Election
#************************************************
# iud percent difference between forecast & observed
#png('iud_percent.png')
# create list of integer from 1 to number of test dates
iud_x <- 1:length(iud_test)
# create a plot for difference
plot(iud_x, 100*(iud_test-iud_preds$mean)/iud_preds$mean, type='n', ylim=c(-10,110), main='Percent Difference in RSV: IUD', ylab='Percent Change in RSV', xaxt="n")
iud_xx <- c(iud_x, rev(iud_x))
# find difference in upper and lower bounds, but only keep 95% PI
iud_yy <- c(100*((iud_test-iud_preds$upper)/iud_preds$upper)[,2], rev(100*((iud_test-iud_preds$lower)/iud_preds$lower)[,2]))
polygon(iud_xx, iud_yy, col=grey(0.9), border = F)
# show the percent difference
lines(iud_x, 100*(iud_test-iud_preds$mean)/iud_preds$mean, lwd=2)
grid()
abline(h=0, lty=2) # no difference
legend(1, 95, legend=c("Percent Change with 95% PI", "No Change"), lty=c(1, 2), box.lty=0, cex=0.75)
xmin = as.Date("2016-11-01")
xmax = as.Date("2017-10-01")
xseq = seq.Date(from = xmin, to = xmax, by = "month")
axis(1, at=iud_x, labels=format(xseq, format = "%b %y"), las=1)
#dev.off()

# pill percent difference between forecast & observed
#png('pill_percent.png')
# create list of integer from 1 to number of test dates
pill_x <- 1:length(pill_test)
# create a plot for difference
plot(pill_x, 100*(pill_test-pill_preds$mean)/pill_preds$mean, type='n', ylim=c(-10,110), main='Percent Difference in RSV: Birth Control Pills', ylab='Percent Change in RSV', xaxt="n")
pill_xx <- c(pill_x, rev(pill_x))
# find difference in upper and lower bounds, but only keep 95% PI
pill_yy <- c(100*((pill_test-pill_preds$upper)/pill_preds$upper)[,2], rev(100*((pill_test-pill_preds$lower)/pill_preds$lower)[,2]))
polygon(pill_xx, pill_yy, col=grey(0.9), border = F)
# show the percent difference
lines(pill_x, 100*(pill_test-pill_preds$mean)/pill_preds$mean, lwd=2)
grid()
abline(h=0, lty=2) # no difference
legend(1, 95, legend=c("Percent Change with 95% PI", "No Change"), lty=c(1, 2), box.lty=0, cex=0.75)
xmin = as.Date("2016-11-01")
xmax = as.Date("2017-10-01")
xseq = seq.Date(from = xmin, to = xmax, by = "month")
axis(1, at=iud_x, labels=format(xseq, format = "%b %y"), las=1)
#dev.off()

# condom percent difference between forecast & observed
#png('condom_percent.png')
# create list of integer from 1 to number of test dates
condom_x <- 1:length(condom_test)
# create a plot for difference
plot(condom_x, 100*(condom_test-condom_preds$mean)/condom_preds$mean, type='n', ylim=c(-10,110), main='Percent Difference in RSV: Condoms', ylab='Percent Change in RSV', xaxt="n")
condom_xx <- c(condom_x, rev(condom_x))
# find difference in upper and lower bounds, but only keep 95% PI
condom_yy <- c(100*((condom_test-condom_preds$upper)/condom_preds$upper)[,2], rev(100*((condom_test-condom_preds$lower)/condom_preds$lower)[,2]))
polygon(condom_xx, condom_yy, col=grey(0.9), border = F)
# show the percent difference
lines(condom_x, 100*(condom_test-condom_preds$mean)/condom_preds$mean, lwd=2)
grid()
abline(h=0, lty=2) # no difference
legend(1, 95, legend=c("Percent Change with 95% PI", "No Change"), lty=c(1, 2), box.lty=0, cex=0.75)
xmin = as.Date("2016-11-01")
xmax = as.Date("2017-10-01")
xseq = seq.Date(from = xmin, to = xmax, by = "month")
axis(1, at=iud_x, labels=format(xseq, format = "%b %y"), las=1)
#dev.off()


#**************************
# Total Demand
#**************************
monthly$all = monthly$iud + monthly$pill + monthly$condom
all_qfts <- ts(monthly$all, frequency = 12, start = c(2004, 1))
autoplot(all_qfts)

png('total_demand.png')
plot(all_qfts, xlim=c(2008,2018), ylab="Query Fraction", main="QF: IUD + BC Pill + Condom")
grid()
dev.off()

# create training (pre-election) and test (post-election) sets
all_training <- window(all_qfts, end = c(2016, 10))
all_test <- window(all_qfts, start = c(2016, 11))
autoplot(all_training)
autoplot(all_test)

# fit training data
all_fit <- auto.arima(all_training, trace=TRUE)
summary(all_fit)
checkresiduals(all_fit, plot = TRUE)
tsdisplay(residuals(all_fit), lag.max = 12)

# forecast
all_preds <- forecast(all_fit, length(all_test))

# calculate simple bootstrap confidence intervals 
B <- replicate(1000, expr={
  # take a random sample of size length(test) with replacement
  ix <- sample(1:length(all_test), length(all_test), replace=T)
  # calculate percentage difference
  (sum(all_test[ix])-sum(all_preds$mean[ix]))/sum(all_preds$mean[ix])
})

# mean and CIs for difference between forecast & observed
print.ests(c((sum(all_test)-sum(all_preds$mean))/sum(all_preds$mean), quantile(B, c(0.025, 0.975))))
# result: 4% (95% CI: 2-6)

# difference in November 16
100*(all_test[1] - all_preds$mean[1])/all_preds$mean[1]
100*(all_test[1] - all_preds$lower[1])/all_preds$lower[1]
100*(all_test[1] - all_preds$upper[1])/all_preds$upper[1]
#8% 95% Prediction Interval: [3,14]

# difference in January 17
100*(all_test[3] - all_preds$mean[3])/all_preds$mean[3]
100*(all_test[3] - all_preds$lower[3])/all_preds$lower[3]
100*(all_test[3] - all_preds$upper[3])/all_preds$upper[3]
#8% 95% Prediction Interval: [1, 16]

# difference in May 17
100*(all_test[7] - all_preds$mean[7])/all_preds$mean[7]
100*(all_test[7] - all_preds$lower[7])/all_preds$lower[7]
100*(all_test[7] - all_preds$upper[7])/all_preds$upper[7]
#11% 95% Prediction Interval: [3, 22]

setwd('~/Documents/GoogleTrend/Images')
png('qf_rsv_total.png')
par(mfrow=c(1,2))

# plot 1: qf total
plot(all_preds, PI=FALSE, fcol = 'black', flwd = 1, xlim=c(2008,2018), ylim=c(0,3000), ylab="Query Fraction", main="QF: Total")
lines(x=all_test)
grid()
all_x <- time(all_test)
all_y_obs = monthly[155:166, 'all']
all_y_pred = as.numeric(all_preds$mean)
polygon(c(all_x,rev(all_x)),c(all_y_obs,rev(all_y_pred)),col = 'grey85', border = NA)

# plot 2: rsv total
# create list of integer from 1 to number of test dates
all_x <- 1:length(all_test)
# create a plot for difference
plot(all_x, 100*(all_test-all_preds$mean)/all_preds$mean, type='n', ylim=c(-10,110), main='Percent Difference in RSV: Total', ylab='Percent Change in RSV', xaxt="n", xlab=NA)
all_xx <- c(all_x, rev(all_x))
# find difference in upper and lower bounds, but only keep 95% PI
all_yy <- c(100*((all_test-all_preds$upper)/all_preds$upper)[,2], rev(100*((all_test-all_preds$lower)/all_preds$lower)[,2]))
polygon(all_xx, all_yy, col=grey(0.9), border = F)
# show the percent difference
lines(all_x, 100*(all_test-all_preds$mean)/all_preds$mean, lwd=2)
grid()
abline(h=0, lty=2) # no difference
legend(1, 95, legend=c("Percent Change with 95% PI", "No Change"), lty=c(1, 2), box.lty=0, cex=0.75)
xmin = as.Date("2016-11-01")
xmax = as.Date("2017-10-01")
xseq = seq.Date(from = xmin, to = xmax, by = "month")
axis(1, at=iud_x, labels=format(xseq, format = "%b %y"), las=1)

dev.off()

#-----------------------------------------
# now look at impact of ACA and election
#----------------------------------------
# create pre-free birth control
pre_free_bc <- window(all_qfts, end=c(2012, 07))
# create post-free birth control, but before election
post_free_bc_pre_elect <- window(all_qfts, start=c(2012, 08), end=c(2016, 10))
# remember post-election is all_test

hist(pre_free_bc)
hist(post_free_bc_pre_elect)
hist(all_test)

# is my data normal?
# look at density plot
plot(density(pre_free_bc))
plot(density(post_free_bc_pre_elect))
plot(density(all_test))

# shapiro test
shapiro.test(pre_free_bc) # not normal
shapiro.test(post_free_bc_pre_elect) # not normal
shapiro.test(all_test) # normal

# qq-plots
qqnorm(pre_free_bc);qqline(pre_free_bc, col = 2)
qqnorm(post_free_bc_pre_elect);qqline(post_free_bc_pre_elect, col = 2)
qqnorm(all_test);qqline(all_test, col=2)

# median
median(pre_free_bc)
# 1874.084
median(post_free_bc_pre_elect)
# 2182.558


# wilcoxin rank sum test
wilcox.test(pre_free_bc, post_free_bc_pre_elect, paired = FALSE, alternative = "two.sided", conf.int = TRUE)
# medians of distributions differ
# pre_free_bc is significantly different than post_free_bc_pre_elect 
# p-value = 9.037e-13


#****************************
# Plot Figures
#****************************
setwd('~/Documents/GoogleTrend/Images')
png('qf_rsv_iud_pill_condom.png')
par(mfrow=c(3,2))
dev.off()


png('all_figures.png')
layout(matrix(c(1, 2,
                3, 4,
                5, 6,
                7, 7), nrow=4, byrow=TRUE))
layout.show(n=7)
dev.off()

#**************************
# Save Workspace
#**************************
# create a saved image below total plot
setwd("~/Documents/GoogleTrend/Workspace")
save.image("IUD_Analysis.RData")