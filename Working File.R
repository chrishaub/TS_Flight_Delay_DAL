library(tswge)
library(magrittr)
library(dplyr)
library(tidyverse)
library(stats)
library(matlab)
library(tseries)
library(vars)
library(RColorBrewer)
library(nnfor)
library(keras)
library(tfruns)
library(stringr)
library(vars)
library(nnfor) #MLP
library(forecast)


# Merged data frame
flightdf=read.csv("D:/SMUMSDS/TimeSeries/Final Project/ModAirDelay2018V3.csv", header = TRUE)
head(flightdf)

# EDA
plotts.wge(flightdf$avg_dep_delay) 
parzen.wge(flightdf$avg_dep_delay)
acf(flightdf$avg_dep_delay, lag.max = 100)
parzen.wge(flightdf$avg_dep_delay, trunc = 300)

# Model 1
# Difference the data for average flight delays
FD_1 = artrans.wge(flightdf$avg_dep_delay,1)
# Looks close to white noise after differencing 
acf(FD_1,lag.max = 200) 
acf(FD_1,lag.max = 25) 

# (1-B^19)Xt=at
FD_1_19 = artrans.wge(FD_1, phi.tr = c(rep(0,18),1)) 
aic5.wge(FD_1_19)
# 0    1   7.114578
# 5    0   7.132675
aic5.wge(FD_1_19, type = "bic") 
# 0    1   7.116580
# 5    0   7.138682

# May need to change to est.arma.wge
# Model ARMA (5,0), 19
e = est.ar.wge(FD_1_19,p = 5) 
e$phi


FD_1_19_AR5 = artrans.wge(FD_1_19, phi.tr = e$phi)
ljung.wge(FD_1_19_AR5) 
ljung.wge(FD_1_19_AR5, K = 48)

# Forecast ARIMA model
# Use code for final prediction, lastn=FALSE, limits=TRUE
fore_arima = fore.arima.wge(flightdf$avg_dep_delay,
                            d = 1, s = 19, phi = e$phi,
                            n.ahead = 19, lastn = TRUE, limits = FALSE) 

# plot(seq(6500,6840,1), flightdf$avg_dep_delay[6500 : 6840], type = "l",xlim = c(6500,6859),
#      ylim =c(0,400), ylab = "Avg Temp", main = "Temp Forecast")
# lines(seq(6841,6859,1), avg_dep_delay$f, type = "l", col = "red")

# Average how much each prediction off in minutes, for last 19 observations 
roll.win.rmse.wge(flightdf$avg_dep_delay,s = 19, d = 1, phi = e$phi, h = 19)

# How much prediction are off for last 19 obs
ASE_B19_AR5 = mean((flightdf$avg_dep_delay
                    [(length(flightdf$avg_dep_delay)-18): length(flightdf$avg_dep_delay)]-fore_arima$f)^2)
ASE_B19_AR5 # 208.2214
# Same code as above different way
mean((fore_arima$f- tail(flightdf$avg_dep_delay,19))^2)

# Model ARMA (5,0), 57
fore_arima_lt = fore.arima.wge(flightdf$avg_dep_delay,d = 1, s = 19, phi = e$phi,
                            n.ahead = 57, lastn = TRUE, limits = FALSE) 

# Average how much each prediction off in minutes, for last 57 observations 
roll.win.rmse.wge(flightdf$avg_dep_delay,s = 19, d = 1, phi = e$phi, h = 57)

# Same code as above different way
ASE_B19_AR5_LT = mean((fore_arima_lt$f- tail(flightdf$avg_dep_delay,57))^2)
ASE_B19_AR5_LT # 3637.509

# Next Model ARMA (0,1)
e = est.arma.wge(FD_1_19,q = 1) 
e$theta

# Use code for final prediction, lastn=FALSE, limits=TRUE
fore_arima = fore.arima.wge(flightdf$avg_dep_delay,d = 1, s = 19, theta = e$theta,
                            n.ahead = 19, lastn = TRUE, limits = FALSE) 
#ASE
ASE_B19_ARMA01 = mean((fore_arima$f- tail(flightdf$avg_dep_delay,19))^2)
ASE_B19_ARMA01 # 182.221

# Average how much each prediction off in minutes, for last 19 observations 
RMSE_ARMA01 = roll.win.rmse.wge(flightdf$avg_dep_delay,s = 19, d = 1, theta = e$theta, h = 19)
#"The Rolling Window RMSE is:  37.567"

# Forecast
fore_arima = fore.arima.wge(flightdf$avg_dep_delay,d = 1, s = 19, theta = e$theta,
                            n.ahead = 19, lastn = FALSE, limits = TRUE) 

# Multivariate Analysis
# Forecast temperature
temp = flightdf$avg_temp
temp_small = temp[1:6821]
temp_small
plotts.sample.wge(temp)
acf(temp)
aic5.wge(temp) #4 2 1.909015
aic5.wge(temp,type = "bic")
est_temp_ARMA_4_2 = est.arma.wge(temp_small, p = 4, q = 2)
# factor.wge(phi = c(2.9046, -2.9462, 1.0722, -0.0314), theta = c(1.8171 -0.9098))

# Factor table shows seasonality of 19, show screenshot of factor table (Do for EDA at beg project)
factor.wge(phi=c(rep(0,18),1))
parzen.wge(temp)

# (1-B^19)Xt=at
temp_1_19 = artrans.wge(temp, c(rep(0,18),1))
temp_1_19
plotts.sample.wge(temp_1_19)
aic5.wge(temp_1_19)

temp_ARMA_3_2 = est.arma.wge(temp, p = 3, q = 2)
plotts.sample.wge(temp_ARMA_3_2$res)
ljung.wge(temp_ARMA_3_2$res)$pval # Fail to reject Ho, 0.02633271
ljung.wge(temp_ARMA_3_2$res, K = 48)$pval # Fail to reject Ho, 0.02633271

roll.win.rmse.wge(temp, phi=temp_ARMA_3_2$phi, horizon = 19)

preds_avg_temp = fore.arma.wge(temp, phi=temp_ARMA_3_2$phi, n.ahead = 19)
preds_avg_temp
plot(preds_avg_temp$f, type = "l")

# Predict next 19
length(seq(1, 6840, 1)) # length of x vector
length(temp) # length of y vector
plot(seq(6500,6840,1), temp[6500 : 6840], type = "l",xlim = c(6500,6859),
     ylim =c(35,70), ylab = "Avg Temp", main = "Temp Forecast")
lines(seq(6841,6859,1), preds_avg_temp$f, type = "l", col = "red")

# Predict previous 19
preds_avg_temp = fore.arma.wge(temp, phi=temp_ARMA_3_2$phi, n.ahead = 19, lastn=TRUE)
preds_avg_temp
plot(preds_avg_temp$f, type = "l")


plot(seq(6500,6840,1), temp[6500 : 6840], type = "l",xlim = c(6500,6859),
     ylim =c(32,70), ylab = "Avg Temp", main = "Temp Forecast")
lines(seq(6822,6840,1), preds_avg_temp$f, type = "l", col = "red")


# # Make sure x and y vectors have the same length
# x <- seq(1, 6840, 1)
# y <- flightdf$avg_temp
# 
# # Plot the first line
# plot(x, y, type = "l", xlim = c(0, 7840), ylab = "Avg Temp", main = "Temp Forecast")
# 
# # Make sure predsPart$f has the same length as x vector
# x2 <- seq(6841, 7840, 1)
# y2 <- predsPart$f[1:length(x2)]
# 
# # Plot the second line
# lines(x2, y2, type = "l", col = "red")





# Forecast humidity

humid = flightdf$avg_humidity
humid_small = humid[1:6821]
humid_small
plotts.sample.wge(humid)
acf(humid)
aic5.wge(humid) #5 0 3.603678
aic5.wge(temp,type = "bic") #3 2 1.915578
est_temp_ARMA_4_2 = est.arma.wge(humid_small, p = 3, q = 2)
# factor.wge(phi = c(0.7291, 0.8057, -0.6293), theta = c(-0.4563, 0.4272))

# (1-B^19)Xt=at
humid_1_19 = artrans.wge(temp, c(rep(0,18),1))
humid_1_19
plotts.sample.wge(humid_1_19)
aic5.wge(humid_1_19) # 3 2 2.246574

humid_ARMA_3_2 = est.arma.wge(temp, p = 3, q = 2) # AR =c(2.8726, -2.8544, 0.9810) MA =c(1.8116, -0.9047)
plotts.sample.wge(humid_ARMA_3_2$res)
ljung.wge(humid_ARMA_3_2$res)$pval # Fail to reject Ho, 0.02633271
ljung.wge(humid_ARMA_3_2$res, K = 48)$pval # Fail to reject Ho, 0.02633271

roll.win.rmse.wge(temp, phi=humid_ARMA_3_2$phi, horizon = 19)

# Predict next 19
preds_avg_humid = fore.arma.wge(temp, phi=humid_ARMA_3_2$phi, n.ahead = 19)
preds_avg_humid
plot(humid_ARMA_3_2$f, type = "l")

length(seq(1, 6840, 1)) # length of x vector
length(temp) # length of y vector
plot(seq(6500,6840,1), humid[6500 : 6840], type = "l",xlim = c(6500,6859),
     ylim =c(25,100), ylab = "Avg Humidity", main = "Humidity Forecast")
lines(seq(6841,6859,1), preds_avg_humid$f, type = "l", col = "red")

# Predict previous 19
preds_avg_humid_prev19 = fore.arma.wge(temp, phi=humid_ARMA_3_2$phi, n.ahead = 19, lastn=TRUE)
preds_avg_humid_prev19
plot(preds_avg_humid_prev19$f, type = "l")

plot(seq(6500,6840,1), temp[6500 : 6840], type = "l",xlim = c(6500,6859),
     ylim =c(25,100), ylab = "Avg Humidity", main = "Humidity Forecast")
lines(seq(6822,6840,1), preds_avg_humid$f, type = "l", col = "red")


flightdf_small = flightdf[1:6821,]
flightdf_small
# add week, have to add time component to model trend
ksfit = lm(avg_dep_delay~avg_temp+avg_humidity+month+day+DEP_TIME, data = flightdf)
plotts.sample.wge(ksfit$residuals)
phi = aic.wge(ksfit$residuals) # aic on residuals, fits an ARMA(2,2)
phi
# fitting an ARMA (2,2)
fit = arima(flightdf_small$avg_dep_delay,order = c(phi$p,0,0), xreg = cbind(
  flightdf_small$avg_temp, flightdf_small$avg_humidity, flightdf_small$month, flightdf_small$day , flightdf_small$DEP_TIME))

AIC(fit) #64198.41

# VAR, Short
# Find ASE using last 19
flightdf_small = flightdf[1:6821,]  # 456 = 508-52
VARselect(cbind(flightdf_small$avg_dep_delay[2:6821], flightdf_small$avg_temp[2:6821], 
                flightdf_small$avg_humidity[2:6821]),
          lag.max = 25, season = 19, type = "both") 
# Most useful AIC lag 23, but for others lag 21

# Predict last 19
flight_del_VAR = VAR(cbind(flightdf_small$avg_dep_delay[2:6821], flightdf_small$avg_temp[2:6821], flightdf_small$avg_humidity[2:6821]),
                     season = 19, type = "both",p = 21)
preds=predict(flight_del_VAR,n.ahead=19)

flight_del_VAR$varresult$y1

# plot(seq(6500,6840,1), type = "l",xlim = c(6500,6840),
#      ylim =c(0,375), ylab = "Flight Delay, Minutes", main = "Average Flight Delay Forecast")
# lines(seq(6822,6840,1), preds$fcst$y1[,1], type = "l", col = "red")

plot(flightdf_small$avg_dep_delay, type = "l", ylim = c(0, 375),xlim = c(6500,6840),
     ylab = "Flight Delay, Minutes", main = "Average Flight Delay Forecast")
lines(seq(6822, 6840, 1), preds$fcst$y1[, 1], type = "l", col = "red")


ASE = mean((flightdf$avg_dep_delay[6822:6840] - preds$fcst$y1[,1])^2)
ASE # 94.63075

# Forecast ahead 19
VARselect(cbind(flightdf$avg_dep_delay, flightdf$avg_temp, 
                flightdf$avg_humidity),
          lag.max = 25, season = 19, type = "both")

# flight_del_VAR_ahead = VAR(cbind(flightdf$avg_dep_delay, flightdf$avg_temp, 
#                                  flightdf$avg_humidity),
#                            season = 19, type = "both",p = 21)
flight_del_VAR_ahead = VAR(flightdf[,5:7],
                           season = 19, type = "both",p = 21)
preds=predict(flight_del_VAR_ahead,n.ahead=19)

# plot(seq(6500,6840,1), type = "l",xlim = c(6500,6859),
#      ylim =c(0,400), ylab = "Flight Delay", main = "Average Flight Delay Forecast")
# lines(seq(6841,6859,1), preds$fcst$y1[,1], type = "l", col = "red")

plot(flightdf$avg_dep_delay, type = "l", ylim = c(-10, 375), xlim = c(6500,6859), 
     ylab = "Flight Delay, Minutes", main = "Average Flight Delay Forecast")
lines(seq(6841, 6859, 1), preds$fcst$y1[,1], type = "l", col = "red")

# VAR, Long
# Find ASE using last 19
flightdf_small57 = flightdf[1:6783,]  # 456 = 508-52
VARselect(cbind(flightdf_small57$avg_dep_delay[2:6783], flightdf_small57$avg_temp[2:6783], 
                flightdf_small57$avg_humidity[2:6783]),
          lag.max = 25, season = 19, type = "both") 
# Most useful AIC lag 23, but for others lag 21

# Predict last 57
flight_del_VAR57 = VAR(cbind(flightdf_small57$avg_dep_delay[2:6783], flightdf_small57$avg_temp[2:6783], flightdf_small57$avg_humidity[2:6783]),
                     season = 19, type = "both",p = 21)
preds=predict(flight_del_VAR57,n.ahead=57)

flight_del_VAR57$varresult$y1

plot(flightdf_small57$avg_dep_delay, type = "l", ylim = c(0, 375),xlim = c(6500,6840),
     ylab = "Flight Delay, Minutes", main = "Average Flight Delay Forecast,")
lines(seq(6784, 6840, 1), preds$fcst$y1[, 1], type = "l", col = "red")

ASE = mean((flightdf$avg_dep_delay[6784:6840] - preds$fcst$y1[,1])^2)
ASE #268.9163

# Forecast ahead 19
VARselect(cbind(flightdf$avg_dep_delay, flightdf$avg_temp, 
                flightdf$avg_humidity),
          lag.max = 25, season = 19, type = "both")

flight_del_VAR_ahead57 = VAR(cbind(flightdf$avg_dep_delay, flightdf$avg_temp, 
                                 flightdf$avg_humidity),
                           season = 19, type = "both",p = 21)
preds=predict(flight_del_VAR_ahead57,n.ahead=57)

plot(flightdf$avg_dep_delay, type = "l", ylim = c(-10, 375), xlim = c(6500,6897), 
     ylab = "Flight Delay, Minutes", main = "Average Flight Delay Forecast")
lines(seq(6841, 6897, 1), preds$fcst$y1[,1], type = "l", col = "red")


#MLP

flight_small = flightdf[5131:6821,]
flight_smallDF = data.frame(temperature = ts(flight_small$avg_temp))

#Using forecast average temp
fit_mlp_climate = mlp(ts(flight_small$avg_temp),reps = 4, comb = "mean")
fore_mlp_climate = forecast(fit_mlp_climate, h = 19)

flight_smallDF_fore = data.frame(climate = ts(fore_mlp_climate$mean))
flight_smallDF_fore


flight_smallDF_delay = data.frame(average_delay = ts(flight_small$avg_dep_delay))

#Using forecast Sunspots
fit_mlp_delay = mlp(ts(flight_smallDF_delay),reps = 2, comb = "mean")
fore_mlp_delay = forecast(fit_mlp_delay, h = 19)
fore_mlp_delay

flight_smallDF_delay_fore = data.frame(average_delay = ts(fore_mlp_delay$mean))
flight_smallDF_delay_fore
# small temperature + predictions (predictions flight_smallDF)
small_temp_fore = ts(c(ts(flight_smallDF), fore_mlp_climate$mean))
head(small_temp_fore)

# climate_variable = data.frame((climate = small_temp_fore),pressure = small_pressure_fore))
climate_variable = data.frame(climate = small_temp_fore)
fit_mlp = mlp(ts(flight_small$avg_dep_delay),reps = 1, xreg = climate_variable)
fit_mlp
plot(fit_mlp)

plot(flightdf$avg_dep_delay, type = "l")
# lines(seq(5131,6840,1),fit_mlp$mean,col = "blue")

fore_mlp = forecast(fit_mlp, h = 19, xreg = climate_variable)
plot(fore_mlp)

ASE = mean((flight_smallDF_delay$average_delay[1673:1691] - fore_mlp$mean)^2)
ASE

# Predict future MLP, 19 (use full dataset)
# flight_small = flightdf[5131:6821,]
# flight_smallDF = data.frame(temperature = ts(flight_small$avg_temp))
flight_reduced = flightdf[5131:6840,]
temp_ts = data.frame(temperature = ts(flight_reduced$avg_temp))

#Using forecast average temp
# fit_mlp_climate = mlp(ts(flight_small$avg_temp),reps = 4, comb = "mean")
# fore_mlp_climate = forecast(fit_mlp_climate, h = 19)
fit_mlp_climate_ahead = mlp(ts(flight_reduced$avg_temp),reps = 4, comb = "mean")
fore_mlp_climate_ahead = forecast(fit_mlp_climate_ahead, h = 19)

# flight_smallDF_fore = data.frame(climate = ts(fore_mlp_climate$mean))
# flight_smallDF_fore
climate_fore = data.frame(climate = ts(fore_mlp_climate_ahead$mean))
climate_fore

# flight_smallDF_delay = data.frame(average_delay = ts(flight_small$avg_dep_delay))
flight_delay = data.frame(average_delay = ts(flight_reduced$avg_dep_delay ))

#Using forecast Sunspots
# fit_mlp_delay = mlp(ts(flight_smallDF_delay),reps = 2, comb = "mean")
# fore_mlp_delay = forecast(fit_mlp_delay, h = 19)
# fore_mlp_delay

# fit_mlp_delay_forward = mlp(ts(flight_delay),reps = 2, comb = "mean")
fit_mlp_delay_forward = mlp(ts(flight_delay),reps = 2, comb = "mean")
fore_mlp_delay_forward = forecast(fit_mlp_delay_forward, h = 19)
fore_mlp_delay_forward

flight_smallDF_delay_fore = data.frame(average_delay = ts(fore_mlp_delay_forward$mean))
flight_smallDF_delay_fore
# reduced temperature + predictions (predictions flight_smallDF_delay_fore)
climate_fore_ahead = ts(c(ts(temp_ts), fore_mlp_climate_ahead$mean))
head(climate_fore_ahead)

# climate_variable = data.frame((climate = small_temp_fore),pressure = small_pressure_fore))
climate_variable = data.frame(climate = climate_fore_ahead)
fit_mlp = mlp(ts(flight_reduced$avg_dep_delay),reps = 4, xreg = climate_variable)
fit_mlp
plot(fit_mlp)

plot(flightdf$avg_dep_delay, type = "l")
lines(seq(5131,6840,1),fit_mlp$mean,col = "blue")

fore_mlp = forecast(fit_mlp, h = 19, xreg = climate_variable)
plot(fore_mlp)

ASE = mean((flight_delay$average_delay[1673:1691] - fore_mlp$mean)^2)
ASE

#ensemble

ensemble = (fore_arima$f + fore_mlp$mean)/2

plot(seq(6822, 6840,1),flightdf$avg_dep_delay[6822:6840], type = "l", ylim=c(0,100))
lines(seq(6822, 6840,1),ensemble,col = "green")

ASE = mean((flightdf$avg_dep_delay[6822:6840] - ensemble)^2)
ASE