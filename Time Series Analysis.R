#Do time series analysis. Explain why the selected techniques are better for thedefined objectives and show if you’ve found any similar research in the literature.
install.packages("TTR")
install.packages("forecast")
library(TTR)
library(forecast)

#Here, in our dataset we can plot the time series of the increase of the feature "cer_prod" by time, "year_of_value"
# Converting our data to time series object

time_series_data_cer_prod <- ts(Agri_resources_data$cer_prod, start = 2012, frequency = 1)
plot.ts(time_series_data_cer_prod, main = "Time Series Plot of cer_prod", xlab = "Year", ylab = "cer_prod")
autoplot(time_series_data_cer_prod) +
  labs(title = "Time Series Plot of cer_prod", x = "Year", y = "cer_prod")
# Decompose the time series to identify trend, seasonality, and remainder components. Here, our time series is too small, hence decomposition is not required. Thus quitting the stage of time-series analysis.
# Fit an ARIMA model
arima_model_cer_prod <- auto.arima(time_series_data_cer_prod)
# Print model summary
summary(arima_model_cer_prod)
# Forecast future values
forecast_cer_prod <- forecast(arima_model_cer_prod, h = 5)  # forecast for the next 5 periods
# Plot the forecast
autoplot(forecast_cer_prod) +
  labs(title = "ARIMA Forecast of cer_prod", x = "Year", y = "cer_prod")
# Check residuals for model diagnostics
checkresiduals(arima_model_cer_prod)

#Here, in our dataset we can plot the time series of the increase of the feature "cap_fish_prod" by time, "year_of_value"
# Converting our data to time series object
time_series_data_cap_fish_prod <- ts(Agri_resources_data$cap_fish_prod, start = 2012, frequency = 1)
plot.ts(time_series_data_cap_fish_prod, main = "Time Series Plot of cap_fish_prod", xlab = "Year", ylab = "cap_fish_prod")
autoplot(time_series_data_cap_fish_prod) +
  labs(title = "Time Series Plot of cap_fish_prod", x = "Year", y = "cap_fish_prod")
# Decompose the time series to identify trend, seasonality, and remainder components. Here, our time series is too small, hence decomposition is not required. Thus quitting the stage of time-series analysis.
# Fit an ARIMA model
arima_model_cap_fish_prod <- auto.arima(time_series_data_cap_fish_prod)
# Print model summary
summary(arima_model_cap_fish_prod)
# Forecast future values
forecast_cap_fish_prod <- forecast(arima_model_cap_fish_prod, h = 5)  # forecast for the next 5 periods
# Plot the forecast
autoplot(forecast_cap_fish_prod) +
  labs(title = "ARIMA Forecast of cap_fish_prod", x = "Year", y = "cap_fish_prod")
# Check residuals for model diagnostics
checkresiduals(arima_model_cap_fish_prod)

#Here, in our dataset we can plot the time series of the increase of the feature "cer_yld" by time, "year_of_value"
# Converting our data to time series object
time_series_data_cer_yld <- ts(Agri_resources_data$cer_yld, start = 2012, frequency = 1)
plot.ts(time_series_data_cer_yld, main = "Time Series Plot of cer_yld", xlab = "Year", ylab = "cer_yld")
autoplot(time_series_data_cer_yld) +
  labs(title = "Time Series Plot of cer_yld", x = "Year", y = "cer_yld")
# Decompose the time series to identify trend, seasonality, and remainder components. Here, our time series is too small, hence decomposition is not required. Thus quitting the stage of time-series analysis.
# Fit an ARIMA model
arima_model_cer_yld <- auto.arima(time_series_data_cer_yld)
# Print model summary
summary(arima_model_cer_yld)
# Forecast future values
forecast_cer_yld <- forecast(arima_model_cer_yld, h = 5)  # forecast for the next 5 periods
# Plot the forecast
autoplot(forecast_cer_yld) +
  labs(title = "ARIMA Forecast of cer_yld", x = "Year", y = "cer_yld")
# Check residuals for model diagnostics
checkresiduals(arima_model_cer_yld)

#Here, in our dataset we can plot the time series of the increase of the feature "fert_cnsmtn" by time, "year_of_value"
# Converting our data to time series object
time_series_data_fert_cnsmtn <- ts(Agri_resources_data$fert_cnsmtn, start = 2012, frequency = 1)
plot.ts(time_series_data_fert_cnsmtn, main = "Time Series Plot of fert_cnsmtn", xlab = "Year", ylab = "fert_cnsmtn")
autoplot(time_series_data_fert_cnsmtn) +
  labs(title = "Time Series Plot of fert_cnsmtn", x = "Year", y = "fert_cnsmtn")
# Decompose the time series to identify trend, seasonality, and remainder components. Here, our time series is too small, hence decomposition is not required. Thus quitting the stage of time-series analysis.
# Fit an ARIMA model
arima_model_fert_cnsmtn <- auto.arima(time_series_data_fert_cnsmtn)
# Print model summary
summary(arima_model_fert_cnsmtn)
# Forecast future values
forecast_fert_cnsmtn <- forecast(arima_model_fert_cnsmtn, h = 5)  # forecast for the next 5 periods
# Plot the forecast
autoplot(forecast_fert_cnsmtn) +
  labs(title = "ARIMA Forecast of fert_cnsmtn", x = "Year", y = "fert_cnsmtn")
# Check residuals for model diagnostics
checkresiduals(arima_model_fert_cnsmtn)

#Here, in our dataset we can plot the time series of the increase of the feature "Aqua_cult_prod" by time, "year_of_value"
# Converting our data to time series object
time_series_data_Aqua_cult_prod <- ts(Agri_resources_data$Aqua_cult_prod, start = 2012, frequency = 1)
plot.ts(time_series_data_Aqua_cult_prod, main = "Time Series Plot of Aqua_cult_prod", xlab = "Year", ylab = "Aqua_cult_prod")
autoplot(time_series_data_Aqua_cult_prod) +
  labs(title = "Time Series Plot of Aqua_cult_prod", x = "Year", y = "Aqua_cult_prod")
# Decompose the time series to identify trend, seasonality, and remainder components. Here, our time series is too small, hence decomposition is not required. Thus quitting the stage of time-series analysis.
# Fit an ARIMA model
arima_model_Aqua_cult_prod <- auto.arima(time_series_data_Aqua_cult_prod)
# Print model summary
summary(arima_model_Aqua_cult_prod)
# Forecast future values
forecast_Aqua_cult_prod <- forecast(arima_model_Aqua_cult_prod, h = 5)  # forecast for the next 5 periods
# Plot the forecast
autoplot(forecast_Aqua_cult_prod) +
  labs(title = "ARIMA Forecast of Aqua_cult_prod", x = "Year", y = "Aqua_cult_prod")
# Check residuals for model diagnostics
checkresiduals(arima_model_Aqua_cult_prod)

#Here, in our dataset we can plot the time series of the increase of the feature "crp_prod_indx" by time, "year_of_value"
# Converting our data to time series object
time_series_data_crp_prod_indx <- ts(Agri_resources_data$crp_prod_indx, start = 2012, frequency = 1)
plot.ts(time_series_data_crp_prod_indx, main = "Time Series Plot of crp_prod_indx", xlab = "Year", ylab = "crp_prod_indx")
autoplot(time_series_data_crp_prod_indx) +
  labs(title = "Time Series Plot of crp_prod_indx", x = "Year", y = "crp_prod_indx")
# Decompose the time series to identify trend, seasonality, and remainder components. Here, our time series is too small, hence decomposition is not required. Thus quitting the stage of time-series analysis.
# Fit an ARIMA model
arima_model_crp_prod_indx <- auto.arima(time_series_data_crp_prod_indx)
# Print model summary
summary(arima_model_crp_prod_indx)
# Forecast future values
forecast_crp_prod_indx <- forecast(arima_model_crp_prod_indx, h = 5)  # forecast for the next 5 periods
# Plot the forecast
autoplot(forecast_crp_prod_indx) +
  labs(title = "ARIMA Forecast of crp_prod_indx", x = "Year", y = "crp_prod_indx")
# Check residuals for model diagnostics
checkresiduals(arima_model_crp_prod_indx)

#Here, in our dataset we can plot the time series of the increase of the feature "fd_prod_indx" by time, "year_of_value"
# Converting our data to time series object
time_series_data_fd_prod_indx <- ts(Agri_resources_data$fd_prod_indx, start = 2012, frequency = 1)
plot.ts(time_series_data_fd_prod_indx, main = "Time Series Plot of fd_prod_indx", xlab = "Year", ylab = "fd_prod_indx")
autoplot(time_series_data_fd_prod_indx) +
  labs(title = "Time Series Plot of fd_prod_indx", x = "Year", y = "fd_prod_indx")
# Decompose the time series to identify trend, seasonality, and remainder components. Here, our time series is too small, hence decomposition is not required. Thus quitting the stage of time-series analysis.
# Fit an ARIMA model
arima_model_fd_prod_indx <- auto.arima(time_series_data_fd_prod_indx)
# Print model summary
summary(arima_model_fd_prod_indx)
# Forecast future values
forecast_fd_prod_indx <- forecast(arima_model_fd_prod_indx, h = 5)  # forecast for the next 5 periods
# Plot the forecast
autoplot(forecast_fd_prod_indx) +
  labs(title = "ARIMA Forecast of fd_prod_indx", x = "Year", y = "fd_prod_indx")
# Check residuals for model diagnostics
checkresiduals(arima_model_fd_prod_indx)

#Here, in our dataset we can plot the time series of the increase of the feature "lvstk_prod_indx" by time, "year_of_value"
# Converting our data to time series object
time_series_data_lvstk_prod_indx <- ts(Agri_resources_data$lvstk_prod_indx, start = 2012, frequency = 1)
plot.ts(time_series_data_lvstk_prod_indx, main = "Time Series Plot of lvstk_prod_indx", xlab = "Year", ylab = "lvstk_prod_indx")
autoplot(time_series_data_lvstk_prod_indx) +
  labs(title = "Time Series Plot of lvstk_prod_indx", x = "Year", y = "lvstk_prod_indx")
# Decompose the time series to identify trend, seasonality, and remainder components. Here, our time series is too small, hence decomposition is not required. Thus quitting the stage of time-series analysis.
# Fit an ARIMA model
arima_model_lvstk_prod_indx <- auto.arima(time_series_data_lvstk_prod_indx)
# Print model summary
summary(arima_model_lvstk_prod_indx)
# Forecast future values
forecast_lvstk_prod_indx <- forecast(arima_model_lvstk_prod_indx, h = 5)  # forecast for the next 5 periods
# Plot the forecast
autoplot(forecast_lvstk_prod_indx) +
  labs(title = "ARIMA Forecast of lvstk_prod_indx", x = "Year", y = "lvstk_prod_indx")
# Check residuals for model diagnostics
checkresiduals(arima_model_lvstk_prod_indx)

#Here, in our dataset we can plot the time series of the increase of the feature "lnd_und_crl_prod" by time, "year_of_value"
# Converting our data to time series object
time_series_data_lnd_und_crl_prod <- ts(Agri_resources_data$lnd_und_crl_prod, start = 2012, frequency = 1)
plot.ts(time_series_data_lnd_und_crl_prod, main = "Time Series Plot of lnd_und_crl_prod", xlab = "Year", ylab = "lnd_und_crl_prod")
autoplot(time_series_data_lnd_und_crl_prod) +
  labs(title = "Time Series Plot of lnd_und_crl_prod", x = "Year", y = "lnd_und_crl_prod")
# Decompose the time series to identify trend, seasonality, and remainder components. Here, our time series is too small, hence decomposition is not required. Thus quitting the stage of time-series analysis.
# Fit an ARIMA model
arima_model_lnd_und_crl_prod <- auto.arima(time_series_data_lnd_und_crl_prod)
# Print model summary
summary(arima_model_lnd_und_crl_prod)
# Forecast future values
forecast_lnd_und_crl_prod <- forecast(arima_model_lnd_und_crl_prod, h = 5)  # forecast for the next 5 periods
# Plot the forecast
autoplot(forecast_lnd_und_crl_prod) +
  labs(title = "ARIMA Forecast of lnd_und_crl_prod", x = "Year", y = "lnd_und_crl_prod")
# Check residuals for model diagnostics
checkresiduals(arima_model_lnd_und_crl_prod)

#Here, in our dataset we can plot the time series of the increase of the feature "lnd_und_crl_prod" by time, "year_of_value"
# Converting our data to time series object
time_series_data_tot_fish_prod <- ts(Agri_resources_data$tot_fish_prod, start = 2012, frequency = 1)
plot.ts(time_series_data_tot_fish_prod, main = "Time Series Plot of tot_fish_prod", xlab = "Year", ylab = "tot_fish_prod")
autoplot(time_series_data_tot_fish_prod) +
  labs(title = "Time Series Plot of tot_fish_prod", x = "Year", y = "tot_fish_prod")
# Decompose the time series to identify trend, seasonality, and remainder components. Here, our timeseries is too small, hence decomposition is not required. Thus quitting the stage of time-series analysis.
# Fit an ARIMA model
arima_model_tot_fish_prod <- auto.arima(time_series_data_tot_fish_prod)
# Print model summary
summary(arima_model_tot_fish_prod)
# Forecast future values
forecast_tot_fish_prod <- forecast(arima_model_tot_fish_prod, h = 5)  # forecast for the next 5 periods
# Plot the forecast
autoplot(forecast_tot_fish_prod) +
  labs(title = "ARIMA Forecast of tot_fish_prod", x = "Year", y = "tot_fish_prod")
# Check residuals for model diagnostics
checkresiduals(arima_model_tot_fish_prod)

#------------------------------------------------------------------------------------------
