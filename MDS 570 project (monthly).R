library(fpp3)

rocktempmonth <- readr::read_csv('C:\\Users\\kacie\\Documents\\Elmhurst\\A.jensen\\Forecast Modeling\\Weather data - MDS 570 monthly.csv')
str(rocktempmonth)

# Build Time Series and extract data of interest
rockmonth <- rocktempmonth %>% tsibble(index = MonthYear, frequency= 12) %>%
  tsibble::fill_gaps(TMAXAVG, .full= FALSE) %>%
  select(TMAXAVG, TMAXMIN)


rockmonth2 <- ts(rocktempmonth[,2],start = c(2011,1),frequency = 12)
plot(rockmonth2, xlab='Year', ylab = 'Tempterature')
plot(diff(rockmonth2),ylab='Differenced Temperature')
plot(log10(rockmonth2),ylab='Log (Temperature)')
plot(diff(log10(rockmonth2)),ylab='Differenced (Temperature)')
install.packages('forecast', dependencies = TRUE)
ARIMAfit = auto.arima(rockmonth2, approximation=FALSE,trace=FALSE)
summary(ARIMAfit)





view(rockmonth)

# View data on time series format
rockmonth %>%
  pivot_longer(-MonthYear, names_to="Key", values_to="Value") %>%
  ggplot(aes(x = MonthYear, y = Value, colour = Key)) +
  geom_line()+
  labs(title = "Rockford Temperatures",
       subtitle= "Period: Ten Years",
       y = "Degree°F")+
  facet_grid(vars(Key), scales = "free_y")

#Seasonal subseries
rockmonth %>%
  gg_subseries(y= TMAXAVG, period= 12)+
  labs(title = "Rockford Temperatures",
       subtitle= "High Temperature subseries",
       y = "Degree°F")





# ARIMA modeling
# Unitroot test (is differencing required)
rockmonth %>% 
  features(TMAXAVG, unitroot_kpss)
# Number of first differences required
rockmonth %>%
  features(TMAXAVG, unitroot_ndiffs)
# Number of seasonal differences required
rockmonth %>%
  features(TMAXAVG, unitroot_ndiffs)



# Create a training set by withholding the last 3 years as a test set.
takeawaymonth <- rockmonth %>%
  filter(year(MonthYear) >= 2010)
train_month <- takeawaymonth %>%
  filter(MonthYear <= "2021-09-01")

count_gaps(train_month)

# Seasonal differencing (since seasonality is present)
train_month %>% autoplot(TMAXAVG %>% difference(lag= 12))


# First differencing applied after seasonal differencing
train_month %>% autoplot(TMAXAVG %>% difference(lag= 12) %>% difference())

# View seasonally differenced/first differenced ACF/PACF plots
train_month %>%
  gg_tsdisplay(TMAXAVG %>% difference(lag= 12)%>% 
                 difference(), plot_type = "partial")

# Look at forecasts
afit <- train_month %>% 
  model(ARIMA(TMAXAVG ~ 0 + pdq(3,0,1)))
report(afit)

afit %>% gg_tsresiduals()