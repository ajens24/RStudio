library(fpp3)

rocktemp <- readr::read_csv('C:\\Users\\kacie\\Documents\\Elmhurst\\A.jensen\\Forecast Modeling\\Weather data - MDS 570 project.csv')
View(rocktemp)
str(rocktemp)

# Build Time Series and extract data of interest
rock <- rocktemp %>% tsibble(index = DATE, frequency= 7) %>% select(TMAX, TMIN)

ggtsdisplay(rock$TMAX)

view(rock)

rock %>%
  pivot_longer(-DATE, names_to="Key", values_to="Value") %>%
  ggplot(aes(x = DATE, y = Value, colour = Key)) +
  geom_line()+
  labs(title = "Rockford Temperatures",
     subtitle= "Period: Ten Years",
     y = "Degree°F")+
  facet_grid(vars(Key), scales = "free_y")

#Seasonal subseries
rock %>%
  gg_subseries(y= TMAX, period= 30, fill_gaps(=TRUE))+
  labs(title = "Rockford Temperatures",
       subtitle= "High Temperature subseries",
       y = "Degree°F")




# STL decomposition to calculate the trend-cycle and seasonal indices
rock %>%
  model(STL(TMAX)) %>%
  components() %>%
  autoplot()

# Auto correlation function
rock %>%
  ACF(TMAX, lag_max = 50) %>%
  autoplot()


# Define and estimate a model
rockmean <- rock %>% model(MEAN(TMIN))
rocksnaive <- rock %>% model(SNAIVE(TMIN ~ lag("1 years")))
rocknaive <- rock %>% model(NAIVE(TMIN))
rockdrift <- rock %>% model(RW(TMIN ~ drift()))

# Look at some forecasts
rockmean %>%
  forecast(h= "1 year") %>%
  autoplot(rock)+
  labs(title = "Rockford Temperatures",
       subtitle= "Average forecast",
       y = "Lows in °F")

# Look at the residuals
rocksnaive %>% gg_tsresiduals()


view(rock)

# Create a training set by withholding the last 3 years as a test set.
takeaway <- rock %>%
  filter(year(DATE) >= 2010)
train <- takeaway %>%
  filter(DATE <= "2018-09-01")

# benchmark methods
fit1 <- train %>%
  model(
    naive = NAIVE(TMIN),
    drift = RW(TMIN ~ drift()),
    mean = MEAN(TMIN),
    snaive = SNAIVE(TMIN ~ lag("year"))
  )
fc <- fit1 %>% forecast(h = "1 years")

# Look a some forecasts
fit1 %>% forecast(h= "1 years") %>% autoplot(train)

# Performance accuracy between models
fc %>%
  accuracy(takeaway) %>%
  arrange(MASE)

# Look at the residuals
fit1 %>%
  select(snaive)%>%
  gg_tsresiduals()

# Look at the prediction intervals
fit1%>% forecast() %>% hilo()





# Use the ETS() function to estimate the equivalent model for simple exponential
# smoothing. Select an appropriate model:
aus_trips <- rock %>%
  summarise(Highs = sum(TMAX))
aus_trips %>%
  model(ETS(Highs)) %>%
  report()
aus_trips %>%
  model(ETS(Highs ~ error("A") + trend("N") + season("A"))) %>%
  forecast(h= 365) %>%
  autoplot(aus_trips)

# Decompose the series using STL and obtain the seasonally adjusted data
dcmp <- rock %>%
  model(STL(TMAX)) %>%
  components()
dcmp %>%
  as_tsibble() %>%
  autoplot(season_adjust)

# Forecast the next two years of the series using an additive damped trend
# method applied to the seasonally adjusted data
stletsdamped <- decomposition_model(
  STL(TMAX),
  ETS(season_adjust ~ error("A") + trend("Ad") + season("N"))
)
rock %>%
  model(dcmp_AAdN = stletsdamped) %>%
  forecast(h = "1 years") %>%
  autoplot(rock)

# Forecast the next two years of the series using an appropriate model for 
# Holt's linear method applied to the seasonally adjusted data.
stletstrend <- decomposition_model(
  STL(TMAX),
  ETS(season_adjust ~ error("A") + trend("N") + season("A"))
)
rock %>%
  model(dcmp_ANA = stletstrend) %>%
  forecast(h = "1 years") %>%
  autoplot(rock)

# Now use ETS() to choose a seasonal model for the data
rock %>%
  model(ets = ETS(TMAX)) %>%
  forecast(h = "1 years") %>%
  autoplot(rock)

# View accuracy..Compare the RMSE of the ETS model with the RMSE of
# the models you obtained using STL decompositions
fit <- rock %>%
  model(
    dcmp_AAdN = stletsdamped,
    dcmp_ANA = stletstrend,
    ets = ETS(TMAX),
    snaive = SNAIVE(TMAX ~ lag("year"))
  )
accuracy(fit)

# Compare the forecasts from the three approaches.
fit %>%
  forecast(h = "1 year") %>%
  autoplot(rock, , level = NULL)

# View residuals
best <- fit %>%
  select(dcmp_AAdN)
augment(best) %>% gg_tsdisplay(.resid, lag_max = 24, plot_type = "histogram")

augment(best) %>%
  features(.innov, ljung_box, lag = 24, dof = 4)



newrock <- rock %>%
  filter(year(DATE) >= 2011)
dcmp1 <- newrock %>%
  model(STL(TMAX ~ trend(window = 365.25), robust = TRUE)) %>%
  components() %>%
  select(-.model)
dcmp1 %>%
  model(NAIVE(season_adjust)) %>%
  forecast(h = 365) %>%
  autoplot(dcmp1) +
  labs(y = "Temp (Highs)",
       title = "Rockford High temps")

fit_dcmp <- newrock %>%
  model(stlf = decomposition_model(
    STL(TMAX ~ trend(window = 365.25), robust = TRUE),
    NAIVE(season_adjust)
  ))
fit_dcmp %>%
  forecast(h= "1 year") %>%
  autoplot(newrock)+
  labs(y = "Temp (Highs)",
       title = "Rockford High temps")

accuracy(fit_dcmp)
accuracy(fit)




# ARIMA modeling
# Unitroot test (is differencing required)
train %>% 
  features(TMAX, unitroot_kpss)
# NUmber of first differences required
train %>%
  features(TMAX, unitroot_ndiffs)
# NUmber of seasonal differences required
train %>%
  features(TMAX, unitroot_ndiffs)

rock %>% autoplot(TMAX)


afit %>% forecast(h = "1 year") %>% autoplot(train)

# Seasonal differencing (since seasonality is present)
train %>% autoplot(TMAX %>% difference(lag= 12))

# First differencing applied second
train %>% autoplot(difference(TMAX, 12) %>% difference())

# View transformed/seasonally differenced/first differenced ACF/PACF plots
train %>%
  gg_tsdisplay(TMAX %>% difference(12) %>% 
                 difference(), plot_type = "partial")

# Look at forecasts
afit <- train %>% 
  model(ARIMA(TMAX))

report(afit)

afit %>% gg_tsresiduals()



# try some other plausible models by experimenting with the orders chosen;
arimafit <- train %>%
  model(
    arima010 = ARIMA(TMAX ~ 0 + pdq(0, 0, 1) + PDQ(0, 0, 1)),
    arima011 = ARIMA(TMAX ~ 0 + pdq(0, 0, 1) + PDQ(0, 0, 1)),
    arima012 = ARIMA(TMAX ~ 0 + pdq(0, 0, 2) + PDQ(0, 0, 2)),
    arima013 = ARIMA(TMAX ~ 0 + pdq(0, 0, 3) + PDQ(0, 0, 3)),
    arima110 = ARIMA(TMAX ~ 0 + pdq(1, 0, 0) + PDQ(1, 0, 0)),
    arima111 = ARIMA(TMAX ~ 0 + pdq(1, 0, 1) + PDQ(1, 0, 1)),
    arima112 = ARIMA(TMAX ~ 0 + pdq(1, 0, 2) + PDQ(1, 0, 2)),
    arima113 = ARIMA(TMAX ~ 0 + pdq(1, 0, 3) + PDQ(1, 0, 3)),
    arima210 = ARIMA(TMAX ~ 0 + pdq(2, 0, 0) + PDQ(2, 0, 0)),
    arima211 = ARIMA(TMAX ~ 0 + pdq(2, 0, 1) + PDQ(2, 0, 1)),
    arima212 = ARIMA(TMAX ~ 0 + pdq(2, 0, 2) + PDQ(2, 0, 2)),
    arima213 = ARIMA(TMAX ~ 0 + pdq(2, 1, 3) + PDQ(2, 0, 3)),
    arima310 = ARIMA(TMAX ~ 0 + pdq(3, 0, 0) + PDQ(3, 0, 0)),
    arima311 = ARIMA(TMAX ~ 0 + pdq(3, 0, 1) + PDQ(3, 0, 1)),
    arima312 = ARIMA(TMAX ~ 0 + pdq(3, 0, 2) + PDQ(3, 0, 2)),
    arima313 = ARIMA(TMAX ~ 0 + pdq(3, 0, 3) + PDQ(3, 0, 3)),
    arima313 = ARIMA(TMAX ~ 0 + pdq(3, 1, 3) + PDQ(3, 0, 3)),
    arima0101 = ARIMA(TMAX ~ 0 + pdq(0, 0, 1) + PDQ(3, 0, 3)),
    arima0112 = ARIMA(TMAX ~ 0 + pdq(0, 0, 1) + PDQ(3, 0, 2)),
    arima0123 = ARIMA(TMAX ~ 0 + pdq(0, 0, 2) + PDQ(3, 0, 1)),
    arima0134 = ARIMA(TMAX ~ 0 + pdq(0, 0, 3) + PDQ(0, 0, 3)),
    arima1105 = ARIMA(TMAX ~ 0 + pdq(1, 0, 0) + PDQ(3, 0, 0)),
    arima1116 = ARIMA(TMAX ~ 0 + pdq(1, 0, 1) + PDQ(2, 0, 3)),
    arima1127 = ARIMA(TMAX ~ 0 + pdq(1, 0, 2) + PDQ(2, 0, 1)),
    arima1138 = ARIMA(TMAX ~ 0 + pdq(1, 0, 3) + PDQ(2, 0, 0)),
    arima2109 = ARIMA(TMAX ~ 0 + pdq(2, 0, 0) + PDQ(1, 0, 3)),
    arima0211 = ARIMA(TMAX ~ 0 + pdq(2, 0, 1) + PDQ(1, 0, 2)),
    arima1212 = ARIMA(TMAX ~ 0 + pdq(2, 0, 2) + PDQ(1, 0, 1)),
    arima2213 = ARIMA(TMAX ~ 0 + pdq(2, 0, 3) + PDQ(1, 0, 0)),
    arima3310 = ARIMA(TMAX ~ 0 + pdq(3, 0, 0) + PDQ(0, 0, 3)),
    arima4311 = ARIMA(TMAX ~ 0 + pdq(3, 0, 1) + PDQ(0, 0, 2)),
    arima5312 = ARIMA(TMAX ~ 0 + pdq(3, 0, 2) + PDQ(0, 0, 1)),
    arima6313 = ARIMA(TMAX ~ 0 + pdq(3, 0, 3) + PDQ(1, 0, 1))
  )

# choose what you think is the best model and check the residual diagnostics;
arimafit %>%
  glance() %>%
  arrange(AICc) %>%
  select(.model, AICc)

# best model based on AICc values.
best_fit <- rock %>%
  model(ARIMA(TMAX ~ 0 + pdq(3, 0, 3) + PDQ(3, 0, 3)))
best_fit %>% report()

# check residuals
best_fit %>% gg_tsresiduals()
augment(best_fit) %>% features(.innov, ljung_box, dof = 2, lag = 10)

# Produce forecasts of your fitted model.
best_fit %>%
  forecast(h= 365) %>%
  autoplot(train)



