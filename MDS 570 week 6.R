library(fpp3)

## Ch. 9 
# Use ARIMA() to find an appropriate ARIMA model. What model was selected?
# Check that the residuals look like white noise. Plot forecasts for the next 10 periods.
aus_airpassengers %>% autoplot(Passengers)

fit <- aus_airpassengers %>% 
  model(arima = ARIMA(Passengers))
report(fit)
view(aus_airpassengers)

fit %>% gg_tsresiduals()

fit %>% forecast(h = 10) %>% autoplot(aus_airpassengers)

# Plot forecasts from an ARIMA(0,1,0) model with drift
# and compare these to previous selected ARIMA.
aus_airpassengers %>% 
  model(arima = ARIMA(Passengers ~ 1 + pdq(0,1,0))) %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers)

# Plot forecasts from an ARIMA(2,1,2) model with drift
# and compare these to part b. Remove the constant and see what happens.
aus_airpassengers %>% 
  model(arima = ARIMA(Passengers ~ 1 + pdq(2,1,2))) %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers)

# should cause an error
aus_airpassengers %>% 
  model(arima = ARIMA(Passengers ~ 0 + pdq(2,1,2)))

# Plot forecasts from an ARIMA(0,2,1) model with a constant.
aus_airpassengers %>% 
  model(arima = ARIMA(Passengers ~ 1 + pdq(0,2,1))) %>%
  forecast(h = 10) %>%
  autoplot(aus_airpassengers)


# For the US GDP series (from global_economy): If necessary, 
# find a suitable Box-Cox transformation for the data.
us_economy <- global_economy %>%
  filter(Code == "USA")
us_economy %>%
  autoplot(GDP)
view(us_economy)

us_economy %>%
  features(GDP, features = guerrero)
us_economy %>%
  autoplot(box_cox(GDP, 0.282))

# Fit a suitable ARIMA model to the transformed data using ARIMA()
fit <- us_economy %>%
  model(ARIMA(box_cox(GDP, 0.282)))
report(fit)

# try some other plausible models by experimenting with the orders chosen;
fit <- us_economy %>%
  model(
    arima010 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(0, 1, 0)),
    arima011 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(0, 1, 1)),
    arima012 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(0, 1, 2)),
    arima013 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(0, 1, 3)),
    arima110 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(1, 1, 0)),
    arima111 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(1, 1, 1)),
    arima112 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(1, 1, 2)),
    arima113 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(1, 1, 3)),
    arima210 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(2, 1, 0)),
    arima211 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(2, 1, 1)),
    arima212 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(2, 1, 2)),
    arima213 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(2, 1, 3)),
    arima310 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(3, 1, 0)),
    arima311 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(3, 1, 1)),
    arima312 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(3, 1, 2)),
    arima313 = ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(3, 1, 3))
  )

# choose what you think is the best model and check the residual diagnostics;
fit %>%
  glance() %>%
  arrange(AICc) %>%
  select(.model, AICc)

# best model based on AICc values.
best_fit <- us_economy %>%
  model(ARIMA(box_cox(GDP, 0.282) ~ 1 + pdq(1, 1, 0)))
best_fit %>% report()

# check residuals
best_fit %>% gg_tsresiduals()
augment(best_fit) %>% features(.innov, ljung_box, dof = 2, lag = 10)

# Produce forecasts of your fitted model.
best_fit %>%
  forecast(h = 10) %>%
  autoplot(us_economy)

# chosen model compared to one with no transformation
us_economy %>%
  model(
    ARIMA(GDP),
    ARIMA(box_cox(GDP, 0.282))
  ) %>%
  forecast(h = 20) %>%
  autoplot(us_economy)

# compare the results with what you would obtain using ETS() (with no transformation).
us_economy %>%
  model(
    ETS(GDP),
    ARIMA(box_cox(GDP, 0.282))) %>%
  forecast(h = 10) %>%
  autoplot(us_economy)






