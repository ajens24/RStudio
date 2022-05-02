library(fpp3)

## CH. 8

# Use the ETS() function to estimate the equivalent model for simple exponential
# smoothing. Find the optimal values of ?? and ???0, and generate forecasts.
fit <- aus_livestock %>%
  filter(Animal == "Pigs", State == "Victoria") %>%
  model(ses = ETS(Count ~ error("A") + trend("N") + season("N")))
report(fit)

fc <- fit %>% forecast(h = "4 months")
fc
fc %>%
  autoplot(filter(aus_livestock, Month >= yearmonth("2010 Jan")))


#(#5) Plot Exports from global economy
view(global_economy)
global_economy %>%
  filter(Country == "Argentina") %>%
  autoplot(Exports) 

# Use an ETS(A,N,N) model to forecast the series, and plot the forecasts
etsANN <- global_economy %>%
  filter(Country == "Argentina") %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))
etsANN %>%
  forecast(h = 10) %>%
  autoplot(global_economy)

# View accuracy..Compute the RMSE values for the training data.
accuracy(etsANN) %>% select(RMSE)
accuracy(etsANN)

# Compare the results to those from an ETS(A,A,N) model.
fit <- global_economy %>%
  filter(Country == "Argentina") %>%
  model(
    ses = ETS(Exports ~ error("A") + trend("N") + season("N")),
    holt = ETS(Exports ~ error("A") + trend("A") + season("N"))
  )
accuracy(fit)

# Compare the forecasts from both methods
fit %>%
  forecast(h = 10) %>%
  autoplot(global_economy)

#(#10) Compute and plot total trips in the tourism dataset.
view(tourism)
aus_trips <- tourism %>%
  summarise(Trips = sum(Trips))
aus_trips %>%
  autoplot(Trips)

# Decompose the series using STL and obtain the seasonally adjusted data
dcmp <- aus_trips %>%
  model(STL(Trips)) %>%
  components()
dcmp %>%
  as_tsibble() %>%
  autoplot(season_adjust)

# Forecast the next two years of the series using an additive damped trend
# method applied to the seasonally adjusted data
stletsdamped <- decomposition_model(
  STL(Trips),
  ETS(season_adjust ~ error("A") + trend("Ad") + season("N"))
)
aus_trips %>%
  model(dcmp_AAdN = stletsdamped) %>%
  forecast(h = "2 years") %>%
  autoplot(aus_trips)

# Forecast the next two years of the series using an appropriate model for 
# Holt's linear method applied to the seasonally adjusted data.
stletstrend <- decomposition_model(
  STL(Trips),
  ETS(season_adjust ~ error("A") + trend("A") + season("N"))
)
aus_trips %>%
  model(dcmp_AAN = stletstrend) %>%
  forecast(h = "2 years") %>%
  autoplot(aus_trips)

# Now use ETS() to choose a seasonal model for the data
aus_trips %>%
  model(ets = ETS(Trips)) %>%
  forecast(h = "2 years") %>%
  autoplot(aus_trips)

# View accuracy..Compare the RMSE of the ETS model with the RMSE of
# the models you obtained using STL decompositions
fit <- aus_trips %>%
  model(
    dcmp_AAdN = stletsdamped,
    dcmp_AAN = stletstrend,
    ets = ETS(Trips)
  )
accuracy(fit)

# Compare the forecasts from the three approaches.
fit %>%
  forecast(h = "2 years") %>%
  autoplot(aus_trips, level = NULL)

# View residuals
best <- fit %>%
  select(dcmp_AAN)
augment(best) %>% gg_tsdisplay(.resid, lag_max = 24, plot_type = "histogram")

augment(best) %>%
  features(.innov, ljung_box, lag = 24, dof = 4)


## (#13) Compare ETS(), SNAIVE() and decomposition_model(STL, ???) on the following
# five time series. You might need to use a Box-Cox transformation for the STL
# decomposition forecasts. Use a test set of three years to decide 
# what gives the best forecasts.

# Beer production from aus_production
fc <- aus_production %>%
  filter(Quarter < max(Quarter - 11)) %>%
  model(
    ETS(Beer),
    SNAIVE(Beer),
    stlm = decomposition_model(STL(log(Beer)), ETS(season_adjust))
  ) %>%
  forecast(h = "3 years")
fc %>%
  autoplot(filter_index(aus_production, "2000 Q1" ~ .), level = NULL)

# view accuracy
fc %>% accuracy(aus_production)

# Bricks production from aus_production
tidy_bricks <- aus_production %>%
  filter(!is.na(Bricks))
fc <- tidy_bricks %>%
  filter(Quarter < max(Quarter - 11)) %>%
  model(
    ets = ETS(Bricks),
    snaive = SNAIVE(Bricks),
    STLM = decomposition_model(STL(log(Bricks)), ETS(season_adjust))
  ) %>%
  forecast(h = "3 years")
fc %>% autoplot(filter_index(aus_production, "1980 Q1" ~ .), level = NULL)

# View accuracy 
fc %>% accuracy(tidy_bricks)

# Cost of drug subsidies for diabetes (ATC2 == "A10")
# and corticosteroids (ATC2 == "H02") from PBS
subsidies <- PBS %>%
  filter(ATC2 %in% c("A10", "H02")) %>%
  group_by(ATC2) %>%
  summarise(Cost = sum(Cost))
subsidies %>%
  autoplot(vars(Cost)) +
  facet_grid(vars(ATC2), scales = "free_y")

fc <- subsidies %>%
  filter(Month < max(Month) - 35) %>%
  model(
    ETS(Cost), 
    SNAIVE(Cost), 
    STLM = decomposition_model(STL(log(Cost)), ETS(season_adjust))) %>%
  forecast(h = "3 years")
fc %>% autoplot(subsidies, level = NULL)

# VIew accuracy
fc %>%
  accuracy(subsidies) %>%
  arrange(ATC2)

# Total food retailing turnover for Australia from aus_retail
food_retail <- aus_retail %>%
  filter(Industry == "Food retailing") %>%
  summarise(Turnover = sum(Turnover))

fc <- food_retail %>%
  filter(Month < max(Month) - 35) %>%
  model(
    ETS(Turnover),
    SNAIVE(Turnover),
    STLM = decomposition_model(STL(log(Turnover)), ETS(season_adjust))
  ) %>%
  forecast(h = "3 years")
fc %>%
  autoplot(filter_index(food_retail, "2005 Jan" ~ .), level = NULL)

# View accuracy
fc %>% accuracy(food_retail)


# (#14) Use ETS() to select an appropriate model for the following series: 
# total number of trips across Australia using tourism
aus_trips <- tourism %>%
  summarise(Trips = sum(Trips))
aus_trips %>%
  model(ETS(Trips)) %>%
  report()
aus_trips %>%
  model(ETS(Trips)) %>%
  forecast() %>%
  autoplot(aus_trips)

# closing prices for the four stocks in gafa_stock
gafa_regular <- gafa_stock %>%
  group_by(Symbol) %>%
  mutate(trading_day = row_number()) %>%
  ungroup() %>%
  as_tsibble(index = trading_day, regular = TRUE)

gafa_stock %>% autoplot(Close)

gafa_regular %>%
  model(ETS(Close))

gafa_regular %>%
  model(ETS(Close)) %>%
  forecast(h = 50) %>%
  autoplot(gafa_regular %>% group_by_key() %>% slice((n() - 100):n()))

# lynx series in pelt
pelt %>%
  model(ETS(Lynx))
pelt %>%
  model(ETS(Lynx)) %>%
  forecast(h = 10) %>%
  autoplot(pelt)


