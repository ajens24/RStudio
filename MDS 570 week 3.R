library(fpp3)

# Extract data of interest
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)

# Define and estimate a model
fit <- recent_production %>% model(SNAIVE(Beer))

# Look at the residuals
fit %>% gg_tsresiduals()

# Look a some forecasts
fit %>% forecast() %>% autoplot(recent_production)

# Look at the prediction intervals
fit %>% forecast() %>% hilo()

## CH.5 #10

# Create a training set for Australian takeaway food turnover (aus_retail)
# by withholding the last four years as a test set.
takeaway <- aus_retail %>%
  filter(Industry == "Takeaway food services") %>%
  summarise(Turnover = sum(Turnover))
train <- takeaway %>%
  filter(Month <= max(Month) - 4 * 12)

# Benchmark methods 
fit <- train %>%
  model(
    naive = NAIVE(Turnover),
    drift = RW(Turnover ~ drift()),
    mean = MEAN(Turnover),
    snaive = SNAIVE(Turnover)
  )
fc <- fit %>% forecast(h = "4 years")

# Performance accuracy betweens models
fc %>%
  accuracy(takeaway) %>%
  arrange(MASE)

# View residuals
fit %>%
  select(naive) %>%
  gg_tsresiduals()

## CH. 5 #11

view(aus_production)
# filter na brick values out
Aus <- aus_production %>%
  filter(!is.na(Bricks))
view(Aus)

# plot STL decomposition
Aus %>%
  model(STL(Bricks)) %>%
  components() %>%
  autoplot()

# When data is multiplicative, use a transformation
dcmp <- Aus %>%
  model(STL(log(Bricks))) %>%
  components()
dcmp %>%
  autoplot()

# Using a periodic season (window), since the seasonality looks stable
dcmp <- Aus %>%
  model(stl = STL(log(Bricks) ~ season(window = "periodic"))) %>%
  components()
dcmp %>% autoplot()

# Plot the seasonaly adjusted data
dcmp %>%
  as_tsibble() %>%
  autoplot(season_adjust)

# Naïve method of seasonally adjusted data
fit <- dcmp %>%
  select(-.model) %>%
  model(naive = NAIVE(season_adjust)) %>%
  forecast(h = "3 years")
dcmp %>%
  as_tsibble() %>%
  autoplot(season_adjust) + autolayer(fit)

# Decomposition to re-seasonalize the results/
# Change back to original data from transformed data
fit <- Aus %>%
  model(stl_mdl = decomposition_model(STL(log(Bricks)), NAIVE(season_adjust)))
fit %>%
  forecast(h = "3 years") %>%
  autoplot(Aus)

# view residuals
fit %>% gg_tsresiduals()

# Repeat with a robust STL decomposition model
fit_robust <- Aus %>%
  model(stl_mdl = decomposition_model(STL(log(Bricks)), NAIVE(season_adjust)))
fit_robust %>% gg_tsresiduals()

# Compare forecasts from decomposition_model() with those from
# SNAIVE(), using a test set comprising the last 2 years of data. 
tidy_bricks_train <- tidy_bricks %>%
  slice(1:(n() - 8))
fit <- tidy_bricks_train %>%
  model(
    stl_mdl = decomposition_model(STL(log(Bricks)), NAIVE(season_adjust)),
    snaive = SNAIVE(Bricks)
  )

fc <- fit %>%
  forecast(h = "2 years")
fc %>%
  autoplot(tidy_bricks, level = NULL)

# Performance accuracy between models
fc %>%
  accuracy(tidy_bricks)






