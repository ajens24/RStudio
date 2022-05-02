library(fpp3)

# Viewing plot, ACF, and PACF
gafa_stock %>%
  filter(Symbol == "AMZN") %>%
  mutate(t = row_number()) %>%
  update_tsibble(index = t) %>%
  gg_tsdisplay(Close, plot_type = "partial")

## Obtaining stationary data for the following series
# *Turkish GDP from global_economy
turkey <- global_economy %>% filter(Country == "Turkey")
turkey %>% autoplot(GDP)

# Log transformation
turkey %>% autoplot(log(GDP))

# First differencing 
turkey %>% autoplot(log(GDP) %>% difference())

# *Accommodation takings in the state of Tasmania from aus_accommodation
tas <- aus_accommodation %>% filter(State == "Tasmania")
tas %>% autoplot(Takings)
view(tas)

# Transformation
tas %>% autoplot(log(Takings))

# Seasonal differencing (since seasonality is present)
tas %>% autoplot(log(Takings) %>% difference(lag= 12))

# First differencing applied second
tas %>% autoplot(log(Takings) %>% difference(lag = 12) %>% difference())


# *Monthly sales from souvenirs
view(souvenirs)

souvenirs %>% autoplot(Sales)

# Transformation
souvenirs %>% autoplot(log(Sales))

# Seasonal differencing (since seasonality is present)
souvenirs %>% autoplot(log(Sales) %>% difference(lag= 12))

# First differencing applied to achieve stationarity
souvenirs %>% autoplot(log(Sales) %>% difference(lag=12) %>% difference())

## find the appropriate order of differencing (after transformation
# if necessary) to obtain stationary data.
set.seed(100000)
myseries <- aus_retail %>%
  filter(
    `Series ID` == sample(aus_retail$`Series ID`, 1),
    Month < yearmonth("2018 Jan")
  )
myseries %>% autoplot(Turnover)

# Transformation
myseries %>% autoplot(log(Turnover))

# seasonal differencing applied
myseries %>% autoplot(log(Turnover) %>% difference(lag = 12))

# View transformed and seasonally differenced ACF/PACF plots to see
# if further differencing should take place.
myseries %>%
  gg_tsdisplay(log(Turnover) %>% difference(lag = 12), plot_type = "partial")

# First differencing applied to achieve stationarity
myseries %>% autoplot(log(Turnover) %>% difference(lag=12) %>% difference())

# View transformed/seasonally differenced/first differenced ACF/PACF plots
myseries %>%
  gg_tsdisplay(log(Turnover) %>% difference(lag = 12) %>% 
  difference(), plot_type = "partial")


