library(fpp3)

view(global_economy)

us_economy <- global_economy %>%
  filter(Country == "United States")
us_economy %>%
  autoplot(GDP)+
  labs(title = "United States GDP") 
us_economy %>%
  features(GDP, features = guerrero)
us_economy %>%
  autoplot(box_cox(GDP, 0.282))

view(aus_livestock)
vic_bulls <- aus_livestock %>%
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria")
vic_bulls %>%
  autoplot(Count)+
  labs(title = "Slaughter of Victorian")
vic_bulls %>%
  features(Count, features = guerrero)
vic_bulls %>%
  autoplot(box_cox(Count, -0.072))

view(vic_elec)  
vic_elec %>%
  autoplot(Demand)+
  labs(title = "Victorian Electricity Demand")

view(aus_production)
aus_production %>%
  autoplot(Gas)+
  labs(title= "Gas production")
aus_production %>%
  features(Gas, features = guerrero)
aus_production %>%
  autoplot(box_cox(Gas, 0.121))

gas <- tail(aus_production, 5*4) %>% select(Gas)
  autoplot(gas)
decomp <- gas %>%
  model(decomp = classical_decomposition(Gas, type = "multiplicative")) %>%
  components()
decomp %>% autoplot()
as_tsibble(decomp) %>%
  autoplot(season_adjust) +
  labs(title = "Seasonally adjusted data", y = "Petajoules")
gas %>%
  mutate(Gas = if_else(Quarter == yearquarter("2007Q4"), Gas + 300, Gas)) %>%
  model(decomp = classical_decomposition(Gas, type = "multiplicative")) %>%
  components() %>%
  as_tsibble() %>%
  autoplot(season_adjust) +
  labs(title = "Seasonally adjusted data", y = "Petajoules")

install.packages('GGally')
library(GGally)
tourism %>%
  features(Trips, feat_stl) %>%
  select(-Region, -State, -Purpose) %>%
  mutate(
    seasonal_peak_year = factor(seasonal_peak_year),
    seasonal_trough_year = factor(seasonal_trough_year),
  ) %>%
  ggpairs()

tourism %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips)) %>%
  features(Trips, feat_stl) %>%
  select(State, seasonal_peak_year)
