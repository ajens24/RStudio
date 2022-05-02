install.packages('fpp3', dependencies = TRUE)
install.packages('tidyverse', dependencies = TRUE)
install.packages("dplyr", dependecies = TRUE)
library('dplyr')
library('fpp3')
library('tidyverse')

tute1 <- readr::read_csv('C:\\Users\\kacie\\Documents\\Elmhurst\\A.jensen\\Forecast Modeling\\tute1.csv')
View(tute1)

tute1 %>%
  select(Sales, Quarter)

mytimeseries <- tute1 %>%
  mutate(Quarter = yearmonth(Quarter)) %>%
  as_tsibble(index = Quarter)
view(mytimeseries)
mytimeseries %>%
  pivot_longer(-Quarter, names_to="Key", values_to="Value") %>%
  ggplot(aes(x = Quarter, y = Value, colour = Key)) +
    geom_line()+
    facet_grid(vars(Key), scales = "free_y")


View(aus_retail)
set.seed(100000)
myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))
myseries %>%
  gg_season(Turnover)

myseries %>%
  ACF(Turnover, lag_max = 50) %>%
  autoplot()
