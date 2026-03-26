# 2026-03-24
# CCB

#load in data via relative path
library(tidyverse)
load("data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData")

head(tsmetrics)
head(timeseries)

timeseries_tsmetrics = timeseries %>%
    left_join(tsmetrics, by = c("tsid" = "tsunique")) # equate the column names
dim(timeseries)
dim(timeseries_tsmetrics)
head(timeseries_tsmetrics)


head(timeseries_values_views)
head(taxonomy)
glimpse(stock)

fish = timeseries_values_views %>%
    left_join(stock, by = c("stockid", "stocklong")) %>%
    left_join(taxonomy, by = c("tsn", "scientificname")) %>%
    select(stockid, stocklong, year, TCbest, tsn, scientificname, commonname, region, FisheryType, taxGroup)

# if you don't join by both then it will create 2 stocklong columns
glimpse(fish)
dim(fish)
dim(timeseries_values_views)

fish %>% arrange(desc(TCbest))

# prelim plot
ggplot() +
  geom_line(data = fish, 
    aes(x =year, y = TCbest, color = stockid)) +
  theme(legend.position = "none")

ggplot() +
  geom_line(data = fish %>% filter(TCbest >3e6), 
    aes(x =year, y = TCbest, color = stocklong)) 

glimpse(fish)
fish %>% 
    filter(scientificname == "Gadus morhua") %>%
    distinct(region)

cod_can = fish %>%
    filter(scientificname == "Gadus morhua", region == "Canada East Coast",
        !is.na(TCbest))
head(cod_can)    

ggplot(data = cod_can) +
  geom_line(aes(x = year, y = TCbest, color = stocklong)) +
    theme_bw() +
  ylab("Total catch in metric tons")

# add all cod stocks in eastern canada together
cod_can_total = cod_can %>%
  group_by(year) %>%
    summarize(total_catch = sum(TCbest))
head (cod_can_total)

ggplot(data = cod_can_total) +
  geom_line(aes(x = year, y = total_catch)) +
    theme_bw() +
  ylab("Total catch in metric tons")

### play with the cumulative functions in dplyr ###

dat = c(1, 3, 6, 2, 3, 9, -1)
dat_max = cummax(dat)
dat_sum = cumsum(dat)
test_cum = data.frame(dat, dat_max, dat_sum)
test_cum

# using Boris worm (2006) stock collapse definition - has cod collapsed?? :D

cod_collapse = cod_can_total %>%
    mutate(historical_max_catch = cummax(total_catch),
            collapse = total_catch <= 0.1 * historical_max_catch)
head(cod_collapse)
tail(cod_collapse)

cod_collapse_yr = cod_collapse %>%
      filter(collapse == TRUE) %>%
      summarize(year = min(year)) %>%
  pull()
cod_collapse_yr
class(cod_collapse_yr)
# had to add pull() function to make the resulting number numeric

ggplot() +
    geom_line(data = cod_collapse, aes(x= year, y= total_catch, color = collapse)) +
    geom_vline(xintercept = cod_collapse_yr) +
  theme_classic()

# period from 1992-1993, we have annual data = not a continuous line

# apply collapse to full dataset
head(fish)
collapse = fish %>%
    filter(!is.na(TCbest)) %>%
    group_by(stockid) %>%
    mutate(historical_max_catch = cummax(TCbest),
            current_collapse = TCbest <= 0.1 * historical_max_catch,
          collapsed_yet = cumsum(current_collapse) > 0) %>%
      ungroup()
# each year it collapse is 1, TRUE = 1, FALSE = 0
glimpse(collapse)

collapse_yr = collapse %>%
    group_by(stockid, stocklong, region) %>%
    filter(collapsed_yet == TRUE) %>%
    summarize(first_collapse_yr = min(year)) %>%
    ungroup()
glimpse(collapse_yr)

# timeseries of shame tsk tsk

n_stocks = length(unique(collapse$stockid))
collapse_ts = collapse_yr %>%
    group_by(first_collapse_yr) %>%
    summarize(n = n()) %>%
    mutate(cum_first_collapse_yr = cumsum(n),
            ratio_collapse_yet = cum_first_collapse_yr / n_stocks)
head(collapse_ts)

ggplot(data = collapse_ts) +
    geom_line(aes(x = first_collapse_yr, y = ratio_collapse_yet))
