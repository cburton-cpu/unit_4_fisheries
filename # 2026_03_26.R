# 2026-03-26
# CCB

#load in data via relative path
library(tidyverse)
load("data/RAMLDB v4.66/R Data/DBdata[asmt][v4.66].RData")

# logistic and binomial regressions are the same thingyy

# you can run everything in that .r file using the source() function
source("build_collapse_table.r")

glimpse(collapse)
head(metadata)

# has a given stock ever collapsed??
model_data = collapse %>%
    group_by(stockid, stocklong) %>%
    summarize(ever_collapsed = any(current_collapse)) %>% # any will tell if you if even 1 of those is true
    ungroup() %>%
    left_join(metadata, by = c("stockid", "stocklong")) %>%
    mutate(FisheryType = as.factor(FisheryType)) # as.factor --> got rid of the quotes, that's how you can tell it went from being read as characters to 1 factor

summary(metadata)

glimpse(model_data)

model_l = glm(data = model_data, ever_collapsed ~ FisheryType, family = "binomial") # glm is statsssss
summary(model_l)
# in the summary, the estimae std. column -- the 2nd line and beyond is the CORRECTION applied to the first factor

newdata = model_data %>% distinct(FisheryType) %>% arrange(FisheryType)
newdata

class(model_l)
?predict.glm

# predictions
model_l_predict = predict(model_l, newdata = newdata, type = "response", se.fit = T)
collapse_fishery_type_prediction = cbind(newdata, model_l_predict)
collapse_fishery_type_prediction

# PLOT YOURE SO AMAZING!!!
ggplot(data = collapse_fishery_type_prediction) +
  geom_bar(aes(x = FisheryType, y = fit, fill = FisheryType), stat = "identity", show.legend = F) + # have to use geom_bar() because we chose a categorical not a continuous variable
  geom_errorbar(aes(x = FisheryType, ymin = fit - se.fit, ymax = fit + se.fit), width = 0.2) +
  coord_flip() + # makes it easier to read
  theme_minimal() # play aroundddd

# gotta use poisson when zero is a number that matters and you CANNOT use linear because having a negative number is IMPOSSIBLE
# hurdle model --> linear (presence/absense), poisson (for counts)
# might model the variance instead of the magnitude and inflate how significant your variable(s) are


### poisson model -- lord help us

glimpse(tsmetrics)
tsmetrics %>% filter(tsshort == "BdivBmgtpref")
# MSY -- maximum sustainable yield
tsmetrics %>% filter(tsshort == "UdivUmsypref")

u_summary = timeseries_values_views %>% 
      filter(!is.na(BdivBmgtpref), 
            !is.na(UdivUmsypref)) %>%
      group_by(stockid, stocklong) %>%
      summarize(yrs_data = n(), 
                  ratio_yrs_overfished = sum(UdivUmsypref > 1)/yrs_data,
                    ratio_yrs_low_stock = sum(BdivBmgtpref < 1)/yrs_data) %>%
      select(-yrs_data) %>%
      ungroup() %>%
      left_join(metadata %>% select(stockid, FisheryType))

glimpse(u_summary)


## join it with the collapse
collapse_summary = collapse %>%
      group_by(stockid, stocklong) %>%
      summarize(yrs_data = n(),
                  yrs_collapsed = sum(current_collapse)) %>%
      ungroup() %>%
      inner_join(u_summary, by = c("stockid", "stocklong"))

glimpse(collapse_summary)


hist(collapse_summary$yrs_collapsed)
table(collapse_summary$yrs_collapsed)

collapse_summary_zero_trunc = collapse_summary %>% filter(yrs_collapsed > 0)
table(collapse_summary_zero_trunc$yrs_collapsed)

# simple
model_p = glm(yrs_collapsed ~ FisheryType, offset(log(yrs_data)), 
              data = collapse_summary_zero_trunc,
              family = "poisson")
summary(model_p)

# complex
model_p = glm(yrs_collapsed ~ FisheryType + ratio_yrs_overfished + ratio_yrs_low_stock, 
          offset(log(yrs_data)), 
              data = collapse_summary_zero_trunc,
              family = "poisson")
summary(model_p)

install.packages("AER")
library(AER)
AER::dispersiontest(model_p)

# dealing with over dispersion
model_qp = glm(yrs_collapsed ~ FisheryType + ratio_yrs_overfished + ratio_yrs_low_stock, 
          offset(log(yrs_data)), 
              data = collapse_summary_zero_trunc,
              family = "quasipoisson")
summary(model_qp)
# can switch the order of your independent variables and get the same result

# plot the prediction

median_ratio_yrs_low = median(collapse_summary_zero_trunc$ratio_yrs_low_stock)
median_ratio_yrs_low
FisheryType = collapse_summary_zero_trunc %>% distinct(FisheryType) %>% pull
newdata = expand.grid(FisheryType = FisheryType,
                        ratio_yrs_low_stock = median_ratio_yrs_low,
                        ratio_yrs_overfished = seq(0, 1, by = 0.1))
newdata
