# 03-03-2026
# CCB

# load in data
# library(rstatix) # cor_test()
# library(GGally) 
library(tidyverse)
library(palmerpenguins)

summary(penguins)


penguins_lm_3 = penguins %>%
    filter(!is.na(bill_depth_mm))

# same thing
# lm_4 = lm(bill_depth_mm ~ bill_length_mm + species + bill_length_mm * species, data = penguins_lm_3) 
lm_4 = lm(bill_depth_mm ~ bill_length_mm * species, data = penguins_lm_3)
# lm_4 = lm(bill_depth_mm ~ bill_length_mm : species, data = penguins_lm_3) # it's unusual not to include the independent variables alongside the interaction

summary(lm_4)


AIC(lm_3, lm_4)
AIC(lm_4)
# difference in AICs using two different models from the SAME dataset, if the difference between the scores >2 then the smaller AIC value is the better fit model

# step function
step(lm_3, lm_4)
best_model = step(lm_4)
best_model

lm_4_predict = lm_4 %>%
    broom::augment(se_fit = T, interval = "confidence")

head(lm_4_predict)

ggplot(data= lm_4_predict) +
    geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_line(aes(x= bill_length_mm, y = .fitted, color = species)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper, x= bill_length_mm, fill = species), alpha = 0.3) + # alpha is just the transparency not a real stat value lol
    theme_classic()

# 2 continuous variable predictors !!!!N ;D
 
library(car) # vif(), calculates variance inflation factor, tests for multi colinearity

gentoo = penguins %>%
      filter(species == "Gentoo")
summary(gentoo)

lm_gentoo_1 = lm(bill_depth_mm ~ bill_length_mm, data = gentoo)
lm_gentoo_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data = gentoo)
lm_gentoo_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data = gentoo)

# lm_gentoo_4 = lm_gentoo_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g - 1, data = gentoo)
# summary(lm_gentoo_4) #intercept is gone!!

AIC(lm_gentoo_1, lm_gentoo_2, lm_gentoo_3)
step(lm_gentoo_3)
summary(lm_gentoo_3) # its unusual to remove the intercept even when its not statistically significant

vif(lm_gentoo_3) # vif >10 is totally colinear, 3-5 space is danger zone, less than 3 your chillin
# vif() is not sensible if you don't have an intercept

newdata = gentoo %>%
      select(body_mass_g) %>%
      mutate(flipper_length_mm = median(gentoo$flipper_length_mm, na.rm=T)) %>%
      mutate(bill_length_mm = median(gentoo$bill_length_mm, na.rm = T))
# body mass is changing and the other two variables arre the same, data we are gonna generate predictions!

head(newdata)
head(gentoo)

lm_gentoo_3_predict = lm_gentoo_3 %>%
    broom::augment(newdata = newdata, se_fit = T, interval = "confidence")
head(lm_gentoo_3_predict)

ggplot(data = lm_gentoo_3_predict) +
    geom_point(aes(x= body_mass_g, y= bill_depth_mm), data = gentoo) +
    geom_line(aes(x=body_mass_g, y = .fitted)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper, x = body_mass_g), alpha = 0.3) +
    annotate("text", x=4250, y=17, label = paste0("flipper length = ", median(gentoo$flipper_length_mm, na.rm=T))) +
  annotate("text", x=4250, y=16.5, label = paste0("bill length = ", median(gentoo$bill_length_mm, na.rm=T))) +
    theme_bw() 
?annotate
?theme
# can customize ggplot, stack overflow
# can make a plot with interactions as well
# annotate on the figure that flipper length and bill depth are SET as a median

# exercise 5.3
newdata2 = gentoo %>%
      select(flipper_length_mm) %>%
      mutate(body_mass_g = median(gentoo$body_mass_g, na.rm=T)) %>%
      mutate(bill_length_mm = median(gentoo$bill_length_mm, na.rm = T))

head(newdata2)
head(gentoo)

lm_gentoo_3_predict2 = lm_gentoo_3 %>%
    broom::augment(newdata = newdata2, se_fit = T, interval = "confidence")
head(lm_gentoo_3_predict2)


ggplot(data = lm_gentoo_3_predict2) +
    geom_point(aes(x= flipper_length_mm, y= bill_depth_mm), data = gentoo) +
    geom_line(aes(x=flipper_length_mm, y = .fitted)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper, x = flipper_length_mm), alpha = 0.3) +
  annotate("text", x=210, y=17, label = paste0("body mass = ", median(gentoo$body_mass_g, na.rm=T))) +
  annotate("text", x=210, y=16.5, label = paste0("bill length = ", median(gentoo$bill_length_mm, na.rm=T))) +
    theme_bw() 

# ANOVA

penguin_lm = lm(body_mass_g ~ species + sex, data = penguins)
summary(penguin_lm)
anova(penguin_lm)


penguins %>%
    group_by(species) %>%
    summarize(mean_body_mass_g = mean(body_mass_g, ma.rm=T))
penguins %>%
    group_by(sex) %>%
    summarize(mean_body_mass_g = mean(body_mass_g, ma.rm=T))

penguin_anova = aov(body_mass_g ~ sex + species, data = penguins)
summary(penguin_anova)


TukeyHSD(penguin_anova)
# ran an anova, found that these are significant variables now you do a deeper dive (especially important when dealing with over two different "treatments" within your variables)
# anova are an analysis of the mean so add the mean!!

# analysis NMDS, question: between these vials are the communities different?
# experimental plans, abundance changing with other abiotic 

# terestrial v. marine 
# research papers
# Brian Hollis (CLS, Room 306), Tad Dallas, grant foster grad student

