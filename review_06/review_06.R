library(tidyverse)
library(fixest)
# Estimate Regressions
bivariate <- feols(Temp ~ Ozone, airquality, vcov = "hetero")
multivariate <- feols(Temp ~ Ozone + Wind, airquality, vcov = "hetero")

summary(bivariate)

summary(multivariate)

library(tidyverse)
library(haven)
library(MatchIt)

smoking <- read_dta("smoking.dta")

match <- 
  matchit(smoker ~ sex + indigeneity + high_school + partnered +
            remoteness + language + risky_alcohol + age,
          data = smoking, 
          exact = ~ sex + high_school, # exact match variables
          replace = TRUE, # whether to use controls multiple times
          ratio = 2 # controls to match to each treated
          )

match

summary(match, un = F, standardize = F)$sum.matched[, 1:3]

matched_smoking <- match.data(match)
matched_smoking %>% select(smoker, weights) %>% head()

matched_smoking %>% 
  mutate(group = ifelse(smoker == 1, "Smokers", "Non-smokers")) %>% 
  ggplot(aes(x = weights)) +
  geom_histogram(binwidth = .2) +
  facet_wrap(. ~ group) +
  theme_bw() +
  theme(strip.text = element_text(size = 14)) 

feols(psyc_distress ~ smoker,
      data = matched_smoking,
      vcov = "hetero",
      weights = ~weights)
