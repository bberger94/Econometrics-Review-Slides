library(tidyverse)
library(haven)
library(fixest)
# Load data
nes <- read_dta("nes2012edit.dta")
# Print data dictionary
print_labels(nes$swp)

# Recode data
nes_2 <- nes %>% 
  # Drop missing observations
  drop_na(gender, libcon3, swp) %>% 
  # Recode gender
  mutate(female = gender - 1)
  
# Estimate model
bivariate <- feols(swp ~ female, nes_2, vcov = "hetero")

summary(bivariate)

t.test(swp ~ female, nes_2)

full_model <- feols(swp ~ female + i(libcon3, ref = 3),
                    data = nes_2,
                    vcov = "hetero")

summary(full_model)
