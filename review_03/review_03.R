library(tidyverse)
library(haven)
library(fixest)
# Load data
nes <- read_dta("nes2012edit.dta")

# Create an interaction term
nes <- mutate(nes, female_colgrad = female * colgrad) 

# Estimate models
m1 <- feols(lib ~ female, 
            nes, vcov = "hetero")
m2 <- feols(lib ~ female + colgrad, 
            nes, vcov = "hetero")
m3 <- feols(lib ~ female + colgrad + female_colgrad,
            nes, vcov = "hetero")

etable(m1, m2, m3)

etable(m1, m2, m3, se.below = TRUE)

setFixest_dict(c(lib = "Liberal",
                 female = "Female",
                 colgrad = "College Graduate",
                 female_colgrad = "Female x College Graduate"
))

etable(m1, m2, m3, se.below = T)

etable(list("Bivariate" = m1, "Multivariate" = m2, "Interacted" = m3),
       se.below = T)



library(tidyverse)
library(haven)
library(fixest)

# Read data
preston_data <- read_dta("preston.dta")

# Define plot
preston_plot <- ggplot(preston_data,
                       aes(x = rgdppc, y = life_exp)) +
  geom_point() +
  coord_cartesian(xlim = c(0, 150000), ylim = c(50, 85)) +
  scale_x_continuous(labels = scales::dollar) +
  labs(x = "Real GDP per capita", y = "Life expectancy")

preston_plot

# Add linear fit
preston_plot + geom_smooth(method = "lm")

# Add log of rgddpc
preston_data <- mutate(preston_data, log_rgdppc = log(rgdppc))

# Plot level log
preston_logplot <- ggplot(preston_data,
                          aes(x = log_rgdppc, y = life_exp))  + 
  geom_point() +
  labs(x = "Log Real GDP per capita",
       y = "Life expectancy")


preston_logplot

# Add linear fit
preston_logplot + geom_smooth(method = "lm")

# Estimate model
m_levlog <- feols(life_exp ~ log_rgdppc, preston_data, vcov = "hetero")
summary(m_levlog, robust = TRUE)

# Extract coefficients
coefs <- m_levlog$coefficients

# Create dataset of predicted values for RGDPPC
pred_data <- tibble(
  rgdppc = seq(100, 150000, 100),
  pred = coefs[1] + coefs[2] * log(rgdppc)
)

## # Plot with predictions
## preston_plot +
##   geom_line(aes(y = pred, x = rgdppc), data = pred_data)
## 

# Plot with predictions
preston_plot +
  geom_line(aes(y = pred, x = rgdppc), data = pred_data)
