# Load packages
library(tidyverse) # for data manipulation, graphing, etc. 
library(haven) # to import a .dta file

# Load data
climate_data <- read_dta("climate_panel.dta") 

# Compute new dataset of annual rainfall
annual_rain <- climate_data %>% 
  group_by(year) %>% 
  summarize(mean_rain = mean(wpre),
            mean_temp = mean(wtem))

# Regress average rainfall on year, save result to object 
bivariate <- lm(mean_rain ~ year, data = annual_rain) 

summary(bivariate) 

# Extract coefficients from model
bivariate$coefficients 

# Extract a specific coefficient
bivariate$coefficients["year"]

# Standard errors are a bit trickier
sqrt(diag(vcov(bivariate)))

# Extract residuals and fitted values
bivariate$residuals
bivariate$fitted.values

# Predict values of the dependent variable
predict(bivariate, tibble(year = 2023))

sum(bivariate$residuals^2)

# Save a scatterplot as an object
rain_scatter <- ggplot(annual_rain, 
                       aes(x = year, y = mean_rain)) +
  geom_point() +
  labs(x = "Year",
       y = "Average Precipitation (100s mm)") 

rain_scatter

# Add regression line by supplying coefficients
rain_scatter + 
  geom_abline(intercept = bivariate$coefficients[1],
              slope = bivariate$coefficients[2])

# Add regression line using ggplot geometry
rain_scatter + geom_smooth(method = "lm")

predict(bivariate, newdata = tibble(year = c(2022, 1022, 3022)))

rain_scatter + 
  geom_abline(intercept = bivariate$coefficients["(Intercept)"],
              slope = bivariate$coefficients["year"])

rain_scatter + 
  geom_abline(intercept = bivariate$coefficients["(Intercept)"],
              slope = bivariate$coefficients["year"]) +
  coord_cartesian(xlim = c(1000, 3500), ylim = c(-40, 60)) +
  geom_vline(xintercept = 2022, linetype = "dashed") +
  geom_vline(xintercept = 1022, linetype = "dashed") +
  geom_vline(xintercept = 3022, linetype = "dashed") 

multivariate <- lm(mean_rain ~ year + mean_temp, 
                   data = annual_rain) 

summary(multivariate)
