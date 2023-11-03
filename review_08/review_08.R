params <-
list(handout = TRUE)

library(tidyverse)
library(haven)
library(fixest)

cigarettes <- read_dta("cigarettes.dta")

# OLS
fit_e1_ols <- feols(log(packs) ~ price, 
                    cigarettes, 
                    cluster = ~state)

# Two-way fixed effects
fit_e1_twfe <- feols(log(packs) ~ price | state + year, 
                     cigarettes, 
                     cluster = ~state)

# IV
fit_e1_iv <- feols(log(packs) ~ 1 | price ~ salestax, 
                   cigarettes, 
                   cluster = ~state)

setFixest_dict(c(
  "log(packs)" = "Log(Packs)",
  price = "Price",
  salestax = "Sales Tax",
  state = "State",
  year = "Year",
  fit_e1_ols = "OLS"
))

etable(fit_e1_ols,
       fit_e1_twfe, 
       fit_e1_iv,
       keep = "Price", 
       headers = c("OLS", "TWFE", "IV"),
       tex = TRUE,
       fitstat = ~ n + ivf
       )

feols(price ~ salestax, cigarettes, cluster = ~state)







# ------------------------ #
# RD Estimation & Plotting
# ------------------------ #
# Load data
rd_data <- read_dta("rd.dta")

# Plot data
rd_plot <- ggplot(rd_data, aes(x = x, y = y)) + 
  geom_point(size = 1) +
  geom_vline(xintercept = 0.5, linetype = "dashed")

rd_plot

# Recenter cutoff at zero
rd_data <- mutate(rd_data, x_cent = x - 0.5)

# Estimate interacted regression 
fit_rd <- feols(y ~ x_cent + treat + x_cent:treat, 
                rd_data,
                vcov = "hetero")
fit_rd

# Get predicted values
predictions <- fit_rd$fitted.values

# Plot predicted values
rd_plot + 
  geom_line(aes(y = predictions,
                group = treat),
            color = "red")

# Estimate interacted regression 
fit_rd_poly <- feols(y ~ treat +
                       x_cent * treat + 
                       x_cent^2 * treat + 
                       x_cent^3 * treat, 
                     rd_data,
                     vcov = "hetero")

broom::tidy(fit_rd_poly) %>% filter(term == "treat")

# Get predicted values
predictions_poly = fit_rd_poly$fitted.values

# Plot predicted values
rd_plot + geom_line(aes(y = predictions_poly,
                        group = (x >= 0.5)),
                    color = "red")

# Keep observations in bandwidth (0.15 points)
rd_data_bw15 <- filter(rd_data, abs(x - 0.5) <= 0.15)

# Estimate interacted regression 
fit_rd_bw15 <- feols(y ~ x_cent + treat + x_cent:treat,
                     rd_data_bw15,
                     vcov = "hetero")

broom::tidy(fit_rd_bw15) %>% filter(term == "treat")

# Get predicted values
rd_data_bw15$predictions <- fit_rd_bw15$fitted.values

# Plot predicted values
rd_plot + geom_line(aes(y = predictions,
                        x = x,
                        group = treat),
                    data = rd_data_bw15,
                    color = "red")
