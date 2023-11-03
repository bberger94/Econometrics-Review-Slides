params <-
list(handout = TRUE)

library(fixest)
data("Grunfeld", package = "plm") 
head(Grunfeld)

# OLS (pooled)
ols <- feols(inv ~ value + capital, 
             data = Grunfeld, 
             cluster = ~firm)

# Least Squares Dummy Variable estimator 
lsdv <- feols(inv ~ value + capital + i(firm),
              data = Grunfeld,
              cluster = ~firm)

# Fixed effects estimator 
fe <- feols(inv ~ value + capital | firm,
            data = Grunfeld,
            cluster = ~firm)

# Two-way fixed effects estimator
twfe <- feols(inv ~ value + capital | firm + year,
              data = Grunfeld,
              cluster = ~firm)

# Set term dictionary
setFixest_dict(c(
  inv = "Investment",
  value = "Value",
  capital = "Capital"
))

# Make regression table
etable(list("OLS" = ols, 
            "Dummy Variables" = lsdv, 
            "Fixed Effects" = fe, 
            "Two-way Fixed Effects" = twfe),
       tex = TRUE, # comment out this line to view in console
       se.below = FALSE,
       drop = "^firm\\$=\\$[5678910]+", 
       order = c("Value", "Capital", "firm"),
       fitstat = "n",
       digits = 2
       )
