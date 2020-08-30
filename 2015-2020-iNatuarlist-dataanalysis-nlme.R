# Analyses of USA iNaturalist data 
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2020-08-09
# DOI: https://doi.org/10.15468/dl.wqjr5j

# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lubridate)
library(nlme)

# Load data 
# If data/ does not exist, run 2015-2020-iNaturalist-USA-dataprocessing.R
usa_counts <- read_csv(file = "data/inat-usa-counts.csv")

# Add log transformation of num_observations
usa_counts <- usa_counts %>%
  mutate(log_num_obs = log10(num_observations))

# Filter data for 2015-2019 records, num_observations and num_users
usa_counts_2015_2019 <- usa_counts %>%
  filter(year != 2020)

# Filter data for 2020 records, num_observations and num_users
usa_counts_2020 <- usa_counts %>%
  filter(year == 2020)

# Plot data ignoring state
ggplot(data = usa_counts, mapping = aes(x = year, y = num_observations)) +
  geom_point() +
  scale_y_log10()

################################################################################
# num_observations Analysis

# Linear model of observation data 2015-2019
# Use ML because we are comparing models
usa_lme <- nlme::lme(log_num_obs ~ year, 
                     random = ~ 1|stateProvince,
                     data = usa_counts_2015_2019, 
                     method = "ML")

# Polynomial model of observation data 2015-2019
# Degree 2
usa_lme_poly <- nlme::lme(log_num_obs ~ poly(x = year, degree = 2),
                          random = ~ 1|stateProvince,
                          data = usa_counts_2015_2019, 
                          method = "ML")

# Compare linear and polynomial models
anova(usa_lme, usa_lme_poly)

# Polynomial model of observation data 2015-2019
# Use REML because we want accurate estimates
usa_lme_poly <- nlme::lme(log_num_obs ~ poly(x = year, degree = 2),
                          random = ~ 1|stateProvince,
                          data = usa_counts_2015_2019, 
                          method = "REML")

# Predict values for 2020 based on 2015-2019 num_observations linear model
usa_counts$pred_log_num_obs <- predict(object = usa_lme_poly, 
                                   newdata = usa_counts)

# Calculate percent change from expected vs observed num_observations linear model
compare_2020 <- usa_counts %>% 
  filter(year == 2020) %>%
  mutate(pred_num_obs = 10^pred_log_num_obs) %>%
  mutate(perc_change = ((num_observations - pred_num_obs)/ pred_num_obs)*100)

# Linear model of user data 2015-2019
# Use ML because we are comparing models
usa_lme <- nlme::lme(num_users ~ year, 
                     random = ~ 1|stateProvince,
                     data = usa_counts_2015_2019, 
                     method = "ML")

# Polynomial model of observation data 2015-2019
# Degree 2
usa_lme_poly <- nlme::lme(num_users ~ poly(x = year, degree = 2),
                          random = ~ 1|stateProvince,
                          data = usa_counts_2015_2019, 
                          method = "ML")

# Compare linear and polynomial models
anova(usa_lme, usa_lme_poly)
