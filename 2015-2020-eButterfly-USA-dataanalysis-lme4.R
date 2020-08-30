# Analyses of USA eButterfly data lme4
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2020-09-01


# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lme4)
library(merTools)

# Load data 
ebutt_usa <- read_csv(file = "data/2015-2020-eButterfly-USA.csv")

# Add year column
ebutt_usa <- ebutt_usa %>%
  mutate(year = year(obs_date))

# Create observation data for analysis
usa_counts <- ebutt_usa %>%
  group_by(state, year) %>%
  summarize(num_observations = n(),
            num_users = length(unique(observer))) %>%
  ungroup()

# Add log transformation of num_observations and num_users
usa_counts <- usa_counts %>%
  mutate(log_num_obs = log10(num_observations)) %>%
  mutate(log_num_users = log10(num_users))

# Filter data for 2015-2019 records, num_observations and num_users
usa_counts_2015_2019 <- usa_counts %>%
  filter(year != 2020)

# Plot observation data ignoring state
ggplot(data = usa_counts, mapping = aes(x = year, y = num_observations)) +
  geom_point() +
  scale_y_log10()

# Plot user data ignoring state
ggplot(data = usa_counts, mapping = aes(x = year, y = num_users)) +
  geom_point() +
  scale_y_log10()

################################################################################
# num_observations analysis

# Linear model of observation data 2015-2019
# Use ML because we are comparing models
usa_lme <- lme4::lmer(log_num_obs ~ year + (1|state),  
                      data = usa_counts_2015_2019, 
                      REML = FALSE)

# Polynomial model of observation data 2015-2019
# Degree 2
usa_lme_poly <- lme4::lmer(log_num_obs ~ poly(x = year, degree = 2) + (1|state),
                           data = usa_counts_2015_2019, 
                           REML = FALSE)

# Compare linear and polynomial models
anova(usa_lme, usa_lme_poly)

# Polynomial model of observation data 2015-2019
# Use REML because we want accurate estimates
usa_lme <- lme4::lmer(log_num_obs ~ year + (1|state),  
                      data = usa_counts_2015_2019, 
                      REML = TRUE)

# Create predicted values
predictions <- merTools::predictInterval(usa_lme, 
                                         newdata = usa_counts, 
                                         level = 0.95)

# Add predictions to dataframe
usa_counts <- usa_counts %>%
  mutate(pred_num_obs = 10^predictions$fit, 
         lwr_num_obs = 10^predictions$lwr,
         upr_num_obs = 10^predictions$upr) %>%
  mutate(below_ci = num_observations < lwr_num_obs, 
         above_ci = num_observations > upr_num_obs) %>%
  mutate(perc_change = ((num_observations - pred_num_obs)/ pred_num_obs)*100)

  # Pull out values for 2020
pred_2020 <- usa_counts %>%
  filter(year == 2020)

# Write to file
write_csv(x = pred_2020, path = "output/eButterfly_predictions_obs_2020.csv")

################################################################################
# num_users analysis

# Linear model of observation data 2015-2019
# Use ML because we are comparing models
usa_lme <- lme4::lmer(log_num_users ~ year + (1|state),  
                      data = usa_counts_2015_2019, 
                      REML = FALSE)

# Polynomial model of observation data 2015-2019
# Degree 2
usa_lme_poly <- lme4::lmer(log_num_users ~ poly(x = year, degree = 2) + (1|state),
                           data = usa_counts_2015_2019, 
                           REML = FALSE)

# Compare linear and polynomial models
anova(usa_lme, usa_lme_poly)

# Polynomial model of observation data 2015-2019
# Use REML because we want accurate estimates
usa_lme <- lme4::lmer(log_num_users ~ year + (1|state),  
                      data = usa_counts_2015_2019, 
                      REML = TRUE)

# Create predicted values
predictions <- merTools::predictInterval(usa_lme, 
                                         newdata = usa_counts, 
                                         level = 0.95)

# Add predictions to dataframe
usa_counts <- usa_counts %>%
  mutate(pred_num_users = 10^predictions$fit, 
         lwr_num_users = 10^predictions$lwr,
         upr_num_users = 10^predictions$upr) %>%
  mutate(below_ci = num_users < lwr_num_users, 
         above_ci = num_users > upr_num_users) %>%
  mutate(perc_change = ((num_users - pred_num_users)/ pred_num_users)*100)  %>%
  dplyr::select(-num_observations, -log_num_obs)

# Pull out values for 2020
pred_2020 <- usa_counts %>%
  filter(year == 2020)

# Write to file
write_csv(x = pred_2020, path = "output/eButterfly_predictions_users_2020.csv")
