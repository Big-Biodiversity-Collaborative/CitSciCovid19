# Analyses of USA NPN data lme4
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2020-09-02

# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lme4)
library(merTools)

# Load data
usa_counts <- read_csv(file = "data/npn-usa-counts-urban.csv")

# Add log transformation of num_observations and num_users
# usa_counts <- usa_counts %>%
# mutate(log_num_obs = log10(num_observations)) %>%
# mutate(log_num_users = log10(num_users))

# Filter data for 2015-2019 records, num_observations and num_users
usa_counts_2015_2019 <- usa_counts %>%
  filter(year != 2020)

# Plot observation data ignoring state
ggplot(data = usa_counts, mapping = aes(x = year, y = num_observations)) +
  geom_point() 

# Plot user data ignoring state
ggplot(data = usa_counts, mapping = aes(x = year, y = num_users)) +
  geom_point() 

# Plot percent urban observation data ignoring state
ggplot(data = usa_counts, mapping = aes(x = year, y = pctUrban)) +
  geom_point()

################################################################################
# num_observations analysis

# Linear model of observation data 2015-2019
# Use ML because we are comparing models
usa_lme <- lme4::lmer(num_observations ~ year + (1|state),  
                      data = usa_counts_2015_2019, 
                      REML = FALSE)

# Polynomial model of observation data 2015-2019
# Degree 2
usa_lme_poly <- lme4::lmer(num_observations ~ poly(x = year, degree = 2) + (1|state),
                           data = usa_counts_2015_2019, 
                           REML = FALSE)

# Compare linear and polynomial models
anova(usa_lme, usa_lme_poly)

# Linear model of observation data 2015-2019
# Use REML because we want accurate estimates
usa_lme <- lme4::lmer(num_observations ~ year + (1|state),  
                     data = usa_counts_2015_2019, 
                           REML = TRUE)

# Create predicted values
predictions <- merTools::predictInterval(usa_lme, 
                                         newdata = usa_counts, 
                                         level = 0.95)

# Add predictions to dataframe
usa_counts <- usa_counts %>%
  mutate(pred_num_obs = predictions$fit, 
         lwr_num_obs = predictions$lwr,
         upr_num_obs = predictions$upr) %>%
  mutate(below_ci = num_observations < lwr_num_obs, 
         above_ci = num_observations > upr_num_obs) %>%
  mutate(pct_change = ((num_observations - pred_num_obs)/ pred_num_obs)*100)

# Pull out values for 2020
pred_2020 <- usa_counts %>%
  filter(year == 2020)

# Write to file
write_csv(x = pred_2020, path = "output/NPN_predictions_obs_2020.csv")

################################################################################
# num_users analysis

# Linear model of observation data 2015-2019
# Use ML because we are comparing models
usa_lme <- lme4::lmer(num_users ~ year + (1|state),  
                      data = usa_counts_2015_2019, 
                      REML = FALSE)

# Polynomial model of observation data 2015-2019
# Degree 2
usa_lme_poly <- lme4::lmer(num_users ~ poly(x = year, degree = 2) + (1|state),
                           data = usa_counts_2015_2019, 
                           REML = FALSE)

# Compare linear and polynomial models
anova(usa_lme, usa_lme_poly)

# Linear model of observation data 2015-2019
# Use REML because we want accurate estimates
usa_lme <- lme4::lmer(num_users ~ year + (1|state),  
                      data = usa_counts_2015_2019, 
                           REML = TRUE)

# Create predicted values
predictions <- merTools::predictInterval(usa_lme, 
                                         newdata = usa_counts, 
                                         level = 0.95)

# Add predictions to dataframe
usa_counts <- usa_counts %>%
  mutate(pred_num_users = predictions$fit, 
         lwr_num_users = predictions$lwr,
         upr_num_users = predictions$upr) %>%
  mutate(below_ci = num_users < lwr_num_users, 
         above_ci = num_users > upr_num_users) %>%
  mutate(pct_change = ((num_users - pred_num_users)/ pred_num_users)*100)  

# Pull out values for 2020
pred_2020 <- usa_counts %>%
  filter(year == 2020)

# Write to file
write_csv(x = pred_2020, path = "output/NPN_predictions_users_2020.csv")

################################################################################
# percent urban observations analysis

# Linear model of observation data 2015-2019
# Use ML because we are comparing models
usa_lme <- lme4::lmer(pctUrban ~ year + (1|state),  
                      data = usa_counts_2015_2019, 
                      REML = FALSE)

# Polynomial model of observation data 2015-2019
# Degree 2
usa_lme_poly <- lme4::lmer(pctUrban ~ poly(x = year, degree = 2) + (1|state),
                           data = usa_counts_2015_2019, 
                           REML = FALSE)

# Compare linear and polynomial models
anova(usa_lme, usa_lme_poly)

# Polynomial model of observation data 2015-2019
# Use REML because we want accurate estimates
usa_lme_poly <- lme4::lmer(pctUrban ~ poly(x = year, degree = 2) + (1|state),
                           data = usa_counts_2015_2019,  
                           REML = TRUE)

# Create predicted values
predictions <- merTools::predictInterval(usa_lme_poly, 
                                         newdata = usa_counts, 
                                         level = 0.95)

# Add predictions to dataframe
usa_counts <- usa_counts %>%
  mutate(pred_pct_urban = predictions$fit, 
         lwr_pct_urban = predictions$lwr,
         upr_pct_urban = predictions$upr) %>%
  mutate(below_ci = pctUrban < lwr_pct_urban, 
         above_ci = pctUrban > upr_pct_urban) %>%
  mutate(pct_change = ((pctUrban - pred_pct_urban)/ pred_pct_urban)*100)  

# Pull out values for 2020
pred_2020 <- usa_counts %>%
  filter(year == 2020)

# Write to file
write_csv(x = pred_2020, path = "output/NPN_predictions_urban_2020.csv")
