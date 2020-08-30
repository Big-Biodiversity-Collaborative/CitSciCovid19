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
usa_counts <- read_csv(file = "data/inat-usa-counts-nojune.csv")

# Add column for precovid/covid
usa_counts <- usa_counts %>%
  mutate(covid = if_else(year == 2020, true = TRUE, false = FALSE))

# Normalize data for visualization
usa_counts <- usa_counts %>%
  group_by(stateProvince) %>%
  mutate(num_observations_norm = (num_observations - min(num_observations))/(max(num_observations) - min(num_observations)))

# Make quick plot of observations
usa_plot <- ggplot(data = usa_counts, mapping = aes(x = year,
                                                    y = num_observations_norm, 
                                                    group = stateProvince, 
                                                    color = stateProvince)) +
  geom_line() + 
  theme(legend.position = "none")
usa_plot

# Make quick plot of observations
usa_plot <- ggplot(data = usa_counts, mapping = aes(x = year,
                                                    y = num_observations, 
                                                    group = stateProvince, 
                                                    color = stateProvince)) +
  geom_line() + 
  scale_y_log10() +
  theme(legend.position = "none")
usa_plot

################################################################################
# Linear model of observation data
# Use ML because we are comparing models
usa_lme <- nlme::lme(num_observations ~ year, 
                     random = ~ 1|stateProvince,
                     data = usa_counts, 
                     method = "ML")

# Polynomial model of observation data
# Degree 2
usa_lme_poly <- nlme::lme(num_observations ~ poly(x = year, degree = 2),
                          random = ~ 1|stateProvince,
                          data = usa_counts, 
                          method = "ML")

# Compare linear and polynomial models
anova(usa_lme, usa_lme_poly)

# Linear model not significantly worse, try B spline with linear model
usa_lme_spline <- nlme::lme(num_observations ~ splines::bs(x = year, 
                                                           knots = 2019,
                                                           degree = 1), 
                            random = ~ 1|stateProvince,
                            data = usa_counts, 
                            method = "ML")
  
# Compare linear and spline model
anova(usa_lme, usa_lme_spline)

# Just for fun try polynomial spline, degree = 2
usa_poly_spline <- nlme::lme(num_observations ~ splines::bs(x = year, 
                                                           knots = 2019,
                                                           degree = 2), 
                            random = ~ 1|stateProvince,
                            data = usa_counts, 
                            method = "ML")

# Compare poly and spline model
anova(usa_lme_poly, usa_poly_spline)

################################################################################
# Try with log transformed observations
usa_counts$num_observations_log <- log10(usa_counts$num_observations)

# Linear model of observation data
# Use ML because we are comparing models
usa_lme <- nlme::lme(num_observations_log ~ year, 
                     random = ~ 1|stateProvince,
                     data = usa_counts, 
                     method = "ML")

# Polynomial model of observation data
# Degree 2
usa_lme_poly <- nlme::lme(num_observations_log ~ poly(x = year, degree = 2),
                          random = ~ 1|stateProvince,
                          data = usa_counts, 
                          method = "ML")

# Compare linear and polynomial models
anova(usa_lme, usa_lme_poly)

# Create polynomial spline, degree = 2
usa_poly_spline <- nlme::lme(num_observations_log ~ splines::bs(x = year, 
                                                            knots = 2019,
                                                            degree = 2), 
                             random = ~ 1|stateProvince,
                             data = usa_counts, 
                             method = "ML")

# Compare poly and spline model
anova(usa_lme_poly, usa_poly_spline)


################################################################################
# Try with normalized data
# Linear model of observation data
# Use ML because we are comparing models
usa_lme <- nlme::lme(num_observations_norm ~ year, 
                     random = ~ 1|stateProvince,
                     data = usa_counts, 
                     method = "ML")

# Polynomial model of observation data
# Degree 2
usa_lme_poly <- nlme::lme(num_observations_norm ~ poly(x = year, degree = 2),
                          random = ~ 1|stateProvince,
                          data = usa_counts, 
                          method = "ML")

# Compare linear and polynomial models
anova(usa_lme, usa_lme_poly)

# Just for fun try polynomial spline, degree = 2
usa_poly_spline <- nlme::lme(num_observations_norm ~ splines::bs(x = year, 
                                                                 knots = 2019,
                                                                 degree = 2), 
                             random = ~ 1|stateProvince,
                             data = usa_counts, 
                             method = "ML")

# Compare poly and spline model
anova(usa_lme_poly, usa_poly_spline)


################################################################################

# Try without random effects
usa_lm <- lm(num_observations ~ year, data = usa_counts)

# Polynomial model without random effects
usa_poly <- lm(num_observations ~ poly(x = year, degree = 2), data = usa_counts)

# compare linear and polynomial model
anova(usa_lm, usa_poly)

# linear spline 
usa_lm_spline <- lm(num_observations ~ splines::bs(x = year, knots = 2019, degree = 1),
                    data = usa_counts)

# Compare linear and spline model
anova(usa_lm, usa_lm_spline)

################################################################################
# Sum data across the country
nostate_counts <- usa_counts %>%
  group_by(year) %>%
  summarize(num_observations = sum(num_observations))

# Linear model
nostate_lm <- lm(num_observations ~ year, data = nostate_counts)

# Polynomial model
nostate_poly <- lm(num_observations ~ poly(x = year, degree = 2), data = nostate_counts)

# Compare models
anova(nostate_lm, nostate_poly)

# Create polynomial spline
nostate_poly_spline <- lm(num_observations ~ splines::bs(x = year, knots = 2019, degree = 2),
                          data = nostate_counts)

# Compare poly to poly spline
anova(nostate_poly, nostate_poly_spline)

################################################################################
# Users

#  Make quick plot of users
usa_plot <- ggplot(data = usa_counts, mapping = aes(x = year,
                                                    y = num_users, 
                                                    group = stateProvince, 
                                                    color = stateProvince)) +
  geom_line() +
  scale_y_log10() +
  theme(legend.position = "none")
usa_plot


# Linear model of user data
# Use ML because we are comparing models
usa_lme <- nlme::lme(num_users ~ year, 
                     random = ~ 1|stateProvince,
                     data = usa_counts, 
                     method = "ML")

# Polynomial model of observation data
# Degree 2
usa_lme_poly <- nlme::lme(num_users ~ poly(x = year, degree = 2),
                          random = ~ 1|stateProvince,
                          data = usa_counts, 
                          method = "ML")

# Compare linear and polynomial models
anova(usa_lme, usa_lme_poly)

# Create polynomial spline, degree = 2
usa_poly_spline <- nlme::lme(num_users ~ splines::bs(x = year, 
                                                     knots = 2019,
                                                     degree = 2), 
                             random = ~ 1|stateProvince,
                             data = usa_counts, 
                             method = "ML")

# Compare polynomial and polynomial spline models
anova(usa_lme_poly, usa_poly_spline)


################################################################################
# OLD BELOW HERE
################################################################################

##### OBSERVATIONS!!!
# Run model with no autocorrelation correction
usa_lme <- nlme::lme(num_observations ~ covid, 
                     random = ~ 1|stateProvince, 
                     data = usa_counts)

# Check for autocorrelation
plot(ACF(usa_lme))

# Run model accounting for temporal autocorrelation
usa_lme_ac <- nlme::lme(num_observations ~ covid, 
                        random = ~ 1|stateProvince, 
                        correlation = corAR1(form = ~ year|stateProvince),
                        data = usa_counts)

# Check for autocorrelation
plot(ACF(usa_lme_ac, resType = "normalized"))

summary(usa_lme_ac)

#### Second Order Autocorrelation
# Run model accounting for temporal autocorrelation
usa_lme_ac <- nlme::lme(num_observations ~ covid, 
                        random = ~ 1|stateProvince, 
                        correlation = corARMA(form = ~ year|stateProvince, q = 2),
                        data = usa_counts)

# Check for autocorrelation
plot(ACF(usa_lme_ac, resType = "normalized"))

summary(usa_lme_ac)

##### USERS!
# Run model with no autocorrelation correction
usa_lme <- nlme::lme(num_users ~ covid, 
                     random = ~ 1|stateProvince, 
                     data = usa_counts)

# Check for autocorrelation
plot(ACF(usa_lme))

# Run model accounting for temporal autocorrelation
usa_lme_ac <- nlme::lme(num_users ~ covid, 
                        random = ~ 1|stateProvince, 
                        correlation = corAR1(form = ~ year|stateProvince),
                        data = usa_counts)

# Check for autocorrelation
plot(ACF(usa_lme_ac, resType = "normalized"))

summary(usa_lme_ac)

##### 
# Test autocorrelation approach
usa_counts_test <- usa_counts %>%
  group_by(stateProvince) %>%
  mutate(num_observations_test = if_else(year == 2020,
                                          true = min(num_observations), 
                                          false = num_observations))

# Run model accounting for temporal autocorrelation
usa_lme_ac <- nlme::lme(num_observations_test ~ covid, 
                        random = ~ 1|stateProvince, 
                        correlation = corAR1(form = ~ year|stateProvince),
                        data = usa_counts_test)

# Check for autocorrelation
plot(ACF(usa_lme_ac, resType = "normalized"))

summary(usa_lme_ac)

# Model all iNat USA data, no random effects
usa_counts_all <- usa_counts %>%
  group_by(year) %>%
  summarize(num_obs = sum(num_observations))
plotusa <- ggplot(data = usa_counts_all, mapping = aes(x = year, y = num_obs)) +
  geom_point() +
  geom_smooth(method = "lm")
plotusa
