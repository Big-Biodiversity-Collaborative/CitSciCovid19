# Analyses of USA iNaturalist data re COVID19 (%change by state)
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
library(data.table)

# Load data large dataset have to use data.table, ugh
# Despite the file extension (.csv) it is a tab delimited file, GBIF!
inat_obs <- fread(file = "data/2015-2020-iNaturalist-USA.csv")

# Filter USA data only just in case
# DT[V2 > 5]
inat_obs <- inat_obs[countryCode == "US"]

# Convert to tibble, not tribble
usa_obs_tbl <- as_tibble(inat_obs)

# Remove non-USA states
# Column names are year, month, countryCode, stateProvince, occurrenceID, recordedBy
# Count number of unique observations and observers by state
usa_counts <- usa_obs_tbl %>%
  filter(stateProvince %in% c(datasets::state.name, "District of Columbia")) %>%
  group_by(stateProvince, year) %>%
  summarize(num_observations = n(), 
            num_users = length(unique(recordedBy)))

# Remove inat_obs and usa_obs_tbl to free up ram
rm(inat_obs, usa_obs_tbl)

# Garbage out!
gc()

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
