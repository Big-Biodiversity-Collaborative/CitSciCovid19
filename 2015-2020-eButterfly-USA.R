# Data Cleaning of USA eButterfly Data 2015-2020
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2020-08-16

# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lubridate)
library(nlme)

# Load data 
ebutt_usa <- read_csv(file = "data/2015-2020-eButterfly-USA.csv")

# Add year column
ebutt_usa <- ebutt_usa %>%
  mutate(year = year(obs_date))

# Create observation data for analysis
usa_counts <- ebutt_usa %>%
  group_by(state, year) %>%
  summarize(num_observations = n(),
            num_users = length(unique(observer)))

# Add column for precovid/covid
usa_counts <- usa_counts %>%
  mutate(covid = if_else(year == 2020, true = TRUE, false = FALSE))

# Make quick plot of observations
usa_plot <- ggplot(data = usa_counts, mapping = aes(x = year,
                                                    y = num_observations, 
                                                    group = state, 
                                                    color = state)) +
  geom_line() + 
  theme(legend.position = "none")
usa_plot

##### OBSERVATIONS!!!
# Run model with no autocorrelation correction
usa_lme <- nlme::lme(num_observations ~ covid, 
                     random = ~ 1|state, 
                     data = usa_counts)

# Check for autocorrelation
plot(ACF(usa_lme))

# Run model accounting for temporal autocorrelation
usa_lme_ac <- nlme::lme(num_observations ~ covid, 
                        random = ~ 1|state, 
                        correlation = corAR1(form = ~ year|state),
                        data = usa_counts)

# Check for autocorrelation
plot(ACF(usa_lme_ac, resType = "normalized"))

summary(usa_lme_ac)

##### USERS!
# Run model with no autocorrelation correction
usa_lme <- nlme::lme(num_users ~ covid, 
                     random = ~ 1|state, 
                     data = usa_counts)

# Check for autocorrelation
plot(ACF(usa_lme))

# Run model accounting for temporal autocorrelation
usa_lme_ac <- nlme::lme(num_users ~ covid, 
                        random = ~ 1|state, 
                        correlation = corAR1(form = ~ year|state),
                        data = usa_counts)

# Check for autocorrelation
plot(ACF(usa_lme_ac, resType = "normalized"))

summary(usa_lme_ac)
