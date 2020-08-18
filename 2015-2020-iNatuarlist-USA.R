# Analyses of USA iNaturalist data re COVID19 (%change by state)
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2020-07-09
# DOI: https://doi.org/10.15468/dl.wqjr5j

# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lubridate)
library(data.table)

# Load data large dataset have to use data.table, ugh
# Despite the file extension (.csv) it is a tab delimited file, GBIF!
inat_obs <- fread(file = "data/2015-2020-iNaturalist-USA.csv")

# Filter USA data only just in case
# DT[V2 > 5]
usa_obs <- inat_obs[countryCode == "US"]

# Convert to tibble, not tribble
usa_obs_tbl <- as_tibble(usa_obs)

# Remove non-USA states
# Column names are year, month, countryCode, stateProvince, occurrenceID, recordedBy
# Count number of unique observations and observers by state
usa_counts <- usa_obs_tbl %>%
  filter(nchar(stateProvince) > 0) %>%
  filter(stateProvince != "Baja California") %>%
  filter(stateProvince != "Midway") %>%
  filter(stateProvince != "Chihuahua")
  group_by(stateProvince, year) %>%
  summarize(num_observations = n(), 
            num_users = length(unique(recordedBy)))

# Plot number of observations 
usa_obs_plot <- ggplot(data = usa_counts, mapping = aes(x = year, 
                                                        y = num_observations,
                                                        group = stateProvince, 
                                                        color = stateProvince)) +
  geom_line() + 
  scale_y_log10() +
  theme(legend.position = "none")
usa_obs_plot