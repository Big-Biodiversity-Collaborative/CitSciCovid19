# Processing of USA iNaturalist data
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2020-08-09
# DOI: https://doi.org/10.15468/dl.wqjr5j

# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(data.table)

# Load data large dataset have to use data.table, ugh
# Despite the file extension (.csv) it is a tab delimited file, GBIF!
inat_obs <- fread(file = "data/2015-2020-iNaturalist-USA.csv")

# Filter USA data only just in case
# DT[V2 > 5]
inat_obs <- inat_obs[countryCode == "US"]

# Convert to tibble, not tribble
usa_obs_tbl <- as_tibble(inat_obs)

# Clean up memory
rm(inat_obs)
gc()

# Remove non-USA states
# Column names are year, month, countryCode, stateProvince, occurrenceID, recordedBy
# Count number of unique observations and observers by state
usa_counts <- usa_obs_tbl %>%
  filter(stateProvince %in% c(datasets::state.name, "District of Columbia")) %>%
  group_by(stateProvince, year) %>%
  summarize(num_observations = n(), 
            num_users = length(unique(recordedBy)))

# Write counts data to file
write_csv(x = usa_counts, path = "data/inat-usa-counts.csv")

# Remove June and create csv
usa_counts <- usa_obs_tbl %>%
  filter(stateProvince %in% c(datasets::state.name, "District of Columbia")) %>%
  filter(month != 6) %>%
  group_by(stateProvince, year) %>%
  summarize(num_observations = n(), 
            num_users = length(unique(recordedBy)))

# Write counts data to file
write_csv(x = usa_counts, path = "data/inat-usa-counts-nojune.csv")
