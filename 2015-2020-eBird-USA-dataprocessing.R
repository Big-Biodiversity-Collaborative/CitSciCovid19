# USA eBird data processing script 
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2020-08-25

# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lubridate)

# Load data
ebird_file <- "~/Downloads/eBird/Data/ebd_US_relJul-2020.txt"

# Establish output file names
ebird_outfile <- "data/2015-2020-ebird-USA.csv"

# Read in Header row, include 1 row of data to ensure data type identification
# ie dates get read in as dates, numbers get read in as numbers
ebird_header <- read_tsv(file = ebird_file, n_max = 1, col_types = cols()) 

ebird_colnames <- colnames(ebird_header)

do_filter <- TRUE

# Read in data 1000 rows at a time
for (i in 1:10000) {
  # Use readr::read_tsv
  max_rows <- 1000
  start <- (i * max_rows) - max_rows + 1
    
  # First iteration, start at row 2
  if (i == 1) {
    start <- 2
    max_rows <- max_rows - 1
  }
  
  ebird_rows <- read_tsv(file = ebird_file, 
                         skip = start - 1, 
                         n_max = max_rows,
                         col_names = FALSE, 
                         col_types = cols())
  
  # Assign column names
  colnames(ebird_rows) <- ebird_colnames
  if (i %% 10 == 0) {
    cat("iteration ", i, " start: ", start, "\n")
  }
  
  # Select columns of interest
  ebird_data <- ebird_rows %>%
    select(`GLOBAL UNIQUE IDENTIFIER`, `OBSERVATION DATE`, `OBSERVER ID`, 
           `SCIENTIFIC NAME`, `COUNTRY CODE`, STATE, LATITUDE, LONGITUDE)
  if (do_filter) {
  
  # Filter to months of interest
  ebird_data <- ebird_data %>%
    filter(month(`OBSERVATION DATE`) %in% c(3:6))
  
  # Filter to year of interest
  ebird_data <- ebird_data %>%
    filter(year(`OBSERVATION DATE`) %in% c(2012:2020))
  
  # Filter to US States
  # Use %in% datasets::state.name
  ebird_data <- ebird_data %>%
    filter(STATE %in% c(datasets::state.name, "District of Columbia"))
  }
  
  # Write to file
  append <- TRUE
  if (i == 1) {
    append <- FALSE
  }
  write_csv(x = ebird_data, 
            path = ebird_outfile, 
            append = append)
}

# Try to find row post 2015
ebird_rows <- read_tsv(file = ebird_file, 
                       skip = 2e8, 
                       n_max = max_rows,
                       col_names = FALSE, 
                       col_types = cols())
colnames(ebird_rows) <- ebird_colnames

