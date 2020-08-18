# Data Cleaning of USA eButterfly Data 2015-2020
# Kathleen L Prudic
# klprudic@arizona.edu
# created 2020-08-16

# Remove wonton variables 
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lubridate)

# Load data 
ebutt_usa <- read_csv(file = "data/2015-2020-eButterfly-USA.csv")


