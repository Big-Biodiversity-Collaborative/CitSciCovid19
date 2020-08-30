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


###############################################################
# Urban vs Rural Analysis

##Urban/Rural designation for sites
library(sp)
library(rgdal)
library(raster)

#read in urban census boundaries layer as spatial data frame
shape <- readOGR(dsn = "data", layer = "tl_2017_us_uac10")

#plot urban boundaries 
#plot(shape)

#convert site visits df to spatial data frame
spdf<-SpatialPointsDataFrame(cbind(ebutt_usa$longitude,ebutt_usa$latitude),ebutt_usa)

## CRS arguments: NA
crs(shape) <- CRS("+proj=longlat +datum=WGS84")
crs(spdf) <- CRS("+proj=longlat +datum=WGS84")

#intersect points with urban boundaries
o <- over(spdf, shape)

ebutt_usaUrb <-cbind.data.frame(ebutt_usa,o)

#drop unnecessary columns
ebutt_usaUrb = subset(ebutt_usaUrb, select = -c(UACE10,GEOID10,NAME10,NAMELSAD10,LSAD10,UATYP10,FUNCSTAT10,ALAND10,
                                                AWATER10,INTPTLAT10,INTPTLON10) )

#convert N/A to 0, convert city name to 1, add to "urban" column
ebutt_usaUrb$urban <- ifelse(is.na(ebutt_usaUrb$MTFCC10) == TRUE,0,1)

#delete extra column
ebutt_usaUrb$MTFCC10 <- NULL

#write out csv
write.csv(ebutt_usaUrb,'data/ebutt_usaUrb.csv')

