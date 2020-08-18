# Theresa Crimmins
# theresa@usanpn.org
# created 8-7-20

# clean up Environment
rm(list = ls())

# Load additional packages
library(tidyverse)
library(lubridate)
library(rnpn)
library(plyr)
 
#data pull by year
#npn_download_status_data(request_source = "TCrimmins", years = c(2019), 
#                       additional_fields = c("observedby_person_id","Dataset_ID"), download_path ='data/NPN_2019.csv')

### processing one year at a time###############

# Load data for one year 
allrecords <- read_csv(file = "data/NPN_2016.csv")  


# remove unnecessary columns
recordstrim = subset(allrecords, select = -c(Update_Datetime,Species_ID,Genus,Species,Phenophase_ID,Phenophase_Description,Day_of_Year,
                                                  Phenophase_Status, Intensity_Category_ID,Intensity_Value,Abundance_Value) )

# Filter data to have months Mar-Jun only 
MarJunrec <- recordstrim %>%
  filter(month(Observation_Date) %in% c(3,4,5,6))

# remove NEON data + other non-NN data
# 3, 9, 10, -9999 are all good
unique(MarJunrec$Dataset_ID, incomparables = FALSE)

MarJunrec<-subset(MarJunrec, Dataset_ID!="16")


#extract month, year from Date column and put into new columns
MarJunrec$month<-format(MarJunrec$Observation_Date, "%m")
MarJunrec$year<-format(MarJunrec$Observation_Date, "%y")

#remove rows if state = -9999
MarJunrec <- MarJunrec[!(MarJunrec$State=="-9999"),]

# find out what other non-US state labels might be causing trouble
unique(MarJunrec$State, incomparables = FALSE)

#remove rows if state = something outside of US
MarJunrec<-subset(MarJunrec, State!="Toscana" & State!="BC" & State!="Shandong Sheng" & State!="AB" & State!="QC" 
                  & State!="ON" & State!="Jiangsu" & State!="Tibet" & State!="QC" & State!="Toscana" & State!="Jambyl Region")



###############################################################

##Urban/Rural designation for sites
library(sp)
library(rgdal)
library(raster)

#read in urban census boundaries layer as spatial data frame
shape <- readOGR(dsn = "data", layer = "tl_2017_us_uac10")

#plot urban boundaries 
#plot(shape)

#convert site visits df to spatial data frame
spdf<-SpatialPointsDataFrame(cbind(MarJunrec$Longitude,MarJunrec$Latitude),MarJunrec)

## CRS arguments: NA
crs(shape) <- CRS("+proj=longlat +datum=WGS84")
crs(spdf) <- CRS("+proj=longlat +datum=WGS84")

#intersect points with urban boundaries
o <- over(spdf, shape)
# o <- over(spdf[105788,], shape)
#spdf@data = cbind(spdf@data, shape[o,])
MarJunrecUrb <-cbind.data.frame(MarJunrec,o)

#drop unnecessary columns
MarJunrecUrb = subset(MarJunrecUrb, select = -c(UACE10,GEOID10,NAME10,NAMELSAD10,LSAD10,UATYP10,FUNCSTAT10,ALAND10,
                                                  AWATER10,INTPTLAT10,INTPTLON10) )

#convert N/A to 0, convert city name to 1, add to "urban" column
MarJunrecUrb$urban <- ifelse(is.na(MarJunrecUrb$MTFCC10) == TRUE,0,1)


#write out RECORDS csv in case I need this df again
write.csv(MarJunrec,'data/MarJun2016records.csv')

#remove duplicate records based on obs_date and individual_ID (turn df of records into a df of "observations")
MarJunObs <- MarJunrec %>% distinct(Observation_Date, Individual_ID, .keep_all= TRUE)

#write out OBSERVATIONS csv in case I need this df again
write.csv(MarJunObs,'data/MarJun2016obs.csv')


###############################################################

# append individual year OBSERVATION tables together

#load 2015 OBSERVATIONS
Obs2015 <- read_csv(file = "data/MarJun2015obs.csv")  

#rename column names in Obs2015

Obs2015 <- rename(Obs2015, c("observation_id"="Observation_ID", "dataset_id"="Dataset_ID", "observedby_person_id"="ObservedBy_Person_ID", "site_id"="Site_ID",
                  "latitude"="Latitude", "longitude"="Longitude", "elevation_in_meters"="Elevation_in_Meters", "state"="State", "common_name" = "Common_Name",
                  "kingdom"="Kingdom", "individual_id"="Individual_ID", "observation_date"="Observation_Date"))

#load 2016 OBSERVATIONS
Obs2016 <- read_csv(file = "data/MarJun2016obs.csv")  

#load 2017 OBSERVATIONS
Obs2017 <- read_csv(file = "data/MarJun2017obs.csv") 

#load 2018 OBSERVATIIONS
Obs2018 <- read_csv(file = "data/MarJun2018obs.csv") 

#load 2019 OBSERVATIIONS
Obs2019 <- read_csv(file = "data/MarJun2019obs.csv") 

#load 2020 OBSERVATIIONS
Obs2020 <- read_csv(file = "data/MarJun2020obs.csv") 

#rename column names in Obs2020
Obs2020 <- rename(Obs2020, c("observation_id"="Observation_ID", "dataset_id"="Dataset_ID", "observedby_person_id"="ObservedBy_Person_ID", "site_id"="Site_ID",
                             "latitude"="Latitude", "longitude"="Longitude", "elevation_in_meters"="Elevation_in_Meters", "state"="State", "common_name" = "Common_Name",
                             "kingdom"="Kingdom", "individual_id"="Individual_ID", "observation_date"="Observation_Date"))

#bind 2015 through 2020 data
NPNObs2015_2020 <- rbind(Obs2015, Obs2016, Obs2017, Obs2018, Obs2019, Obs2020)

#write out OBSERVATIONS for 2015-2020 as single csv
write.csv(NPNObs2015_2020,'data/NPN_MAMJObs_2015_2020.csv')


###############################################################

# append individual year RECORDS tables together

#load 2015 RECORDS
Recs2015 <- read_csv(file = "data/MarJun2015records.csv")  

#rename column names in Recs2015
Recs2015 <- rename(Recs2015, c("observation_id"="Observation_ID", "dataset_id"="Dataset_ID", "observedby_person_id"="ObservedBy_Person_ID", "site_id"="Site_ID",
                             "latitude"="Latitude", "longitude"="Longitude", "elevation_in_meters"="Elevation_in_Meters", "state"="State", "common_name" = "Common_Name",
                             "kingdom"="Kingdom", "individual_id"="Individual_ID", "observation_date"="Observation_Date"))

#load 2016 RECORDS
Recs2016 <- read_csv(file = "data/MarJun2016records.csv")  

#load 2017 RECORDS
Recs2017 <- read_csv(file = "data/MarJun2017records.csv")  

#load 2018 RECORDS
Recs2018 <- read_csv(file = "data/MarJun2018records.csv")  

#load 2019 RECORDS
Recs2019 <- read_csv(file = "data/MarJun2019records.csv")  

#load 2020 RECORDS
Recs2020 <- read_csv(file = "data/MarJun2020records.csv")  

#rename column names in Recs2020
Recs2020 <- rename(Recs2020, c("observation_id"="Observation_ID", "dataset_id"="Dataset_ID", "observedby_person_id"="ObservedBy_Person_ID", "site_id"="Site_ID",
                               "latitude"="Latitude", "longitude"="Longitude", "elevation_in_meters"="Elevation_in_Meters", "state"="State", "common_name" = "Common_Name",
                               "kingdom"="Kingdom", "individual_id"="Individual_ID", "observation_date"="Observation_Date"))


#bind 2015 through 2020 data
NPNRecs2015_2020 <- rbind(Recs2015, Recs2016, Recs2017, Recs2018, Recs2019, Recs2020)

#write out OBSERVATIONS for 2015-2020 as single csv
write.csv(NPNRecs2015_2020,'data/NPN_MAMJRecs_2015_2020.csv')

################################################################
#load 2020 OBSERVATIONS
NPNObs2015_2020 <- read_csv(file = "data/NPN_MAMJObs_2015_2020.csv")

# Determine number of unique observers in each year
ObvsYr <- ddply(NPNObs2015_2020, ~year, summarise, distinct_observers = length(unique(ObservedBy_Person_ID)))

# Determine count of unique sites at which data were collected in each year
SitesYr <- ddply(NPNObs2015_2020, ~year, summarise, distinct_sites = length(unique(Site_ID)))

###############################################################

# Determine top-observed taxa in MAMJ of given year

#remove duplicate records based on obs_date and individual_ID (turn df of records into a df of "observations")
MarJun2019nodups <- MarJun2019 %>% distinct(observation_date, individual_id, .keep_all= TRUE)

#group and count stuff up from previous step
records_taxa <- MarJun2019nodups %>% 
  group_by(common_name, year) %>%
  summarize(count = n())
records_taxa <- records_taxa[with(records_taxa, order(year, -count)), ]

#write out df as .csv
write.csv(records_taxa,'data/NPN-observations_by_taxa-2018.csv')


# Determine top-observed taxa by MONTH of given year
obs_taxa_moyr <- MarJun2019nodups %>% 
  group_by(common_name, month, year) %>%
  summarize(count = n())
obs_taxa_moyr <- obs_taxa_moyr[with(obs_taxa_moyr, order(month, -count)), ]

write.csv(obs_taxa_moyr,'data/NPN-observations_by_taxa-month2018.csv')



##STILL WORKING HERE
# for ACTIVITY tests: group records by SITE*DATE - BUT - do we want to retain species info, etc?
records_site_date <- MarJun2020 %>% 
  group_by(site_id, observation_date) %>%
  count(site_id)




#join w/original df to retain lat/long, etc?



