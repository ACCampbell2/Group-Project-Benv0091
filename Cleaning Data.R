## Loading libraries 

library(lubridate)
library(tidyverse)
library(stringr)
library(ggplot2)
library(glmnet)


# Import -------------------------------------------------------------------

## My working directory for the data

setwd("C:/Users/angus/OneDrive/Documents/Year 4 notes/Data Analysis for Energy Systems/Group Project")

## Read in weather data

meteo_2017 <- read.csv("sunlab-faro-meteo-2017.csv",sep = ";")
meteo_2016 <- read.csv("sunlab-faro-meteo-2016.csv",sep = ";")
meteo_2015 <- read.csv("sunlab-faro-meteo-2015.csv",sep = ";")
meteo_2014 <- read.csv("sunlab-faro-meteo-2014.csv",sep = ";")

## Read in Pv data

pvsunlab_2017 <- read.csv("sunlab-faro-pv-2017.csv",sep=";")
pvsunlab_2016 <- read.csv("sunlab-faro-pv-2016.csv",sep=";")
pvsunlab_2015 <- read.csv("sunlab-faro-pv-2015.csv",sep=";")
pvsunlab_2014 <- read.csv("sunlab-faro-pv-2014.csv",sep=";")



# Renaming columns --------------------------------------------------------

## Haven't included the units in the titles for now.

Weather_columns <- c("Datetime","Ambient_Temperature", "Global_Radiation", "Diffuse_Radiation",
                     "UV", "Wind_Velocity","Wind_Direction","Precipitation","Atmospheric_Pressure")


## Loop to change all the names for the weather data, can do this also by straight assignment but this way works too for now.

for(i in seq(2,length(meteo_2017))){
  names(meteo_2017)[i] <- Weather_columns[i]
}

for(i in seq(2,length(meteo_2016))){
  names(meteo_2016)[i] <- Weather_columns[i]
}

for(i in seq(2,length(meteo_2015))){
  names(meteo_2015)[i] <- Weather_columns[i]
}

for(i in seq(2,length(meteo_2014))){
  names(meteo_2014)[i] <- Weather_columns[i]
}

## To change the names of the generation data, using string replace to remove all the extra .'s and units.

names(pvsunlab_2017) <- str_replace_all(names(pvsunlab_2017),c("..ºC."="",".DC..A."="",".DC..W."="",".DC..V."="","\\.\\.\\."="_"))
names(pvsunlab_2016) <- str_replace_all(names(pvsunlab_2016),c("..ºC."="",".DC..A."="",".DC..W."="",".DC..V."="","\\.\\.\\."="_"))
names(pvsunlab_2015) <- str_replace_all(names(pvsunlab_2015),c("..ºC."="",".DC..A."="",".DC..W."="",".DC..V."="","\\.\\.\\."="_"))
names(pvsunlab_2014) <- str_replace_all(names(pvsunlab_2014),c("..ºC."="",".DC..A."="",".DC..W."="",".DC..V."="","\\.\\.\\."="_"))


# Checking data format ----------------------------------------------------

## To see size of dataset and the type of data stored in each column.

str(meteo_2017)
str(meteo_2016)
str(meteo_2015)
str(meteo_2014)

## Sometimes Ambient temperature and wwind velocity didn't load in as numeric 

meteo_2017$Ambient_Temperature <- as.numeric(meteo_2017$Ambient_Temperature)
meteo_2017$Wind_Velocity <- as.numeric(meteo_2017$Wind_Velocity)

meteo_2016$Ambient_Temperature <- as.numeric(meteo_2016$Ambient_Temperature)
meteo_2016$Wind_Velocity <- as.numeric(meteo_2016$Wind_Velocity)

meteo_2015$Ambient_Temperature <- as.numeric(meteo_2015$Ambient_Temperature)
meteo_2015$Wind_Velocity <- as.numeric(meteo_2015$Wind_Velocity)

meteo_2014$Ambient_Temperature <- as.numeric(meteo_2014$Ambient_Temperature)
meteo_2014$Wind_Velocity <- as.numeric(meteo_2014$Wind_Velocity)


# Editing Data -----------------------------------------------------------

## Adding columns for time and dates to make sorting and aggreagating the data simplier 


meteo_2017$Datetime <- ymd_hms(meteo_2017$Datetime)
meteo_2017$Date <- as.Date(as_date(meteo_2017$Datetime))
meteo_2017$Hour <- hour(meteo_2017$Datetime)
meteo_2017$Month <- month(meteo_2017$Datetime)
meteo_2017$Minutes <- minute(meteo_2017$Datetime)

meteo_2016$Datetime <- ymd_hms(meteo_2016$Datetime)
meteo_2016$Date <- as.Date(as_date(meteo_2016$Datetime))
meteo_2016$Hour <- hour(meteo_2016$Datetime)
meteo_2016$Month <- month(meteo_2016$Datetime)
meteo_2016$Minutes <- minute(meteo_2016$Datetime)

meteo_2015$Datetime <- ymd_hms(meteo_2015$Datetime)
meteo_2015$Date <- as.Date(as_date(meteo_2015$Datetime))
meteo_2015$Hour <- hour(meteo_2015$Datetime)
meteo_2015$Month <- month(meteo_2015$Datetime)
meteo_2015$Minutes <- minute(meteo_2015$Datetime)

meteo_2014$Datetime <- ymd_hms(meteo_2014$Datetime)
meteo_2014$Date <- as.Date(as_date(meteo_2014$Datetime))
meteo_2014$Hour <- hour(meteo_2014$Datetime)
meteo_2014$Month <- month(meteo_2014$Datetime)
meteo_2014$Minutes <- minute(meteo_2014$Datetime)

# Value Checking ----------------------------------------------------------

## Identify extreme values in the data

summary(meteo_2017)

## Removing extreme values by catergory 

## setting min and max ambient temperature should realistically be between 44 and -2 

meteo_2017 <- meteo_2017 %>% filter(Ambient_Temperature < 44, Ambient_Temperature > -5)

## Shouldn't have a negative for radiation or values of over 2,000 really so can set these as boundaries

meteo_2017 <- meteo_2017 %>% filter(Global_Radiation < 2000, Global_Radiation > -2)

## Diffuse is less than global radiation but not neccessarily the largest global radiation values that lead to largest diffuse 
## Max of about 800 
## May not be worth removing small negatives as rest of the data maybe reasonable.

meteo_2017 <- meteo_2017 %>% filter(Diffuse_Radiation < 1000, Diffuse_Radiation > -2)

meteo_2017 <- meteo_2017 %>% filter(UV < 500, UV > -2)

## One of these 3 removed all the data by accident, also is technically speed not velocity as doesn't have direction in one.

meteo_2017 <- meteo_2017 %>% filter(Wind_Velocity < 200, Wind_Velocity > -2)

meteo_2017 <- meteo_2017 %>% filter(Precipitation < 100, Precipitation > -2)

meteo_2017 <- meteo_2017 %>% filter(Atmospheric_Pressure < 2000, Atmospheric_Pressure > -2)



summary(meteo_2017)


## 2016 Cleaning following the same process

summary(meteo_2016)

meteo_2016 <- meteo_2016 %>% filter(Ambient_Temperature < 44, Ambient_Temperature > -5)
meteo_2016 <- meteo_2016 %>% filter(Global_Radiation < 2000, Global_Radiation > -2)
meteo_2016 <- meteo_2016 %>% filter(Diffuse_Radiation < 1000, Diffuse_Radiation > -2)
meteo_2016 <- meteo_2016 %>% filter(UV < 500, UV > -2)
meteo_2016 <- meteo_2016 %>% filter(Wind_Velocity < 200, Wind_Velocity > -2)
meteo_2016 <- meteo_2016 %>% filter(Precipitation < 100, Precipitation > -2)
meteo_2016 <- meteo_2016 %>% filter(Atmospheric_Pressure < 2000, Atmospheric_Pressure > -2)

summary(meteo_2016)

## 2015 Cleaning

summary(meteo_2015)

meteo_2015 <- meteo_2015 %>% filter(Ambient_Temperature < 44, Ambient_Temperature > -5)
meteo_2015 <- meteo_2015 %>% filter(Global_Radiation < 2000, Global_Radiation > -2)
meteo_2015 <- meteo_2015 %>% filter(Diffuse_Radiation < 1000, Diffuse_Radiation > -2)
meteo_2015 <- meteo_2015 %>% filter(UV < 500, UV > -2)
meteo_2015 <- meteo_2015 %>% filter(Wind_Velocity < 200, Wind_Velocity > -2)
meteo_2015 <- meteo_2015 %>% filter(Precipitation < 100, Precipitation > -2)
meteo_2015 <- meteo_2015 %>% filter(Atmospheric_Pressure < 2000, Atmospheric_Pressure > -2)

summary(meteo_2015)

## 2014 Cleaning

summary(meteo_2014)

meteo_2014 <- meteo_2014 %>% filter(Ambient_Temperature < 44, Ambient_Temperature > -5)
meteo_2014 <- meteo_2014 %>% filter(Global_Radiation < 2000, Global_Radiation > -2)
meteo_2014 <- meteo_2014 %>% filter(Diffuse_Radiation < 1000, Diffuse_Radiation > -2)
meteo_2014 <- meteo_2014 %>% filter(UV < 500, UV > -2)
meteo_2014 <- meteo_2014 %>% filter(Wind_Velocity < 200, Wind_Velocity > -2)
meteo_2014 <- meteo_2014 %>% filter(Precipitation < 100, Precipitation > -2)
meteo_2014 <- meteo_2014 %>% filter(Atmospheric_Pressure < 2000, Atmospheric_Pressure > -2)

summary(meteo_2014)


# Data Gaps ---------------------------------------------------------------

## 2017

meteo_2017 <- arrange(meteo_2017,Datetime)

meteo_2017 <- meteo_2017 %>% mutate(time_diff =(meteo_2017$Datetime-lag(meteo_2017$Datetime,1)))

meteo_2017$time_diff[is.na(meteo_2017$time_diff)]<-0

missing_d_2017 <- filter(meteo_2017, time_diff != 60) ## This also includes duplicate days
missing_d_2017 <- select(missing_d_2017,c(1,15))
str(missing_d_2017)

meteo_2017 <- filter(meteo_2017, time_diff == 60)

missing_d2 <- filter(meteo_2017, time_diff != 60) ## this checks if there are still time gaps 
str(missing_d2)

## time_diff isn't good for seeing missing chunks as they will still sum to the same amount generally if we include the base 60 but 
## can find missing times from them

## 2016

meteo_2016 <- arrange(meteo_2016,Datetime)

meteo_2016 <- meteo_2016 %>% mutate(time_diff =(meteo_2016$Datetime-lag(meteo_2016$Datetime,1)))

meteo_2016$time_diff[is.na(meteo_2016$time_diff)]<-0

missing_d_2016 <- filter(meteo_2016, time_diff != 60)
missing_d_2016 <- select(missing_d_2016,c(1,15))
str(missing_d_2016)

meteo_2016 <- filter(meteo_2016, time_diff == 60)

## 2015 

meteo_2015 <- arrange(meteo_2015,Datetime)

meteo_2015 <- meteo_2015 %>% mutate(time_diff =(meteo_2015$Datetime-lag(meteo_2015$Datetime,1)))

meteo_2015$time_diff[is.na(meteo_2015$time_diff)]<-0

missing_d_2015 <- filter(meteo_2015, time_diff != 60) 
missing_d_2015 <- select(missing_d_2015,c(1,15))
str(missing_d_2015)

meteo_2015 <- filter(meteo_2015, time_diff == 60)

## 2014

meteo_2014 <- arrange(meteo_2014,Datetime)

meteo_2014 <- meteo_2014 %>% mutate(time_diff =(meteo_2014$Datetime-lag(meteo_2014$Datetime,1)))

meteo_2014$time_diff[is.na(meteo_2014$time_diff)]<-0

missing_d_2014 <- filter(meteo_2014, time_diff != 60)
missing_d_2014 <- select(missing_d_2014,c(1,15))
str(missing_d_2014)

meteo_2014 <- filter(meteo_2014, time_diff == 60)

## Removes duplicates too

# Group/Aggregate the data ----------------------------------------------------------

## 2017

## May want to group the data into months at first

By_month_2017 <- meteo_2017 %>% group_by(Month) %>% 
  summarise(mean_temp = mean(Ambient_Temperature), max_temp = max(Ambient_Temperature), 
            min_temp = min(Ambient_Temperature), mean_radiation = mean(Global_Radiation), mean_wind_vel = mean(Wind_Velocity),
            mean_precip = mean(Precipitation), mean_atmo = mean(Atmospheric_Pressure), count = n())

ggplot(By_month_2017)+geom_line(aes(x=Month, y=mean_radiation))

By_day_2017 <- meteo_2017 %>% group_by(Date) %>% 
  summarise(mean_temp = mean(Ambient_Temperature), max_temp = max(Ambient_Temperature), 
            min_temp = min(Ambient_Temperature), mean_radiation = mean(Global_Radiation), mean_wind_vel = mean(Wind_Velocity),
            mean_precip = mean(Precipitation), mean_atmo = mean(Atmospheric_Pressure), count = n(),
            missing_time_mins = (1440-n()))

## per day there should be 1440 counts, can calculate the missing time for each day by this 

ggplot(By_day_2017)+geom_line(aes(x=Date, y=missing_time_mins))

## only one day in march with duplicates (26th) the rest have days missing
## find these days and remove them?

## Aggregating to 5 min intervals 

Five_Min_intervals_2017 <- meteo_2017 %>% group_by(Datetime = cut(Datetime, breaks = "5 min")) %>%
  summarise(Ambient_Temperature = mean(Ambient_Temperature), Global_Radiation = mean(Global_Radiation),
            Diffuse_Radiation = mean(Diffuse_Radiation), UV = mean(UV), Wind_Velocity = mean(Wind_Velocity),
            Wind_Direction = mean(Wind_Direction), Precipitation = mean(Precipitation), 
            Atmospheric_Pressure = mean(Atmospheric_Pressure),count = n(), missing_time_mins = (5-n()))

## Should be 105120 counts in total there are 104964


## 2016

By_month_2016 <- meteo_2016 %>% group_by(Month) %>% 
  summarise(mean_temp = mean(Ambient_Temperature), max_temp = max(Ambient_Temperature), 
            min_temp = min(Ambient_Temperature), mean_radiation = mean(Global_Radiation), mean_wind_vel = mean(Wind_Velocity),
            mean_precip = mean(Precipitation), mean_atmo = mean(Atmospheric_Pressure), count = n())

By_day_2016 <- meteo_2016 %>% group_by(Date) %>% 
  summarise(mean_temp = mean(Ambient_Temperature), max_temp = max(Ambient_Temperature), 
            min_temp = min(Ambient_Temperature), mean_radiation = mean(Global_Radiation), mean_wind_vel = mean(Wind_Velocity),
            mean_precip = mean(Precipitation), mean_atmo = mean(Atmospheric_Pressure), count = n(),
            missing_time_mins = (1440-n()))

Five_Min_intervals_2016 <- meteo_2016 %>% group_by(Datetime = cut(Datetime, breaks = "5 min")) %>%
  summarise(Ambient_Temperature = mean(Ambient_Temperature), Global_Radiation = mean(Global_Radiation),
            Diffuse_Radiation = mean(Diffuse_Radiation), UV = mean(UV), Wind_Velocity = mean(Wind_Velocity),
            Wind_Direction = mean(Wind_Direction), Precipitation = mean(Precipitation), 
            Atmospheric_Pressure = mean(Atmospheric_Pressure),count = n(), missing_time_mins = (5-n()))

## 2015

By_month_2015 <- meteo_2015 %>% group_by(Month) %>% 
  summarise(mean_temp = mean(Ambient_Temperature), max_temp = max(Ambient_Temperature), 
            min_temp = min(Ambient_Temperature), mean_radiation = mean(Global_Radiation), mean_wind_vel = mean(Wind_Velocity),
            mean_precip = mean(Precipitation), mean_atmo = mean(Atmospheric_Pressure), count = n())

By_day_2015 <- meteo_2015 %>% group_by(Date) %>% 
  summarise(mean_temp = mean(Ambient_Temperature), max_temp = max(Ambient_Temperature), 
            min_temp = min(Ambient_Temperature), mean_radiation = mean(Global_Radiation), mean_wind_vel = mean(Wind_Velocity),
            mean_precip = mean(Precipitation), mean_atmo = mean(Atmospheric_Pressure), count = n(),
            missing_time_mins = (1440-n()))

Five_Min_intervals_2015 <- meteo_2015 %>% group_by(Datetime = cut(Datetime, breaks = "5 min")) %>%
  summarise(Ambient_Temperature = mean(Ambient_Temperature), Global_Radiation = mean(Global_Radiation),
            Diffuse_Radiation = mean(Diffuse_Radiation), UV = mean(UV), Wind_Velocity = mean(Wind_Velocity),
            Wind_Direction = mean(Wind_Direction), Precipitation = mean(Precipitation), 
            Atmospheric_Pressure = mean(Atmospheric_Pressure),count = n(), missing_time_mins = (5-n()))

## 2014

By_month_2014 <- meteo_2014 %>% group_by(Month) %>% 
  summarise(mean_temp = mean(Ambient_Temperature), max_temp = max(Ambient_Temperature), 
            min_temp = min(Ambient_Temperature), mean_radiation = mean(Global_Radiation), mean_wind_vel = mean(Wind_Velocity),
            mean_precip = mean(Precipitation), mean_atmo = mean(Atmospheric_Pressure), count = n())

By_day_2014 <- meteo_2014 %>% group_by(Date) %>% 
  summarise(mean_temp = mean(Ambient_Temperature), max_temp = max(Ambient_Temperature), 
            min_temp = min(Ambient_Temperature), mean_radiation = mean(Global_Radiation), mean_wind_vel = mean(Wind_Velocity),
            mean_precip = mean(Precipitation), mean_atmo = mean(Atmospheric_Pressure), count = n(),
            missing_time_mins = (1440-n()))

Five_Min_intervals_2014 <- meteo_2014 %>% group_by(Datetime = cut(Datetime, breaks = "5 min")) %>%
  summarise(Ambient_Temperature = mean(Ambient_Temperature), Global_Radiation = mean(Global_Radiation),
            Diffuse_Radiation = mean(Diffuse_Radiation), UV = mean(UV), Wind_Velocity = mean(Wind_Velocity),
            Wind_Direction = mean(Wind_Direction), Precipitation = mean(Precipitation), 
            Atmospheric_Pressure = mean(Atmospheric_Pressure),count = n(), missing_time_mins = (5-n()))





