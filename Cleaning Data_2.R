## Loading libraries 

library(lubridate)
library(tidyverse)
library(stringr)
library(ggplot2)
library(glmnet)
library(rvest)
library(httr)
library(jsonlite)
library(modelr)


# Import -------------------------------------------------------------------

## My working directory for the data

setwd("C:/Users/angus/OneDrive/Documents/Year 4 notes/Data Analysis for Energy Systems/Group Project")

## Read in weather data

meteo_2017 <- read.csv("sunlab-faro-meteo-2017.csv",sep = ";")
meteo_2016 <- read.csv("sunlab-faro-meteo-2016.csv",sep = ";")


## Read in Pv data

pvsunlab_2017 <- read.csv("sunlab-faro-pv-2017.csv",sep=";")
pvsunlab_2016 <- read.csv("sunlab-faro-pv-2016.csv",sep=";")


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


## To change the names of the generation data, using string replace to remove all the extra .'s and units.

names(pvsunlab_2017) <- str_replace_all(names(pvsunlab_2017),c("..ºC."="",".DC..A."="",".DC..W."="",".DC..V."="","\\.\\.\\."="_"))
names(pvsunlab_2016) <- str_replace_all(names(pvsunlab_2016),c("..ºC."="",".DC..A."="",".DC..W."="",".DC..V."="","\\.\\.\\."="_"))


# Checking data format ----------------------------------------------------

## To see size of dataset and the type of data stored in each column.

str(meteo_2017)
str(meteo_2016)


## Sometimes Ambient temperature and wwind velocity didn't load in as numeric 

meteo_2017$Ambient_Temperature <- as.numeric(meteo_2017$Ambient_Temperature)
meteo_2017$Wind_Velocity <- as.numeric(meteo_2017$Wind_Velocity)

meteo_2016$Ambient_Temperature <- as.numeric(meteo_2016$Ambient_Temperature)
meteo_2016$Wind_Velocity <- as.numeric(meteo_2016$Wind_Velocity)



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

pvsunlab_2017$Datetime <- ymd_hms(pvsunlab_2017$Datetime)
pvsunlab_2016$Datetime <- ymd_hms(pvsunlab_2016$Datetime)

## Also exclude atmospheric pressure and precipitation due to numerous NA values

meteo_2017_wo <- subset(meteo_2017, select = -c(Precipitation, Atmospheric_Pressure))
meteo_2016_wo<- subset(meteo_2016, select = -c(Precipitation, Atmospheric_Pressure))

data1 = full_join(meteo_2014,meteo_2015)
data2= full_join(meteo_2016,meteo_2017)
total_meteo=full_join(data1,data2)

total_pvsunlab =full_join(pvsunlab_2016,pvsunlab_2017)

# Value Checking ----------------------------------------------------------
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

## Rainfall is minimum in portugal and so I will make an assumption that the NA's will indicate no rainfall that day.

meteo_2016 <- meteo_2016 %>% filter(Precipitation < 100, Precipitation > -2) ## Removes a lot of data likely due to NA's

meteo_2016 <- meteo_2016 %>% filter(Atmospheric_Pressure < 2000, Atmospheric_Pressure > -2)
## Identify extreme values in the data

## Precipitation proves a large problem here as the NA's will lead to many values being removed 

summary(total_meteo)

## Removing extreme values by catergory 

## setting min and max ambient temperature should realistically be between 44 and -2 

total_meteo <- total_meteo %>% filter(Ambient_Temperature < 44, Ambient_Temperature > -5)

## Shouldn't have a negative for radiation or values of over 2,000 really so can set these as boundaries

total_meteo <- total_meteo %>% filter(Global_Radiation < 2000, Global_Radiation > -2)

## Diffuse is less than global radiation but not neccessarily the largest global radiation values that lead to largest diffuse 
## Max of about 800 
## May not be worth removing small negatives as rest of the data maybe reasonable.

total_meteo <- total_meteo %>% filter(Diffuse_Radiation < 1000, Diffuse_Radiation > -2)

total_meteo <- total_meteo %>% filter(UV < 500, UV > -2)

## One of these 3 removed all the data by accident, also is technically speed not velocity as doesn't have direction in one.

total_meteo <- total_meteo %>% filter(Wind_Velocity < 200, Wind_Velocity > -2)

total_meteo <- total_meteo %>% filter(Precipitation < 100, Precipitation > -2)

total_meteo <- total_meteo %>% filter(Atmospheric_Pressure < 2000, Atmospheric_Pressure > -2)

summary(total_meteo)


# Transforming Data -------------------------------------------------------

## Wind directions is a meaningless vector at the moment, can use cos() to give it applicable value

total_meteo <- mutate(total_meteo, Wind_Direction = cos(Wind_Direction))

total_meteo <- mutate(total_meteo, Radiation_ratio = Diffuse_Radiation/Global_Radiation)

a <- subset(total_meteo, Radiation_ratio >0.95)

## 01-01-2014 was a cloudy day especially morining till 9 and after 3 next day also cloudy apart from 12-3/5
## Also works for 2014-04-15

## Could generalize saying any value of 0.9 is either morning or an overacast day?

Overcast_data <- a


combined_overcast <- left_join(total_pvsunlab,Overcast_data,by="Datetime")
combined_overcast <- arrange(combined_overcast,Datetime)

combined_overcast <- combined_overcast[!is.na(combined_overcast$B_Optimal_Power), ]

combined_overcast <- drop_na(combined_overcast)

x_overcast <- as.matrix(combined_overcast[,c(26:31,36)])
y_overcast <- as.matrix(combined_overcast[,19])


ridge <- glmnet(x_overcast,y_overcast, intercept= FALSE, family = "gaussian",alpha = 0)
print(ridge)

plot(ridge,xvar ="lambda")

cvfit_overcast <- cv.glmnet(x_overcast,y_overcast)
plot(cvfit_overcast)

cvfit_overcast$lambda.min
cvfit_overcast$lambda.1se

s_coef <- coef.cv.glmnet(cvfit_overcast, s="lambda.1se")


prediction <- predict(cvfit_overcast,x_overcast,s="lambda.1se")
combined_overcast <- data.frame(combined_overcast,prediction)
names(combined_overcast)[37] <- "Overcast_Prediction"

ggplot(combined_overcast[1:40000,])+geom_line(aes(x=Datetime, y=B_Optimal_Power),col='red')+
  geom_line(aes(x=Datetime, y= Overcast_Prediction), col='blue')

difference <- prediction - y_overcast


plot(combined_overcast$Datetime,prediction, type = "l")

data.f <- data.frame(combined_overcast$Datetime,prediction,y_overcast)
ggplot(data.f,aes(combined_overcast$Datetime))+ geom_line(aes(y=difference),colour = "red")

summary(difference)

ggplot(meteo_2016)+geom_line(aes(x=Datetime,y=Ambient_Temperature))
## Seems to over predict by a constant value and under predicts more often than over.

b <- subset(total_meteo, Radiation_ratio <= 0.95)

Sunny_data <- b


combined_sunny <- left_join(total_pvsunlab,Sunny_data,by="Datetime")
combined_sunny <- arrange(combined_sunny,Datetime)

combined_overcast <- combined_overcast[!is.na(combined_overcast$B_Optimal_Power), ]

combined_overcast <- drop_na(combined_overcast)

x_overcast <- as.matrix(combined_overcast[,c(26:31,36)])
y_overcast <- as.matrix(combined_overcast[,19])



ridge <- glmnet(x_overcast,y_overcast,family = "gaussian",alpha = 0)
print(ridge)

plot(ridge,xvar ="lambda")

cvfit_overcast <- cv.glmnet(x_overcast,y_overcast)
plot(cvfit_overcast)

cvfit_overcast$lambda.min
cvfit_overcast$lambda.1se

coef.cv.glmnet(cvfit_overcast, s="lambda.1se")
prediction <- predict(cvfit_overcast,x_overcast,s="lambda.1se")

difference <- prediction - y_overcast

summary(combined_2014$B_Optimal_Power)

plot(combined_overcast$Datetime,prediction, type = "l")

data.f <- data.frame(combined_overcast$Datetime,prediction,y_overcast)
ggplot(data.f,aes(combined_overcast$Datetime))+ geom_line(aes(y=difference),colour = "red")

summary(difference)

# Web Scrapping  ----------------------------------------------------------

## To get information of cloudy days

url <- 'https://www.worldweatheronline.com/faro-weather-history/faro/pt.aspx'

faro_webpage <- read_html(url)

## Can't see /faro-weather-history/ or /faro/ or /pt.aspx in robots.txt file so assume it is ok to scrape this page

dates <- html_nodes(faro_webpage, ".MainContentHolder_txtPastDate")
head(dates)
 API_key <- '7065813102924e24939231735190901'
 
url <- GET(url = 'http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key=7065813102924e24939231735190901&q=London&format=json&extra=Faro&date=2017-12-01&enddate=2017-12-31&includelocation=yes&show_comments=yes&tp=tp=1')

fromJSON(url)

## dUNNO

# Group/Aggregate the data ----------------------------------------------------------

## 2017

## May want to group the data into months at first

By_month <- total_meteo %>% group_by(Month) %>% 
  summarise(mean_temp = mean(Ambient_Temperature), max_temp = max(Ambient_Temperature), 
            min_temp = min(Ambient_Temperature), mean_radiation = mean(Global_Radiation), mean_wind_vel = mean(Wind_Velocity),
            mean_precip = mean(Precipitation), mean_atmo = mean(Atmospheric_Pressure), count = n())

ggplot(By_month_2017)+geom_line(aes(x=Month, y=mean_radiation))

By_day <- total_meteo %>% group_by(Date) %>% 
  summarise(mean_temp = mean(Ambient_Temperature), max_temp = max(Ambient_Temperature), 
            min_temp = min(Ambient_Temperature), mean_radiation = mean(Global_Radiation), mean_wind_vel = mean(Wind_Velocity),
            mean_precip = mean(Precipitation), mean_atmo = mean(Atmospheric_Pressure), count = n(),
            missing_time_mins = (1440-n()))

## per day there should be 1440 counts, can calculate the missing time for each day by this 

ggplot(By_day)+geom_line(aes(x=Date, y=missing_time_mins))

## only one day in march with duplicates (26th) the rest have days missing
## find these days and remove them?

## Aggregating to 5 min intervals 

Five_Min_intervals <- total_meteo %>% group_by(Datetime = cut(Datetime, breaks = "5 min")) %>%
  summarise(Ambient_Temperature = mean(Ambient_Temperature), Global_Radiation = mean(Global_Radiation),
            Diffuse_Radiation = mean(Diffuse_Radiation), UV = mean(UV), Wind_Velocity = mean(Wind_Velocity),
            Wind_Direction = mean(Wind_Direction), Precipitation = mean(Precipitation), 
            Atmospheric_Pressure = mean(Atmospheric_Pressure),count = n(), missing_time_mins = (5-n()))








