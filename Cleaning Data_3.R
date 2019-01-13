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
library(randomForest)
library(tree)
library(stats)
library(dynlm)
library(leaps)
library(MASS)
library(caret)

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
meteo_2016 <- meteo_2016 %>% filter(Precipitation < 100, Precipitation > -2) 
meteo_2016 <- meteo_2016 %>% filter(Atmospheric_Pressure < 2000, Atmospheric_Pressure > -2)

#Add columns for date and time for pvsunlab_2016, delete the useless columns 
pvsunlab_2016 = pvsunlab_2016[,c(1,19)]
pvsunlab_2016$Datetime <- ymd_hms(pvsunlab_2016$Datetime)
pvsunlab_2016$Date <- as.Date(as_date(pvsunlab_2016$Datetime))
pvsunlab_2016$Year <- year(pvsunlab_2016$Datetime)
pvsunlab_2016$Month <- month(pvsunlab_2016$Datetime)
pvsunlab_2016$MDay <- mday(pvsunlab_2016$Datetime)
pvsunlab_2016$Hour <- hour(pvsunlab_2016$Datetime)
pvsunlab_2016$Minutes <- minute(pvsunlab_2016$Datetime)

#Repeat the same directions for pvsunlab_2017
#Use 2016 dataset to create the neural network
#Add columns for date and time for pvsunlab_2016, delete the useless columns 
pvsunlab_2017 = pvsunlab_2017[,c(1,19)]
pvsunlab_2017$Datetime <- ymd_hms(pvsunlab_2017$Datetime)
pvsunlab_2017$Date <- as.Date(as_date(pvsunlab_2017$Datetime))
pvsunlab_2017$Year <- year(pvsunlab_2017$Datetime)
pvsunlab_2017$Hour <- hour(pvsunlab_2017$Datetime)
pvsunlab_2017$Month <- month(pvsunlab_2017$Datetime)
pvsunlab_2017$MDay <- mday(pvsunlab_2017$Datetime)
pvsunlab_2017$Minutes <- minute(pvsunlab_2017$Datetime)

t#combine the data
total_2016 = full_join(pvsunlab_2016,meteo_2016)
total_2016 <- drop_na(total_2016)
total_2017 = full_join(pvsunlab_2017,meteo_2017)
total_sun = full_join(total_2016,total_2017)
total_sun <- drop_na(total_sun)
total_sun = total_sun[,c(1,3:7,2,9:16)]

summary(total_sun)

# Transforming Data -------------------------------------------------------

## Wind directions is a meaningless vector at the moment, can use cos() to give it applicable value

total_sun <- mutate(total_sun, Wind_Direction = cos(Wind_Direction))

total_sun <- mutate(total_sun, Radiation_ratio = Diffuse_Radiation/Global_Radiation)

## 01-01-2014 was a cloudy day especially morining till 9 and after 3 next day also cloudy apart from 12-3/5
## Also works for 2014-04-15

## Could generalize saying any value of 0.9 is either morning or an overacast day?

combined_data <- total_sun
combined_data <- arrange(combined_data,Datetime)
combined_data <- combined_data %>% filter(B_Optimal_Power < 243)

## Create a sample of the data to keep as testing. 


# Exploratory Graphs ------------------------------------------------------

## Plot to show distribution of temperature over the two years.

ggplot(combined_data)+geom_boxplot(aes(x=as.factor(Month), y=Ambient_Temperature))

## Now comparing each year seperately

combined_data$Year <- year(combined_data$Datetime)

ggplot(combined_data)+geom_line(aes(x=as.factor(Month), y= Ambient_Temperature, color = Year))

by_date <- combined_data %>% group_by(Date) %>%
summarise(B_Optimal_Power = mean(B_Optimal_Power), Ambient_Temperature = mean(Ambient_Temperature), Global_Radiation = mean(Global_Radiation),
         Diffuse_Radiation = mean(Global_Radiation),UV = mean(UV), Wind_Velocity = mean(Wind_Velocity),
        Wind_Direction = mean(Wind_Direction), Precipitation = mean(Precipitation), 
                              Atmospheric_Pressure =mean(Atmospheric_Pressure), Year = mean(Year), Month = mean(Month))
  
ggplot(by_date,aes(x = Date, y= Ambient_Temperature, color = Year))+ geom_point()+ geom_smooth(col = 'black')

ggplot(by_date,aes(x = Date, y= B_Optimal_Power, color = Year))+ geom_point()+ geom_smooth(col = 'black')

ggplot(by_date,aes(x = as.factor(Month), y= B_Optimal_Power))+ geom_boxplot()

## More consistent power outputs in the summer

# Classification ----------------------------------------------------------

ggplot(combined_data)+geom_point(aes(x=Radiation_ratio, y= B_Optimal_Power))

regression_tree <- tree(B_Optimal_Power ~ Global_Radiation + Diffuse_Radiation, data = combined_data)
plot(regression_tree)
text(regression_tree, cex=1)

regression_tree_2 <- tree(B_Optimal_Power ~ Radiation_ratio, data = combined_data)
plot(regression_tree_2)
text(regression_tree_2, cex=1)

full_regression_tree <- tree(B_Optimal_Power ~., data = combined_data[,c(7:16)])
plot(full_regression_tree)
text(full_regression_tree, splits =TRUE,adj = c(0.5,-0.75), cex = 0.7)
  
summary(full_regression_tree)

combined_overcast <- subset(combined_data, Radiation_ratio >0.36 & Radiation_ratio < 1.2)
train_sample_overcast <- sample(1:nrow(combined_overcast),0.8*nrow(combined_overcast))
combined_overcast <- combined_overcast[train_sample_overcast,]
combined_overcast_test <- combined_overcast[-train_sample_overcast,]

combiend_sunny <- subset(combined_data, Radiation_ratio <= 0.36)
train_sample_sunny <- sample(1:nrow(combined_sunny),0.9*nrow(combined_sunny))
combined_sunny <- combined_sunny[train_sample_sunny,]
combined_sunny_test <- combined_sunny[-train_sample_sunny,]



## Could possibly split these down into two sub groups again as suggested by the regression tree

# Stepwise regression -----------------------------------------------------

## Remove the other pvsunlab values from dataset

cropped_sunny <- combined_sunny

full_sunny_model <- lm(B_Optimal_Power ~ .-Datetime,
                       data = cropped_sunny)

step_sunny_model <- stepAIC(full_sunny_model, direction = "both", trace=FALSE)
summary(step_sunny_model)

## R^2 of 0.6424 including temp, global, diffuse and UV radiation, Wind Velocity and Atmo Pressure. Indluing dates massively increases R^2 to 0.75

cropped_overcast <- combined_overcast

full_overcast_model <- lm(B_Optimal_Power ~ .-Datetime,
                          data = cropped_overcast)

step_overcast_model <- stepAIC(full_overcast_model, direction = "both", trace=FALSE)
summary(step_overcast_model)


# Normal Linear Regression ------------------------------------------------

regression_model_overcast <- lm(B_Optimal_Power ~ 
                                  Ambient_Temperature+Global_Radiation+Diffuse_Radiation+UV+
                                  Wind_Velocity+Atmospheric_Pressure+Hour+Month,
                                data = combined_overcast)

reg_overcast <- add_predictions(combined_overcast,regression_model_overcast, var ="Regression_Prediction")
reg_overcast <- add_residuals(reg_overcast,regression_model_overcast, var ="Regression_Residuals")

summary(regression_model_overcast)

regression_model_sunny <- lm(B_Optimal_Power ~ 
                                  Ambient_Temperature+Global_Radiation+
                               Wind_Velocity+UV+Atmospheric_Pressure+Month+Hour,
                                data = combined_sunny)


reg_sunny <- add_predictions(combined_sunny,regression_model_sunny, var ="Regression_Prediction")
reg_sunny <- add_residuals(reg_sunny,regression_model_sunny, var ="Regression_Residuals")

summary(regression_model_sunny)

# Another subset ----------------------------------------------------------

combined_overcast_1 <- subset(combined_overcast, Radiation_ratio <= 0.871)
combined_overcast_2 <- subset(combined_overcast, Radiation_ratio > 0.871)

cropped_overcast_1 <- combined_overcast_1

full_overcast_model_1 <- lm(B_Optimal_Power ~ .-Datetime,
                          data = cropped_overcast_1)

step_overcast_model_1 <- stepAIC(full_overcast_model_1, direction = "both", trace=FALSE)
summary(step_overcast_model_1)

cropped_overcast_2 <- combined_overcast_2

full_overcast_model_2 <- lm(B_Optimal_Power ~ .-Datetime,
                            data = cropped_overcast_2)
summary(full_overcast_model)
step_overcast_model_2 <- stepAIC(full_overcast_model_2, direction = "both", trace=FALSE)
summary(step_overcast_model_2)

## model_2 is worse of the two

combined_sunny_1 <- subset(combined_sunny, Radiation_ratio < 0.163)
combined_sunny_2 <- subset(combined_sunny, Radiation_ratio >= 0.163)

cropped_sunny_1 <- combined_sunny_1

full_sunny_model_1 <- lm(B_Optimal_Power ~ .-Datetime-Sunny_Prediction,
                       data = cropped_sunny_1)

step_sunny_model_1 <- stepAIC(full_sunny_model_1, direction = "both", trace=FALSE)
summary(step_sunny_model_1)

cropped_sunny_2 <- combined_sunny_2

full_sunny_model_2 <- lm(B_Optimal_Power ~ .-Datetime-Sunny_Prediction,
                       data = cropped_sunny_2)

step_sunny_model_2 <- stepAIC(full_sunny_model_2, direction = "both", trace=FALSE)
summary(step_sunny_model_2)

## Worse for model 2 but model 1 is much better??



# Further Regression Overcast Model -----------------------------------------------

regression_model_overcast_1 <- lm(B_Optimal_Power ~ 
                                  Ambient_Temperature+Global_Radiation+Diffuse_Radiation+UV+
                                  Wind_Velocity+Atmospheric_Pressure+Hour+Month,
                                data = combined_overcast_1)

reg_overcast <- add_predictions(reg_overcast,regression_model_overcast_1, var ="Regression_Prediction_1")
reg_overcast <- add_residuals(reg_overcast,regression_model_overcast_1, var ="Regression_Residuals_1")

summary(regression_model_overcast_1)

regression_model_overcast_2 <- lm(B_Optimal_Power ~ 
                                    Ambient_Temperature+Global_Radiation+Diffuse_Radiation+UV+
                                    Wind_Velocity+Atmospheric_Pressure+Hour+Month,
                                  data = combined_overcast_2)

reg_overcast <- add_predictions(reg_overcast,regression_model_overcast_2, var ="Regression_Prediction_2")
reg_overcast <- add_residuals(reg_overcast,regression_model_overcast_2, var ="Regression_Residuals_2")

summary(regression_model_overcast_2)

ggplot(reg_overcast[115000:118000,])+
  geom_line(aes(x=Datetime, y=Regression_Prediction_1), col = 'blue')+
  geom_point(aes(x=Datetime, y=B_Optimal_Power))

ggplot(reg_overcast[115000:118000,])+
  geom_line(aes(x=Datetime, y=Regression_Prediction_2), col = 'red')+
  geom_point(aes(x=Datetime, y=B_Optimal_Power))

ggplot(reg_overcast[115000:118000,])+
  geom_line(aes(x=Datetime, y=Regression_Prediction), col = 'black')+
  geom_point(aes(x=Datetime, y=B_Optimal_Power))

ggplot(reg_overcast[115000:118000,])+
  geom_line(aes(x=Datetime, y=Regression_Prediction), col = 'black')+
  geom_line(aes(x=Datetime, y=Regression_Prediction_1), col = 'blue')+
  geom_line(aes(x=Datetime, y=Regression_Prediction_2), col = 'red')+
  geom_point(aes(x=Datetime, y=B_Optimal_Power))

ggplot(reg_overcast[117500:118000,])+
  geom_line(aes(x=Datetime, y=Regression_Prediction_1), col = 'blue')+
  geom_line(aes(x=Datetime, y=Regression_Prediction_2), col = 'red')


# Further Regression Sunny Model ------------------------------------------

regression_model_sunny_1 <- lm(B_Optimal_Power ~ 
                                 Ambient_Temperature+Global_Radiation+Diffuse_Radiation+
                                 UV+Atmospheric_Pressure+Month+Hour,
                               data = combined_sunny_1)

reg_sunny <- add_predictions(reg_sunny,regression_model_sunny_1, var ="Regression_Prediction_1")
reg_sunny <- add_residuals(reg_sunny,regression_model_sunny_1, var ="Regression_Residuals_1")

summary(regression_model_sunny_1)

regression_model_sunny_2 <- lm(B_Optimal_Power ~ 
                               Ambient_Temperature+Global_Radiation+Diffuse_Radiation+Precipitation+
                               Wind_Velocity+UV+Atmospheric_Pressure+Month+Hour,
                             data = combined_sunny_2)

reg_sunny <- add_predictions(reg_sunny,regression_model_sunny_2, var ="Regression_Prediction_2")
reg_sunny <- add_residuals(reg_sunny,regression_model_sunny_2, var ="Regression_Residuals_2")

summary(regression_model_sunny_2)


ggplot(reg_sunny[115000:118000,])+
  geom_line(aes(x=Datetime, y=Regression_Prediction_1), col = 'blue')+
  geom_point(aes(x=Datetime, y=B_Optimal_Power))

ggplot(reg_sunny[115000:118000,])+
  geom_line(aes(x=Datetime, y=Regression_Prediction_2), col = 'red')+
  geom_point(aes(x=Datetime, y=B_Optimal_Power))

ggplot(reg_sunny[115000:118000,])+
  geom_line(aes(x=Datetime, y=Regression_Prediction), col = 'black')+
  geom_point(aes(x=Datetime, y=B_Optimal_Power))

ggplot(reg_sunny[115000:118000,])+
  geom_line(aes(x=Datetime, y=Regression_Prediction), col = 'black')+
  geom_line(aes(x=Datetime, y=Regression_Prediction_1), col = 'blue')+
  geom_line(aes(x=Datetime, y=Regression_Prediction_2), col = 'red')+
  geom_point(aes(x=Datetime, y=B_Optimal_Power))

ggplot(reg_sunny[117500:118000,])+
  geom_line(aes(x=Datetime, y=Regression_Prediction_1), col = 'blue')+
  geom_line(aes(x=Datetime, y=Regression_Prediction_2), col = 'red')+
  geom_point(aes(x=Datetime, y=B_Optimal_Power))


# Final Sunlab regression -------------------------------------------------

## The previous plots were all compared on similar times however really we would like to select the appropriate regression model
## For a specific day/ time of day e.g. cloudy or sunny
## There create a function to run a certain model depending on the value of radiation ratio


sun_1 <- subset(combined_data, Radiation_ratio <= 0.163)
sun_2 <- subset(combined_data, Radiation_ratio <=0.361 & Radiation_ratio > 0.163)
overcast_1 <- subset(combined_data, Radiation_ratio > 0.361 & Radiation_ratio <= 0.871)
overcast_2 <- subset(combined_data, Radiation_ratio > 0.871)


sun_1 <- add_predictions(sun_1, regression_model_sunny_1, var = "Regression_Prediction")
sun_2 <- add_predictions(sun_2, regression_model_sunny_2, var = "Regression_Prediction")
overcast_1 <- add_predictions(overcast_1, regression_model_overcast_1, var = "Regression_Prediction")
overcast_2 <- add_predictions(overcast_2, regression_model_overcast_2, var = "Regression_Prediction")

sun <- full_join(sun_1,sun_2)
overcast <- full_join(overcast_1,overcast_2)
final_regression_data <- full_join(sun,overcast)

## Remove negatie values for Power

ggplot(final_regression_data)+geom_line(aes(x=Datetime, y= B_Optimal_Power),col='red')+
  geom_line(aes(x=Datetime, y=Regression_Prediction),col='blue')

final_regression_data$Regression_Prediction

r2_value <- cor(final_regression_data$B_Optimal_Power, final_regression_data$Regression_Prediction)^2
r2_value

## Using the test sample to verify the R^2 value?

sun_1_test <- subset(combined_sunny_test, Radiation_ratio <= 0.163)
sun_2_test <- subset(combined_sunny_test, Radiation_ratio <=0.361 & Radiation_ratio > 0.163)
overcast_1_test <- subset(combined_overcast_test, Radiation_ratio > 0.361 & Radiation_ratio <= 0.871)
overcast_2_test <- subset(combined_overcast_test, Radiation_ratio > 0.871)


sun_1_test <- add_predictions(sun_1_test, regression_model_sunny_1, var = "Regression_Prediction")
sun_2_test <- add_predictions(sun_2_test, regression_model_sunny_2, var = "Regression_Prediction")
overcast_1_test <- add_predictions(overcast_1_test, regression_model_overcast_1, var = "Regression_Prediction")
overcast_2_test <- add_predictions(overcast_2_test, regression_model_overcast_2, var = "Regression_Prediction")

sun_test <- full_join(sun_1_test,sun_2_test)
overcast_test <- full_join(overcast_1_test,overcast_2_test)
final_regression_data_test <- full_join(sun_test,overcast_test)

r2_value_test <- cor(final_regression_data_test$B_Optimal_Power, final_regression_data_test$Regression_Prediction)^2
r2_value_test

Full_model_error_plot <- 
  ggplot(final_regression_data)+geom_line(aes(x=Datetime, y= B_Optimal_Power), col = 'red')+
  geom_line(aes(x=Datetime, y= Regression_Prediction),col = 'blue')
Full_model_error_plot

## Gives the same r^2 value as the training data, would suggest low bias of the model 

Baseline_and_time_model <- summary(lm(B_Optimal_Power ~ 
             Ambient_Temperature+Global_Radiation+Diffuse_Radiation+
             Wind_Velocity+UV+Atmospheric_Pressure+Month+Hour, data = combined_data))

Baseline_model <- summary(lm(B_Optimal_Power ~ 
             Ambient_Temperature+Global_Radiation+Diffuse_Radiation+
             Wind_Velocity+UV+Atmospheric_Pressure, data = combined_data))


## Not a large improvement in R^2 value after segmenting the data but still an improvement.

# Group/Aggregate the data ----------------------------------------------------------

## Aggregating to 5 min intervals 

Five_Min_intervals <- combined_data %>% group_by(Datetime = cut(Datetime, breaks = "5 min")) %>%
  summarise(B_Optimal_Power = mean(B_Optimal_Power), Ambient_Temperature = mean(Ambient_Temperature), 
            Global_Radiation = mean(Global_Radiation),
            Diffuse_Radiation = mean(Diffuse_Radiation), UV = mean(UV), Wind_Velocity = mean(Wind_Velocity),
            Wind_Direction = mean(Wind_Direction), Precipitation = mean(Precipitation), 
            Atmospheric_Pressure = mean(Atmospheric_Pressure), Radiation_ratio = mean(Radiation_ratio), 
            Month= mean(Month),
            count = n())

Ten_Min_intervals <- combined_data %>% group_by(Datetime = cut(Datetime, breaks = "10 min")) %>%
  summarise(B_Optimal_Power = mean(B_Optimal_Power), Ambient_Temperature = mean(Ambient_Temperature), 
            Global_Radiation = mean(Global_Radiation),
            Diffuse_Radiation = mean(Diffuse_Radiation), UV = mean(UV), Wind_Velocity = mean(Wind_Velocity),
            Wind_Direction = mean(Wind_Direction), Precipitation = mean(Precipitation), 
            Atmospheric_Pressure = mean(Atmospheric_Pressure), Radiation_ratio = mean(Radiation_ratio),
            Month= mean(Month),
            count = n())

Thirty_Min_intervals <- combined_data %>% group_by(Datetime = cut(Datetime, breaks = "30 min")) %>%
  summarise(B_Optimal_Power = mean(B_Optimal_Power), Ambient_Temperature = mean(Ambient_Temperature), 
            Global_Radiation = mean(Global_Radiation),
            Diffuse_Radiation = mean(Diffuse_Radiation), UV = mean(UV), Wind_Velocity = mean(Wind_Velocity),
            Wind_Direction = mean(Wind_Direction), Precipitation = mean(Precipitation), 
            Atmospheric_Pressure = mean(Atmospheric_Pressure), Radiation_ratio = mean(Radiation_ratio),
            Month= mean(Month),
            count = n())

Five_Min_intervals$Hour <- hour(Five_Min_intervals$Datetime)
Ten_Min_intervals$Hour <- hour(Ten_Min_intervals$Datetime)
Thirty_Min_intervals$Hour <- hour(Thirty_Min_intervals$Datetime)

# Aggregated five min overcast regression --------------------------------------------

## Repeatting the same steps as taken in the models before but for aggregated data.

regression_tree_five_agg <- tree(B_Optimal_Power ~ Radiation_ratio, data = Five_Min_intervals)
plot(regression_tree_five_agg)
text(regression_tree_five_agg, cex=1)

## Creates more branches than for disaggregated data 

combined_overcast_five_agg <- subset(Five_Min_intervals, Radiation_ratio >= 0.528)

combined_overcast_1_five_min_agg <- subset(combined_overcast_five_agg, Radiation_ratio <= 0.917)
combined_overcast_2_five_min_agg <- subset(combined_overcast_five_agg, Radiation_ratio > 0.917)

full_overcast_model_1_five_agg <- lm(B_Optimal_Power ~ .-Datetime,
                            data = combined_overcast_1_five_min_agg)

step_overcast_model_1_five_agg <- stepAIC(full_overcast_model_1_five_agg, direction = "both", trace=FALSE)
summary(step_overcast_model_1_five_agg)

full_overcast_model_2_five_agg <- lm(B_Optimal_Power ~ .-Datetime,
                            data = combined_overcast_2_five_min_agg)

step_overcast_model_2_five_agg <- stepAIC(full_overcast_model_2_five_agg, direction = "both", trace=FALSE)
summary(step_overcast_model_2_five_agg)

## Now to add predictions to original data

regression_model_overcast_1_five_min <- lm(B_Optimal_Power ~ 
                                    Ambient_Temperature+Diffuse_Radiation+UV+
                                    Wind_Velocity+Atmospheric_Pressure+Radiation_ratio+Hour+Month,
                                  data = combined_overcast_1_five_min_agg)

regression_model_overcast_2_five_min <- lm(B_Optimal_Power ~ 
                                    Ambient_Temperature+Global_Radiation+UV+
                                    Wind_Velocity+Precipitation+Atmospheric_Pressure+Radiation_ratio+Hour,
                                  data = combined_overcast_2_five_min_agg)

# Aggregate five min sunny regression -------------------------------------

## model_2 is worse of the two

combined_sunny_five_agg <- subset(Five_Min_intervals, Radiation_ratio < 0.528)

combined_sunny_1_five_min_agg <- subset(combined_sunny_five_agg, Radiation_ratio <= 0.163)
combined_sunny_2_five_min_agg <- subset(combined_sunny_five_agg, Radiation_ratio > 0.163)

## not going to split into the third level 


full_sunny_model_1_five_agg <- lm(B_Optimal_Power ~ .-Datetime,
                                     data = combined_sunny_1_five_min_agg)

step_sunny_model_1_five_agg <- stepAIC(full_sunny_model_1_five_agg, direction = "both", trace=FALSE)
summary(step_sunny_model_1_five_agg)

full_sunny_model_2_five_agg <- lm(B_Optimal_Power ~ .-Datetime,
                                     data = combined_sunny_2_five_min_agg)

step_sunny_model_2_five_agg <- stepAIC(full_sunny_model_2_five_agg, direction = "both", trace=FALSE)
summary(step_sunny_model_2_five_agg)

## Linear models

regression_model_sunny_1_five_min <- lm(B_Optimal_Power ~ 
                                             Ambient_Temperature+Global_Radiation+Diffuse_Radiation+UV+Precipitation+
                                             Atmospheric_Pressure+Radiation_ratio+Hour+Month,
                                           data = combined_sunny_1_five_min_agg)

regression_model_sunny_2_five_min <- lm(B_Optimal_Power ~ 
                                             Ambient_Temperature+Global_Radiation+Diffuse_Radiation+UV+
                                             Wind_Velocity+Atmospheric_Pressure+Radiation_ratio+Hour+Month,
                                           data = combined_sunny_2_five_min_agg)


# Adding aggregate to overall data ----------------------------------------

sun_1_five <- subset(combined_data, Radiation_ratio <= 0.163)
sun_2_five <- subset(combined_data, Radiation_ratio <=0.528 & Radiation_ratio > 0.163)
overcast_1_five <- subset(combined_data, Radiation_ratio > 0.528 & Radiation_ratio <= 0.917)
overcast_2_five <- subset(combined_data, Radiation_ratio > 0.917)


sun_1 <- add_predictions(sun_1_five, regression_model_sunny_1_five_min, var = "Regression_Prediction_five_min")
sun_2 <- add_predictions(sun_2_five, regression_model_sunny_2_five_min, var = "Regression_Prediction_five_min")
overcast_1 <- add_predictions(overcast_1_five, regression_model_overcast_1_five_min, var = "Regression_Prediction_five_min")
overcast_2 <- add_predictions(overcast_2_five, regression_model_overcast_2_five_min, var = "Regression_Prediction_five_min")

sun <- full_join(sun_1,sun_2)
overcast <- full_join(overcast_1,overcast_2)
final_regression_data <- full_join(sun,overcast)

r2_value_five_min <- cor(final_regression_data$B_Optimal_Power, final_regression_data$Regression_Prediction_five_min)^2
r2_value_five_min

## Higher value than for dissaggregated data.


# Ten minute aggregation overcast -----------------------------------------

regression_tree_ten_agg <- tree(B_Optimal_Power ~ Radiation_ratio, data = Ten_Min_intervals)
plot(regression_tree_ten_agg)
text(regression_tree_ten_agg, cex=1)

## Creates more branches than for disaggregated data 

combined_overcast_ten_agg <- subset(Ten_Min_intervals, Radiation_ratio >= 0.513)

combined_overcast_1_ten_min_agg <- subset(combined_overcast_ten_agg, Radiation_ratio <= 0.882)
combined_overcast_2_ten_min_agg <- subset(combined_overcast_ten_agg, Radiation_ratio > 0.882)

full_overcast_model_1_ten_agg <- lm(B_Optimal_Power ~ .-Datetime,
                                     data = combined_overcast_1_ten_min_agg)

step_overcast_model_1_ten_agg <- stepAIC(full_overcast_model_1_ten_agg, direction = "both", trace=FALSE)
summary(step_overcast_model_1_ten_agg)

full_overcast_model_2_ten_agg <- lm(B_Optimal_Power ~ .-Datetime,
                                     data = combined_overcast_2_ten_min_agg)

step_overcast_model_2_ten_agg <- stepAIC(full_overcast_model_2_ten_agg, direction = "both", trace=FALSE)
summary(step_overcast_model_2_ten_agg)

## Now to add predictions to original data

regression_model_overcast_1_ten_min <- lm(B_Optimal_Power ~ 
                                             Ambient_Temperature+Global_Radiation+Diffuse_Radiation+UV+
                                             Wind_Velocity+Atmospheric_Pressure+Radiation_ratio+Hour+Month,
                                           data = combined_overcast_1_ten_min_agg)

regression_model_overcast_2_ten_min <- lm(B_Optimal_Power ~ 
                                             Ambient_Temperature+Global_Radiation+UV+
                                             Wind_Velocity+Precipitation+Atmospheric_Pressure+Radiation_ratio+Hour,
                                           data = combined_overcast_2_ten_min_agg)

# Ten min aggregation sunny  -------------------------------------

## model_2 is worse of the two

combined_sunny_ten_agg <- subset(Ten_Min_intervals, Radiation_ratio < 0.513)

combined_sunny_1_ten_min_agg <- subset(combined_sunny_ten_agg, Radiation_ratio <= 0.165)
combined_sunny_2_ten_min_agg <- subset(combined_sunny_ten_agg, Radiation_ratio > 0.165)

## not going to split into the third level 

full_sunny_model_1_ten_agg <- lm(B_Optimal_Power ~ .-Datetime,
                                  data = combined_sunny_1_ten_min_agg)

step_sunny_model_1_ten_agg <- stepAIC(full_sunny_model_1_ten_agg, direction = "both", trace=FALSE)
summary(step_sunny_model_1_ten_agg)

full_sunny_model_2_ten_agg <- lm(B_Optimal_Power ~ .-Datetime,
                                  data = combined_sunny_2_ten_min_agg)

step_sunny_model_2_ten_agg <- stepAIC(full_sunny_model_2_ten_agg, direction = "both", trace=FALSE)
summary(step_sunny_model_2_five_agg)

## Linear models

regression_model_sunny_1_ten_min <- lm(B_Optimal_Power ~ 
                                          Ambient_Temperature+Global_Radiation+Diffuse_Radiation+UV+Precipitation+
                                          Atmospheric_Pressure+Radiation_ratio+Hour+Month,
                                        data = combined_sunny_1_ten_min_agg)

regression_model_sunny_2_ten_min <- lm(B_Optimal_Power ~ 
                                          Ambient_Temperature+Global_Radiation+Diffuse_Radiation+UV+
                                          Wind_Velocity+Atmospheric_Pressure+Radiation_ratio+Hour+Month,
                                        data = combined_sunny_2_ten_min_agg)


# Adding aggregate to overall data ----------------------------------------

sun_1_ten <- subset(combined_data, Radiation_ratio <= 0.165)
sun_2_ten <- subset(combined_data, Radiation_ratio <=0.513 & Radiation_ratio > 0.165)
overcast_1_ten <- subset(combined_data, Radiation_ratio > 0.513 & Radiation_ratio <= 0.881)
overcast_2_ten <- subset(combined_data, Radiation_ratio > 0.881)


sun_1 <- add_predictions(sun_1_ten, regression_model_sunny_1_ten_min, var = "Regression_Prediction_ten_min")
sun_2 <- add_predictions(sun_2_ten, regression_model_sunny_2_ten_min, var = "Regression_Prediction_ten_min")
overcast_1 <- add_predictions(overcast_1_ten, regression_model_overcast_1_ten_min, var = "Regression_Prediction_ten_min")
overcast_2 <- add_predictions(overcast_2_ten, regression_model_overcast_2_ten_min, var = "Regression_Prediction_ten_min")

sun <- full_join(sun_1,sun_2)
overcast <- full_join(overcast_1,overcast_2)
final_regression_data <- full_join(sun,overcast)

r2_value_ten_min <- cor(final_regression_data$B_Optimal_Power, final_regression_data$Regression_Prediction_ten_min)^2
r2_value_ten_min



# Thirty min aggregation overcast --------------------------------------------------


regression_tree_thirty_agg <- tree(B_Optimal_Power ~ Radiation_ratio, data = Thirty_Min_intervals)
plot(regression_tree_thirty_agg)
text(regression_tree_thirty_agg, cex=1)

combined_overcast_thirty_agg <- subset(Thirty_Min_intervals, Radiation_ratio >= 0.516)

combined_overcast_1_thirty_min_agg <- subset(combined_overcast_thirty_agg, Radiation_ratio <= 0.897)
combined_overcast_2_thirty_min_agg <- subset(combined_overcast_thirty_agg, Radiation_ratio > 0.897)

full_overcast_model_1_thirty_agg <- lm(B_Optimal_Power ~ .-Datetime,
                                    data = combined_overcast_1_thirty_min_agg)

step_overcast_model_1_thirty_agg <- stepAIC(full_overcast_model_1_thirty_agg, direction = "both", trace=FALSE)
summary(step_overcast_model_1_thirty_agg)

full_overcast_model_2_thirty_agg <- lm(B_Optimal_Power ~ .-Datetime,
                                    data = combined_overcast_2_thirty_min_agg)

step_overcast_model_2_thirty_agg <- stepAIC(full_overcast_model_2_thirty_agg, direction = "both", trace=FALSE)
summary(step_overcast_model_2_thirty_agg)

## Now to add predictions to original data

regression_model_overcast_1_thirty_min <- lm(B_Optimal_Power ~ 
                                            Ambient_Temperature+Global_Radiation+UV+
                                            Wind_Velocity+Atmospheric_Pressure+Radiation_ratio+Hour+Month,
                                          data = combined_overcast_1_thirty_min_agg)

regression_model_overcast_2_thirty_min <- lm(B_Optimal_Power ~ 
                                            Ambient_Temperature+Global_Radiation+UV+
                                            Wind_Velocity+Atmospheric_Pressure+Radiation_ratio+Hour,
                                          data = combined_overcast_2_thirty_min_agg)

# Ten min aggregation sunny  -------------------------------------

## model_2 is worse of the two

combined_sunny_thirty_agg <- subset(Ten_Min_intervals, Radiation_ratio < 0.516)

combined_sunny_1_thirty_min_agg <- subset(combined_sunny_thirty_agg, Radiation_ratio <= 0.163)
combined_sunny_2_thirty_min_agg <- subset(combined_sunny_thirty_agg, Radiation_ratio > 0.163)

## not going to split into the third level 

full_sunny_model_1_thirty_agg <- lm(B_Optimal_Power ~ .-Datetime,
                                 data = combined_sunny_1_thirty_min_agg)

step_sunny_model_1_thirty_agg <- stepAIC(full_sunny_model_1_thirty_agg, direction = "both", trace=FALSE)
summary(step_sunny_model_1_thirty_agg)

full_sunny_model_2_thirty_agg <- lm(B_Optimal_Power ~ .-Datetime,
                                 data = combined_sunny_2_thirty_min_agg)

step_sunny_model_2_thirty_agg <- stepAIC(full_sunny_model_2_thirty_agg, direction = "both", trace=FALSE)
summary(step_sunny_model_2_thirty_agg)

## Aggregation seems to make sunny data easier to predict 

## Linear models

regression_model_sunny_1_thirty_min <- lm(B_Optimal_Power ~ 
                                         Ambient_Temperature+Global_Radiation+Diffuse_Radiation+UV+
                                         Atmospheric_Pressure+Radiation_ratio+Hour+Month,
                                       data = combined_sunny_1_thirty_min_agg)

regression_model_sunny_2_thirty_min <- lm(B_Optimal_Power ~ 
                                         Ambient_Temperature+Global_Radiation+Diffuse_Radiation+UV+
                                         Wind_Velocity+Atmospheric_Pressure+Radiation_ratio+Hour+Month,
                                       data = combined_sunny_2_thirty_min_agg)


# Adding aggregate to overall data ----------------------------------------

sun_1_thirty <- subset(combined_data, Radiation_ratio <= 0.163)
sun_2_thirty <- subset(combined_data, Radiation_ratio <=0.516 & Radiation_ratio > 0.163)
overcast_1_thirty <- subset(combined_data, Radiation_ratio > 0.516 & Radiation_ratio <= 0.897)
overcast_2_thirty <- subset(combined_data, Radiation_ratio > 0.897)


sun_1 <- add_predictions(sun_1_thirty, regression_model_sunny_1_thirty_min, var = "Regression_Prediction_thirty_min")
sun_2 <- add_predictions(sun_2_thirty, regression_model_sunny_2_thirty_min, var = "Regression_Prediction_thirty_min")
overcast_1 <- add_predictions(overcast_1_thirty, regression_model_overcast_1_thirty_min, var = "Regression_Prediction_thirty_min")
overcast_2 <- add_predictions(overcast_2_thirty, regression_model_overcast_2_thirty_min, var = "Regression_Prediction_thirty_min")

sun <- full_join(sun_1,sun_2)
overcast <- full_join(overcast_1,overcast_2)
final_regression_data <- full_join(sun,overcast)

r2_value_thirty_min <- cor(final_regression_data$B_Optimal_Power, final_regression_data$Regression_Prediction_thirty_min)^2
r2_value_thirty_min

# Lasso Regression Overcast --------------------------------------------------------

x_overcast <- as.matrix(combined_overcast[,-7])
y_overcast <- as.matrix(combined_overcast[,7])

lasso_overcast <- glmnet(x_overcast,y_overcast, intercept= FALSE, family = "gaussian",alpha = 1)
cvfit_overcast <- cv.glmnet(x_overcast,y_overcast)
overcast_coef <- coef.cv.glmnet(cvfit_overcast, s="lambda.1se")
overcast_coef

prediction_overcast <- predict(cvfit_overcast,x_overcast,s="lambda.1se")

cor(prediction_overcast, y_overcast)^2

## R^2 value of 0.649

# Lasso Regression Sunny --------------------------------------------------

x_sunny <- as.matrix(combined_sunny[,-7])
y_sunny <- as.matrix(combined_sunny[,7])

lasso_sunny <- glmnet(x_sunny,y_sunny,family = "gaussian",alpha = 1)
cvfit_sunny <- cv.glmnet(x_sunny,y_sunny)
sunny_coef <- coef.cv.glmnet(cvfit_sunny, s="lambda.1se")
sunny_coef
prediction_sunny <- predict(cvfit_sunny,x_sunny,s="lambda.1se")

cor(prediction_sunny, y_sunny)^2

## The r^2 value is 0.753

## Does the lower R^2 value suggest the other model has a higher variance as it has included more parameters.

# Lasso regression further subset -----------------------------------------

x_overcast_1 <- as.matrix(combined_overcast_1[,-7])
y_overcast_1 <- as.matrix(combined_overcast_1[,7])

lasso_overcast_1 <- glmnet(x_overcast_1,y_overcast_1, intercept= FALSE, family = "gaussian",alpha = 1)
cvfit_overcast_1 <- cv.glmnet(x_overcast_1,y_overcast_1)
overcast_coef_1 <- coef.cv.glmnet(cvfit_overcast_1, s="lambda.1se")
overcast_coef_1

prediction_overcast_1 <- predict(cvfit_overcast_1,x_overcast_1,s="lambda.1se")

cor(prediction_overcast_1, y_overcast_1)^2

x_overcast_2 <- as.matrix(combined_overcast_2[,-7])
y_overcast_2 <- as.matrix(combined_overcast_2[,7])

lasso_overcast_2 <- glmnet(x_overcast_2,y_overcast_2, intercept= FALSE, family = "gaussian",alpha = 1)
cvfit_overcast_2 <- cv.glmnet(x_overcast_2,y_overcast_2)
overcast_coef_2 <- coef.cv.glmnet(cvfit_overcast_2, s="lambda.1se")
overcast_coef_2

prediction_overcast_2 <- predict(cvfit_overcast_2,x_overcast_2,s="lambda.1se")

cor(prediction_overcast_2, y_overcast_2)^2
