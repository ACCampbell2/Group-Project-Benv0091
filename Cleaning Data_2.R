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

## Also exclude atmospheric pressure and precipitation due to numerous NA values

meteo_2017_wo <- subset(meteo_2017, select = -c(Precipitation, Atmospheric_Pressure))
meteo_2016_wo<- subset(meteo_2016, select = -c(Precipitation, Atmospheric_Pressure))


total_meteo=full_join(meteo_2016,meteo_2017)

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

## 01-01-2014 was a cloudy day especially morining till 9 and after 3 next day also cloudy apart from 12-3/5
## Also works for 2014-04-15

## Could generalize saying any value of 0.9 is either morning or an overacast day?

combined_data <- left_join(total_pvsunlab,total_meteo,by="Datetime")
combined_data <- arrange(combined_data,Datetime)

combined_data <- combined_data[!is.na(combined_data$B_Optimal_Power), ]
## Drop missing optimal power
combined_data <- drop_na(combined_data)
## drop  rest of the na's



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

full_regression_tree <- tree(B_Optimal_Power ~., data = combined_data[,c(19,26:33)])
plot(full_regression_tree)
text(full_regression_tree, cex=.5)

summary(full_regression_tree)

combined_overcast <- subset(combined_data, Radiation_ratio >0.37 && Radiation_ratio < 1)
combiend_sunny <- subset(combined_data, Radiation_ratio <= 0.37)

x_overcast <- as.matrix(combined_overcast[,c(26:31,35:37)])
y_overcast <- as.matrix(combined_overcast[,19])

x_sunny <- as.matrix(combined_sunny[,c(26:31,35:37)])
y_sunny <- as.matrix(combined_sunny[,19])

## Could possibly split these down into two sub groups again as suggested by the regression tree

# Stepwise regression -----------------------------------------------------

## Remove the other pvsunlab values from dataset

cropped_sunny <- combined_sunny[,c(1,19,26:33,35:37)]

full_sunny_model <- lm(B_Optimal_Power ~ .-Datetime,
                       data = cropped_sunny)

step_sunny_model <- stepAIC(full_sunny_model, direction = "both", trace=FALSE)
summary(step_sunny_model)

## R^2 of 0.6424 including temp, global, diffuse and UV radiation, Wind Velocity and Atmo Pressure. Indluing dates massively increases R^2 to 0.75

cropped_overcast <- combined_overcast[,c(1,19,26:33, 35:37)]

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

ggplot(reg)


# Another subset ----------------------------------------------------------

combined_overcast_1 <- subset(combined_overcast, Radiation_ratio < 0.871)
combined_overcast_2 <- subset(combined_overcast, Radiation_ratio >= 0.871)

cropped_overcast_1 <- combined_overcast_1[,c(1,19,26:33, 35:37)]

full_overcast_model_1 <- lm(B_Optimal_Power ~ .-Datetime-Overcast_Prediction,
                          data = cropped_overcast_1)

step_overcast_model_1 <- stepAIC(full_overcast_model_1, direction = "both", trace=FALSE)
summary(step_overcast_model_1)

cropped_overcast_2 <- combined_overcast_2[,c(1,19,26:33, 35:37)]

full_overcast_model_2 <- lm(B_Optimal_Power ~ .-Datetime-Overcast_Prediction,
                            data = cropped_overcast_2)

step_overcast_model_2 <- stepAIC(full_overcast_model_2, direction = "both", trace=FALSE)
summary(step_overcast_model_2)


## model_2 is worse of the two

combined_sunny_1 <- subset(combined_sunny, Radiation_ratio < 0.163)
combined_sunny_2 <- subset(combined_sunny, Radiation_ratio >= 0.163)

cropped_sunny_1 <- combined_sunny_1[,c(1,19,26:33,35:37)]

full_sunny_model_1 <- lm(B_Optimal_Power ~ .-Datetime-Sunny_Prediction,
                       data = cropped_sunny_1)

step_sunny_model_1 <- stepAIC(full_sunny_model_1, direction = "both", trace=FALSE)
summary(step_sunny_model_1)

cropped_sunny_2 <- combined_sunny_2[,c(1,19,26:33,35:37)]

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
reg_overcast$Reg

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
  geom_line(aes(x=Datetime, y=Regression_Prediction_2), col = 'red')

# Ridge Regression Overcast --------------------------------------------------------

ridge_overcast <- glmnet(x_overcast,y_overcast, intercept= FALSE, family = "gaussian",alpha = 0)
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

r2 <- ridge_overcast$glmnet.fit$dev.ratio[which(fitnet$glmnet.fit$lambda == fitnet$lambda.1se)] 

ggplot(combined_overcast[1:40000,])+geom_line(aes(x=Datetime, y=B_Optimal_Power),col='red')+
  geom_line(aes(x=Datetime, y= Overcast_Prediction), col='blue')

difference <- prediction - y_overcast

plot(combined_overcast$Datetime,prediction, type = "l")

data.f <- data.frame(combined_overcast$Datetime,prediction,y_overcast)
ggplot(data.f,aes(combined_overcast$Datetime))+ geom_line(aes(y=difference),colour = "red")

summary(difference)

ggplot(meteo_2016)+geom_line(aes(x=Datetime,y=Ambient_Temperature))
## Seems to over predict by a constant value and under predicts more often than over.


# Ridge Regression Sunny --------------------------------------------------



ridge_sunny <- glmnet(x_sunny,y_sunny,family = "gaussian",alpha = 0)
print(ridge_sunny)

cvfit_sunny <- cv.glmnet(x_sunny,y_sunny)
plot(cvfit_sunny)

cvfit_sunny$lambda.1se

coef.cv.glmnet(cvfit_sunny, s="lambda.1se")
prediction_sunny <- predict(cvfit_sunny,x_sunny,s="lambda.1se")

difference_sunny <- prediction_sunny - y_sunny


data.f_sunny<- data.frame(combined_sunny$Datetime,prediction_sunny,y_sunny)
ggplot(data.f_sunny,aes(combined_sunny$Datetime))+ geom_line(aes(y=difference_sunny),colour = "red")

summary(difference_sunny)

combined_sunny <- data.frame(combined_sunny,prediction_sunny)
names(combined_sunny)[37] <- "Sunny_Prediction"

ggplot(combined_sunny[1:40000,])+geom_line(aes(x=Datetime, y=B_Optimal_Power),col='red')+
  geom_line(aes(x=Datetime, y= Sunny_Prediction), col='blue')

## Under predicts for jan to march but over predicts in june 




# Web Scrapping  ----------------------------------------------------------

## To get information of cloudy days

url <- 'https://sunsetsunrisetime.com/portugal/faro_16730.html'

faro_webpage <- read_html(url)

## Can't see /faro-weather-history/ or /faro/ or /pt.aspx in robots.txt file so assume it is ok to scrape this page

sunrise_data <- html_nodes(faro_webpage, "table")
head(sunrise_data)

h <- html_nodes(faro_webpage, ".period-link")
head(h)

sunrise_Table <- faro_webpage %>% 
  html_nodes("#data_table_1") %>% html_table(fill=TRUE) %>% .[[1]]

sunrise_table <- html_table(faro_webpage, 'data_table_1', fill=TRUE)

API_key <- '7065813102924e24939231735190901'
 
sunrise <- url %>%
    html() %>%
    read_html(xpath='//*[@id="data_table_1"]') %>%
   html_table(fill=TRUE)

head(population)
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








