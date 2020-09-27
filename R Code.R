################################################################################
## Analysis of Continental Temperature changes and Crop Yield
##    for the period 1961-2018/9
##
## Script takes in 2 csv files for temperature and crop yield
##    Creates some figures and forecasts
##      for submission to the CGIAR 28/09/2019 submission contest
################################################################################

#Cleaning Rstudio environment
rm(list = ls())


# Loading needed packages -------------------------------------------------
#loading packages
library(tidyverse)
library(GGally)
library(forecast)
library(timetk)
library(sweep)
library(tidyquant)

# Loading data ------------------------------------------------------------

#Loading csv data
temp_data <- read.csv(file.choose())
crop_data <- read.csv(file.choose())

#Observing the structure of variables
View(temp_data)
View(crop_data)
str(temp_data)
glimpse(temp_data)


#Selecting necessary information from the data
temp_data1 <- temp_data %>%
  filter(Element == "Temperature change") %>%
  select(2, 4, 8:66)

crop_data1 <- crop_data %>%
  filter(Element == "Yield") %>%
  select(2, 8:65)

#Restructuring the data for the ease of plotting and analysis
temp_df <- temp_data1 %>%
  pivot_longer(., cols = c(3:61), names_to = "Years",
               values_to = "Temperature Change")

crop_df <- crop_data1 %>%
  pivot_longer(., cols = c(2:59), names_to = "Years",
               values_to = "Crop Yield")


#Labelling correctly and estrating the Year
temp_df1 <- temp_df %>%
  mutate(Year = str_sub(temp_df$Years, 2)) %>%
  mutate(temp_df, Decade = ifelse(Year %in% 1961:1970, "1961-1970",
                                  ifelse(Year %in% 1971:1980, "1971-1980",
                                         ifelse(Year %in% 1981:1990, "1981-1990",
                                                ifelse(Year %in% 1991:2000, "1991-2000",
                                                       ifelse(Year %in% 2001:2010, "2001-2010",
                                                              ifelse(Year %in% 2011-2019, "2011-2019")))))))

cdf1 <- crop_df %>%
  mutate(Year = str_sub(crop_df$Years, 2)) 


# Grouping data by variables ----------------------------------------------

#Grouping Continental temperature changes by year
temp_grp_data <- temp_df1 %>%
  filter(Area %in% c("Africa", "Asia", "Americas", "Europe", "Oceania")) %>%
  group_by(Area, Year) %>%
  summarise(Average_Temp = mean(`Temperature Change`))

temp_grp_data

crop_grp_data <- cdf1 %>%
  filter(Area %in% c("Africa", "Asia", "Americas", "Europe", "Oceania")) %>%
  group_by(Area, Year) %>%
  summarise(Crop_yield = sum(`Crop Yield`))

#Quarters in a year
quart <- c("Dec-Jan-Feb", "Jun-Jul-Aug", "Mar-Apr-May", "Sep-Oct-Nov")


#Grouping by quarters for continents
temp_quart <- temp_df1 %>%
  filter(Area %in% c("Africa", "Asia", "Americas", "Europe", "Oceania")) %>%
  filter(Months %in% quart) %>%
  #filter(Months %in% c("Dec-Jan-Feb", "Jun-Jul-Aug", "Mar-Apr-May", "Sep-Oct-Nov")) %>%
  group_by(Area, Months, Year) %>%
  summarise(Mean = mean(`Temperature Change`))




# merge two data frames by Area and Year
total <- merge(temp_grp_data, crop_grp_data,by=c("Area","Year"))
View(total)


# Data plotting -----------------------------------------------------------

#Temperature changes by country
t <- ggplot(temp_grp_data, aes(x=Year, y=Average_Temp)) + geom_point(aes(color=Area)) +
  ggtitle("Yearly Temperature changes by Continent from 1961-2019")

#Due to the huge number of yes, its neater to leave the x axis as blank
t + theme(axis.text.x = element_blank())

ggsave(filename = "Yearly Temperature changes from 1961-2019", plot = t,
       device = "png")

#Plotting Crop yield
g <- ggplot(crop_grp_data, aes(x=Year, y=Crop_yield)) + geom_point(aes(color=Area)) +
  ggtitle("Yearly Crop Yield by Continent from 1961-2019") +
  ylab("Crop Yield Hg/Ha")

#Due to the huge number of yes, its neater to leave the x axis as blank
g + theme(axis.text.x = element_blank())

ggsave(filename = "Yearly crop yield by continent from 1961-2019", plot = g,
       device = "png")

#Box plots Continent
ggplot(temp_quart, aes(x=Area, y=Mean)) + 
  geom_boxplot(aes(fill=Months)) + 
  xlab("Continent") + ylab("Average Temperage change") +
  geom_jitter() + ggtitle("Continental Quarter Changes")

q #Europe has spaced quarterly changes. Some areas are warmer whilst some are not

ggsave(filename= "Continental Quartely temp changes.png", plot=q, device="png")

# Correlation by group ----------------------------------------------------

total %>%
  group_by(Area) %>%
  summarise(COR = cor(Average_Temp, Crop_yield))

ggpairs(total, columns = 3:4, ggplot2::aes(color = Area))

#The low European correlation of -0.289 could be due to the lack of normality?

# Forecasting yearly Crop yield given temperature changes -------------------

# Forecasting temperature change for the next 10 years --------------------


#Using Nesting function to consolidate each time series

total$Year <- as.Date(total$Year, format = "%Y")

total_temp <- total %>%
  select(Area, Year, Average_Temp)

total_nest_temp <- total_temp %>%
  group_by(Area) %>%
  nest()

total_nest_temp

#Coercing a time series object class 


total_ts_temp <- total_nest_temp %>%
  mutate(data.ts = map(.x = data,
                       .f = tk_ts,
                       select = -Year,
                       start = 1961))

total_ts_temp

#Fitting Exponential Smoothing ETS (Error, Trend, Seasonal) model function
total_fit_temp <- total_ts_temp %>%
  mutate(fit.ets = map(data.ts, ets))

#Model Parameter
total_fit_temp %>%
  mutate(tidy = map(fit.ets, sw_tidy)) %>%
  unnest(tidy) %>%
  spread(key = Area, value = estimate)

#Model accuracies
total_fit_temp %>%
  mutate(glance = map(fit.ets, sw_glance)) %>%
  unnest(glance)

#Residual and fitted values
aug_fit_temp <- total_fit_temp %>%
  mutate(augment = map(fit.ets, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(augment)

aug_fit_temp

#Plotting residuals against fitted
aug_fit_temp %>%
  ggplot(aes(x = date, y = .resid, group = Area)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_line(color = palette_light()[[2]]) +
  geom_smooth(method = "loess") +
  labs(title = "Temperature Change by Continent",
       subtitle = "ETS Model Residuals", x = "") + 
  theme_tq() +
  facet_wrap(~ Area, scale = "free_y", ncol = 3) +
  scale_x_date(date_labels = "%Y")

#Model decomposition
total_fit_temp %>%
  mutate(decomp = map(fit.ets, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(decomp)

#Forecasting for the next 10 years
total_temp_forecast <- total_fit_temp %>%
  mutate(fcast.ets = map(fit.ets, forecast, h = 10))

total_temp_forecast

total_temp_tidy <- total_temp_forecast %>%
  mutate(sweep = map(fcast.ets, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
  unnest(sweep)

#Tidy the forecast
total_temp_tidy %>%
  ggplot(aes(x = index, y = Average_Temp, color = key, group = Area)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line() +
  labs(title = "Temperature changes by Continent",
       subtitle = "ETS Model Forecasts",
       x = "Year", y = "Crop Yield HG/Ha") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap(~ Area, scales = "free_y", ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_blank())


# Forecasting crop yield for the next 10 years ----------------------------

total_crop <- total %>%
  select(Area, Year, Crop_yield)

total_nest_crop <- total_crop %>%
  group_by(Area) %>%
  nest()

total_nest_crop

#Coercing a time series object class 


total_ts_crop <- total_nest_crop %>%
  mutate(data.ts = map(.x = data,
                       .f = tk_ts,
                       select = -Year,
                       start = 1961))

total_ts_crop

#Fitting Exponential Smoothing ETS (Error, Trend, Seasonal) model function
total_fit_crop <- total_ts_crop %>%
  mutate(fit.ets = map(data.ts, ets))

#Model Parameter
total_fit_crop %>%
  mutate(tidy = map(fit.ets, sw_tidy)) %>%
  unnest(tidy) %>%
  spread(key = Area, value = estimate)

#Model accuracies
total_fit_crop %>%
  mutate(glance = map(fit.ets, sw_glance)) %>%
  unnest(glance)

#Residual and fitted values
aug_fit_crop <- total_fit_crop %>%
  mutate(augment = map(fit.ets, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(augment)

aug_fit_crop

#Plotting residuals against fitted
aug_fit_crop %>%
  ggplot(aes(x = date, y = .resid, group = Area)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_line(color = palette_light()[[2]]) +
  geom_smooth(method = "loess") +
  labs(title = "Crop Yield by Continent",
       subtitle = "ETS Model Residuals", x = "") + 
  theme_tq() +
  facet_wrap(~ Area, scale = "free_y", ncol = 3) +
  scale_x_date(date_labels = "%Y")

#Model decomposition
total_fit_crop %>%
  mutate(decomp = map(fit.ets, sw_tidy_decomp, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(decomp)

#Forecasting for the next 10 years
total_crop_forecast <- total_fit_crop %>%
  mutate(fcast.ets = map(fit.ets, forecast, h = 10))

total_crop_forecast

total_fcast_tidy <- total_crop_forecast %>%
  mutate(sweep = map(fcast.ets, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
  unnest(sweep)

#Tidy the forecast
total_fcast_tidy %>%
  ggplot(aes(x = index, y = Crop_yield, color = key, group = Area)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line() +
  labs(title = "Crop Yield by Continent",
       subtitle = "ETS Model Forecasts",
       x = "Year", y = "Crop Yield HG/Ha") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  facet_wrap(~ Area, scales = "free_y", ncol = 3) +
  theme_tq() +
  theme(axis.text.x = element_blank())

sessionInfo()
