test <- read.csv("test (1).csv")
train <- read.csv("train.csv")
library(tidyverse)
library(DataExplorer)
library(ggplot2)
library(RColorBrewer)
library(prophet)
library(beepr)
library(data.table)

#Explore data
head(train)
str(train$date)

#Merge data 
store <- bind_rows(train=train, test=test, .id="Set")
store <- store[,1:5]

store$date <- as.Date(store$date, "%Y-%m-%d")

plot_missing(store)

#Create month and year variabel
store <- store %>%
  mutate(month = format(date, "%m"), year = format(date, "%Y"))

store$wday <- weekdays(store$date)

season <- function(x){
  if ((x > '0' & x < '3') | x == '12') {
    szn <- "Winter"
  }
  else if ((x > '2' & x < '6')) {
    szn <- "Spring"
  }
  else if ((x > '5' & x < '9')) {
    szn <- "Summer"
  }
  else {
    szn <- "Fall"
  }
  return(szn)
}

for (i in 1:length(store)) {
  store$season[i] <- season(store$month[i])
}

#Look at growth by date
avg_sale <- aggregate(sales~date, store, mean)

ggplot(avg_sale, aes(x=as.factor(date), y=sales))+
  geom_line(color="blue", aes(group=1), size=1.5)+
  geom_point(colour="blue", size = 3.5, alpha=0.5)+
  labs(title="The Growth of Sale Prices by date", x=NULL, y="Sale Price")+
  theme( plot.title=element_text(vjust=3, size=15) ) + theme_minimal()

#Try and work with prophet library
#Split up to work on just one item of the data
#Take log1p of sales

practice_item_1 <- train %>%
  filter(store == 1) %>%
  filter(item == 1) %>%
  mutate(y = log1p(sales)) %>%
  select(date,y)

colnames(practice_item_1) <- c("ds","y")

model <- prophet(practice_item_1)#, changepoint.prior.scale = 0.0001,
                 #holidays.prior.scale = 0.01)
#summary(model)

future_item_1 <- make_future_dataframe(model, periods = 90)
forecast_item_1 <- predict(model, future_item_1)

plot(model, forecast_item_1) + add_changepoints_to_plot(model)
prophet_plot_components(model, forecast_item_1)

#Try something with holidays

###########
super_bowls <- practice_item_1 %>%
  filter(y > 3.65)
playoff <- practice_item_1 %>%
  filter(y < 3.65 & y > 3.58)

playoffs <- tibble(
  holiday = 'playoff',
  ds = as.Date(playoff$ds),
  lower_window = 0,
  upper_window = 0
)
superbowls <- tibble(
  holiday = 'superbowl',
  ds = as.Date(c('2013-06-09', '2015-06-06', '2015-07-04', '2015-07-18', '2015-07-26',
                 '2015-09-19', '2016-05-15', '2016-06-03', '2016-07-01', '2016-07-30',
                 '2017-06-10', '2017-06-18', '2017-06-28', '2017-07-02', '2017-07-14',
                 '2017-08-26', '2017-09-01 ')),
  lower_window = 0,
  upper_window = 0
)
holidays <- bind_rows(playoffs, superbowls)

#########
hol_mod <- prophet()
hol_mod <- add_seasonality(hol_mod, name = 'daily', period = 60, fourier.order = 5)
#hol_mod <- add_country_holidays(hol_mod, country_name = 'US')
hol_mod <- prophet(practice_item_1, holidays = holidays, holidays.prior.scale = 0.5,
                   yearly.seasonality = 4, interval.width = 0.95, 
                   changepoint.prior.scale = 0.0001, daily.seasonality = T)

hol_future_item_1 <- make_future_dataframe(m, periods = 90, freq = 'days')
hol_forecast_item_1 <- predict(hol_mod, hol_future_item_1)

plot(hol_mod, hol_forecast_item_1) + add_changepoints_to_plot(model)
prophet_plot_components(hol_mod, hol_forecast_item_1)

error2 <- smape_fun(practice_item_1$y, 
                    hol_forecast_item_1$yhat[1:length(practice_item_1$y)])
mean(error2)


#Make a function to calculate smape
#Dont forget to exponentiate
smape_fun <- function(real_vals, forecasts){
  real_vals <- (as.numeric(real_vals))
  forecasts<- expm1(as.numeric(forecasts))
  smape <- (abs(real_vals-forecasts))/((abs(real_vals)+abs(forecasts))/2)
  return(smape)
}

error2 <- smape_fun(practice_item_1$y, m$yhat[1:length(practice_item_1$y)])
mean(error2)

head(forecast_item_1$yhat)
library(caret)
set.seed(3456)


#Try to tune with cross validation
library(caret)

hyper_grid <- expand.grid(
  changepoint.prior.scale = c(0.0001, 0.001, 0.01, 0.1, 0.5),
  holidays.prior.scale  = c(0.01,0.1,0.5,0.75),
  fourier.order = c(1,5,10),
  Smape   = 0
)

for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- prophet()
  model <- add_seasonality(model, name = 'daily', period = 60, 
                           fourier.order = hyper_grid$fourier.order[i])
  model <- prophet(practice_item_1, holidays = holidays, 
                   holidays.prior.scale = hyper_grid$holidays.prior.scale[i],
                     yearly.seasonality = 4, interval.width = 0.95, 
                     changepoint.prior.scale = hyper_grid$changepoint.prior.scale[i]
                   , daily.seasonality = T)
  future <- make_future_dataframe(model, periods = 90, freq = 'days')
  forecast <- predict(model, future)
  
  error <- smape_fun(practice_item_1$y, 
                      forecast$yhat[1:length(practice_item_1$y)])
  avg_smape <- mean(error)
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(avg_smape)
}
beep(27)
#min smape 1.18663

#Optimized parameters
opp_mod <- prophet()
opp_mod <- add_seasonality(opp_mod, name = 'daily', period = 60, fourier.order = 5)
#hol_mod <- add_country_holidays(hol_mod, country_name = 'US')
opp_mod <- prophet(practice_item_1, holidays = holidays, holidays.prior.scale = 0.01,
                   yearly.seasonality = 4, interval.width = 0.95, 
                   changepoint.prior.scale = 0.0001, daily.seasonality = T)

opp_future_item_1 <- make_future_dataframe(opp_mod, periods = 90, freq = 'days')
opp_forecast_item_1 <- predict(opp_mod, opp_future_item_1)

plot(opp_mod, opp_forecast_item_1) + add_changepoints_to_plot(model)
prophet_plot_components(opp_mod, opp_forecast_item_1)

opp_error <- smape_fun(practice_item_1$y, 
                    opp_forecast_item_1$yhat[1:length(practice_item_1$y)])
mean(opp_error)


#trainIndex <- createTimeSlices(train$date, initialWindow = 5)
#head(trainIndex)

#Look at cross validation metrics
df.cv <- cross_validation(opp_mod, initial = 730, period = 90, horizon = 365, units = 'days')

df.p <- performance_metrics(df.cv)
head(df.p)

plot_cross_validation_metric(df.cv, metric = 'mape')


#Start fitting to all of the data
train$Year=NULL
train$Month=NULL
train$sales=log1p(train$sales)
head(train)

colnames(train) <- c("ds","store","item","y")
train <- as.data.table(train)
train_splitting <- split(train, by=c("store", "item"), keep.by=FALSE)
class(train_splitting)

prediction <- function(df) {
  super_bowls <- df %>%
    filter(y > 3.65)
  playoff <- df %>%
    filter(y < 3.65 & y > 3.58)
  
  playoffs <- tibble(
    holiday = 'playoff',
    ds = as.Date(playoff$ds),
    lower_window = 0,
    upper_window = 0
  )
  superbowls <- tibble(
    holiday = 'superbowl',
    ds = as.Date(c('2013-06-09', '2015-06-06', '2015-07-04', '2015-07-18', '2015-07-26',
                   '2015-09-19', '2016-05-15', '2016-06-03', '2016-07-01', '2016-07-30',
                   '2017-06-10', '2017-06-18', '2017-06-28', '2017-07-02', '2017-07-14',
                   '2017-08-26', '2017-09-01 ')),
    lower_window = 0,
    upper_window = 0
  )
  holidays <- bind_rows(playoffs, superbowls)
  
  
  opp_mod <- prophet()
  opp_mod <- add_seasonality(opp_mod, name = 'daily', period = 60, fourier.order = 5)
  opp_mod <- prophet(df, holidays = holidays, holidays.prior.scale = 0.01,
                     yearly.seasonality = 4, interval.width = 0.95, 
                     changepoint.prior.scale = 0.0001, daily.seasonality = T)
  
  opp_future_item_1 <- make_future_dataframe(opp_mod, periods = 90, freq = 'days')
  opp_forecast_item_1 <- predict(opp_mod, opp_future_item_1)
  
  forecast_final <- xts::last(opp_forecast_item_1[, c("ds","yhat")],90)
  return(forecast_final)
}

prediction_final <- as.data.frame(sapply(train_splitting[c(1,2)],prediction))

library(reshape)
dim(prediction_final)
md <- melt(prediction_final)
dim(md)
colnames(md)<-c("store","date","sales")
md$sales=expm1(md$sales)
head(md)

#sub <- read.csv("sample_submission.csv")
sub$sales <- md$sales
head(sub)
fwrite(sub,"sub_prophet_v1.csv")
