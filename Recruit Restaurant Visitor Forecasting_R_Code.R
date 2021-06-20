library(dplyr)
library(ggplot2)
library(forecast)
library(gridExtra)
library(lubridate)
library(gbm)
library(quantmod)
library(rpart)
library(rpart.plot)
library(randomForest)

air_visit_data <- read.csv('air_visit_data.csv')
air_reserve <- read.csv('air_reserve.csv')
air_store_info <- read.csv('air_store_info.csv') #done
date_info <- read.csv('date_info.csv') # done
sample_submission <- read.csv('sample_submission.csv')
store_id_relation <- read.csv('store_id_relation.csv')

head(air_visit_data)
str(air_visit_data)

summary(air_visit_data)

head(air_reserve)
str(air_reserve)

air_reserve$visit_date <- as.Date(air_reserve$visit_datetime)
air_reserve$reserve_visitors_air <- air_reserve$reserve_visitors
air_reserve$visit_datetime_air <- air_reserve$visit_datetime
air_reserve$reserve_datetime_air <- air_reserve$reserve_datetime
air_reserve$reserve_visitors <-NULL
air_reserve$visit_datetime <-NULL
air_reserve$reserve_datetime <-NULL

str(air_reserve)

air_store_info$latitude <- NULL
air_store_info$longitude <- NULL
head(air_store_info)
str(air_store_info)

head(date_info)
str(date_info)



head(store_id_relation)
str(store_id_relation)

head(sample_submission)
str(sample_submission)

# combining the data frame with common columns
air_visit_data1 <- left_join(air_visit_data, air_store_info, by = c('air_store_id' = 'air_store_id'))
air_visit_data2 <- left_join(air_visit_data1, date_info, by = c('visit_date' = 'calendar_date'))
air_visit_data2 <- mutate(air_visit_data2, holiday_flg = ifelse(holiday_flg == 0, 'No_holiday', 'Holiday'))
air_visit_data2$holiday_flg <- as.factor(air_visit_data2$holiday_flg)
air_visit_data2$visit_date <- as.Date(air_visit_data2$visit_date)
head(air_visit_data2)
str(air_visit_data2)

summary(air_visit_data2)

head(air_reserve)
str(air_reserve)

summary(air_reserve)

air_visit_data3 <- left_join(air_visit_data2, air_reserve, by = c('air_store_id' = 'air_store_id', 'visit_date' = 'visit_date'))

head(air_visit_data3)
str(air_visit_data3)

summary(air_visit_data3)

air_visit_data4 <- na.omit(air_visit_data3)

air_visit_data4$unreserved_visitors <- (air_visit_data4$visitors -(air_visit_data4$reserve_visitors_air))
air_visit_data4$reserved_visitors <- air_visit_data4$visitors - air_visit_data4$unreserved_visitors
air_visit_data4$time_gap_in_hours <- as.numeric(difftime(air_visit_data4$visit_datetime_air,air_visit_data4$reserve_datetime_air), units = 'hours')

breaks <- hour(hm("00:00", "8:00", "2:00", "18:00", "23:59")) # create breaks
labels <- c("Morning", "Afternoon", "Evening", "Night") # labels for the breaks
air_visit_data4$Time_of_day <- cut(x=hour(air_visit_data4$visit_datetime_air), breaks = breaks, labels = labels, include.lowest= TRUE)

air_visit_data4$reserve_datetime_air <- as.POSIXct(air_visit_data4$reserve_datetime_air)
air_visit_data4$visit_month <-  months.Date(air_visit_data4$visit_date)
air_visit_data4$r_month_air <-  months.Date(air_visit_data4$reserve_datetime_air)

air_visit_data4$reserve_visitors_air <- NULL

head(air_visit_data4)
str(air_visit_data4)

air_visit_data4$air_store_id <- as.factor(air_visit_data4$air_store_id)
air_visit_data4$visit_month <- as.factor(air_visit_data4$visit_month)
air_visit_data4$r_month_air <- as.factor(air_visit_data4$r_month_air)

summary(air_visit_data4)

options(repr.plot.width = 13, repr.plot.height = 6)
plot1 <- ggplot(air_visit_data4, aes(x = visit_date, y= visitors)) +
         geom_line(col = 'red') +
         labs( x = 'Visit date', y = 'visitors', title = 'Overall visitors') +
         theme(plot.title = element_text(hjust = 0.5, size = 12),
         axis.title = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 8),
         axis.text = element_text(colour = 'black'))
 
plot2 <- ggplot(air_visit_data4 %>% filter(visit_date <= '2016-07-01'), aes(x = visit_date, y= visitors)) + # before september 20, 2016
         geom_line(col = 'red') +
         labs( x = 'Visit date', y = 'visitors', title = 'Before july 2016') +
         theme(plot.title = element_text(hjust = 0.5, size = 12),
         axis.title = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 8),
         axis.text = element_text(colour = 'black'))

plot3 <- ggplot(air_visit_data4 %>% filter(visit_date > '2016-11-01') , aes(x = visit_date, y= visitors)) + # after september 20, 2016
         geom_line(col = 'red') +
         labs( x = 'Visit date', y = 'visitors', title = 'After november 2016') +
         theme(plot.title = element_text(hjust = 0.5, size = 12),
         axis.title = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 8),
         axis.text = element_text(colour = 'black'))

grid.arrange(plot1, plot2,plot3, nrow = 2, ncol = 2)

options(repr.plot.width = 13, repr.plot.height = 6)
plot1 <- ggplot(air_visit_data4, aes(x = visit_date, y= unreserved_visitors)) +
         geom_line(col = 'red') +
         labs( x = 'Visit date', y = 'Unreserved visitors', title = 'Overall visitors') +
         theme(plot.title = element_text(hjust = 0.5, size = 12),
         axis.title = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 8),
         axis.text = element_text(colour = 'black'))
 
plot2 <- ggplot(air_visit_data4 %>% filter(visit_date <= '2016-07-01'), aes(x = visit_date, y= unreserved_visitors)) + # before september 20, 2016
         geom_line(col = 'red') +
         labs( x = 'Visit date', y = 'Unreserved visitors', title = 'Before july 2016') +
         theme(plot.title = element_text(hjust = 0.5, size = 12),
         axis.title = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 8),
         axis.text = element_text(colour = 'black'))

plot3 <- ggplot(air_visit_data4 %>% filter(visit_date > '2016-11-20') , aes(x = visit_date, y= unreserved_visitors)) + # after september 20, 2016
         geom_line(col = 'red') +
         labs( x = 'Visit date', y = 'Unreserved visitors', title = 'After november 2016') +
         theme(plot.title = element_text(hjust = 0.5, size = 12),
         axis.title = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 8),
         axis.text = element_text(colour = 'black'))

grid.arrange(plot1, plot2,plot3, nrow = 2, ncol = 2)

options(repr.plot.width = 13, repr.plot.height = 6)
plot1 <- ggplot(air_visit_data4, aes(x = visit_date, y= reserved_visitors)) +
         geom_line(col = 'red') +
         labs( x = 'Visit date', y = 'Reserved visitors', title = 'Overall visitors') +
         theme(plot.title = element_text(hjust = 0.5, size = 12),
         axis.title = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 8),
         axis.text = element_text(colour = 'black'))
 
plot2 <- ggplot(air_visit_data4 %>% filter(visit_date <= '2016-07-01'), aes(x = visit_date, y= reserved_visitors)) + # before september 20, 2016
         geom_line(col = 'red') +
         labs( x = 'Visit date', y = 'Reserved visitors', title = 'Before july 2016') +
         theme(plot.title = element_text(hjust = 0.5, size = 12),
         axis.title = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 8),
         axis.text = element_text(colour = 'black'))

plot3 <- ggplot(air_visit_data4 %>% filter(visit_date > '2016-11-20') , aes(x = visit_date, y= reserved_visitors)) + # after september 20, 2016
         geom_line(col = 'red') +
         labs( x = 'Visit date', y = 'Reserved visitors', title = 'After november 2016') +
         theme(plot.title = element_text(hjust = 0.5, size = 12),
         axis.title = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 8),
         axis.text = element_text(colour = 'black'))

grid.arrange(plot1, plot2,plot3, nrow = 2, ncol = 2)

options(repr.plot.width = 8, repr.plot.height = 3)

plot1 <- ggplot(air_visit_data4, aes(reserved_visitors)) +
         geom_histogram(fill = 'steelblue')
plot2 <- ggplot(air_visit_data4, aes(unreserved_visitors)) +
         geom_histogram(fill = 'steelblue')
plot3 <- ggplot(air_visit_data4, aes(visitors)) +
         geom_histogram(fill = 'steelblue')
plot4 <- ggplot(air_visit_data4, aes(time_gap_in_hours)) +
         geom_histogram(fill = 'steelblue')

grid.arrange(plot1, plot2,plot3,plot4, nrow = 2, ncol = 2)

options(repr.plot.width = 9, repr.plot.height = 3)
g3 <- air_visit_data4 %>% group_by(Month = months.Date(visit_date))
ggplot(g3, aes(reorder(Month,Month,function(x)-length(x)))) +
geom_bar(fill = 'steelblue') +
labs( x = 'Month', y = 'Number of stores', title = 'Stores Distribution') +
theme(plot.title = element_text(hjust = 0.5, size = 12),
axis.title = element_text(size = 12),
axis.text.x = element_text(angle = 90, size = 10),
axis.text.y = element_text(size = 8),
panel.grid = element_blank(),
panel.background = element_blank(),
axis.text = element_text(colour = 'black')) +
facet_wrap(~holiday_flg)

options(repr.plot.width = 7, repr.plot.height = 3)
g1 <- air_visit_data4 %>% group_by(visit_date)
ggplot(g1, aes(reorder(Time_of_day,Time_of_day,function(x)-length(x)))) +
geom_bar(fill = 'steelblue') +
labs( x = 'Time of day', y = 'Number of stores', title = 'Stores Distribution') +
theme(plot.title = element_text(hjust = 0.5, size = 12),
axis.title = element_text(size = 12),
axis.text.x = element_text(angle = 90, size = 10),
axis.text.y = element_text(size = 8),
panel.grid = element_blank(),
panel.background = element_blank(),
axis.text = element_text(colour = 'black')) +
facet_wrap(~holiday_flg)

options(repr.plot.width = 9, repr.plot.height = 3)

plot1 <- ggplot(air_visit_data4 %>% group_by(air_genre_name) %>% summarise(Average_visitors = round(mean(visitors))) , 
                aes(air_genre_name, Average_visitors)) +
         geom_bar(stat = "identity",fill = 'steelblue') +
         coord_flip()

plot2 <- ggplot(air_visit_data4 %>% group_by(air_genre_name) %>% summarise(Total_visitors = sum(visitors)), aes(air_genre_name, Total_visitors)) +
         geom_bar(stat = "identity",fill = 'steelblue') +
         scale_y_continuous(labels = scales::comma) +
         coord_flip() 

grid.arrange(plot1, plot2, nrow = 1, ncol = 2)

options(repr.plot.width = 9, repr.plot.height = 3)

plot1 <- ggplot(air_visit_data4, aes(r_month_air)) +
         geom_bar(fill = 'steelblue') +
         coord_flip()
plot2 <- ggplot(air_visit_data4, aes(visit_month)) +
         geom_bar(fill = 'steelblue') +
         scale_y_continuous(labels = scales::comma) +
         coord_flip() 

grid.arrange(plot1, plot2, nrow = 1, ncol = 2)

options(repr.plot.width = 11, repr.plot.height = 6)

plot1 <- ggplot(air_visit_data4 %>% group_by(visit_date) %>% summarise(Total_visitors = sum(visitors)),aes(visit_date, Total_visitors)) +
         geom_line(col = 'red')

plot3 <- ggplot(air_visit_data4 %>% group_by(visit_date) %>% summarise(Total_reserved = sum(reserved_visitors)),aes(visit_date, Total_reserved)) +
         geom_line(col = 'red') +
         coord_cartesian(ylim = c(0,80000))
plot2 <- ggplot(air_visit_data4 %>% group_by(visit_date) %>% summarise(Total_unreserved = sum(unreserved_visitors)),aes(visit_date, Total_unreserved)) +
         geom_line(col = 'red') +
         coord_cartesian(ylim = c(0,80000)) 

grid.arrange(plot1, plot2, plot3, nrow = 2, ncol = 2)

options(repr.plot.width = 10, repr.plot.height = 3.5)

plot1 <- ggplot(air_visit_data4 %>% group_by(day_of_week) %>% summarise(Tot_res_visitors = sum(reserved_visitors)), 
                aes(day_of_week, Tot_res_visitors)) +
         geom_bar(stat = 'identity' ,fill = 'steelblue') 
plot2 <- ggplot(air_visit_data4 %>% group_by(day_of_week) %>% summarise(Tot_visitors = sum(visitors)), 
                aes(day_of_week, Tot_visitors)) +
         geom_bar(stat = 'identity' ,fill = 'steelblue') +
         scale_y_continuous(labels = scales::comma) 

grid.arrange(plot1, plot2,nrow = 1, ncol = 2)

sort(table(air_visit_data4$air_store_id), decreasing = TRUE)

head(air_visit_data4)

str(air_visit_data4)

sort(table(air_visit_data4$air_genre_name), decreasing = TRUE)[1:5]



# since there is a random fluctuation after july to november, we are going to use the data after september 2016 for forecasting
# we are going to forecast the total visitors and average visitors count.
avg_visitors_data <- air_visit_data4 %>% filter(visit_date > '2016-11-01') %>% group_by(visit_date) %>% 
         summarise(avg_visitors = round(mean(visitors)))

train1 <- avg_visitors_data %>% filter(visit_date <= '2017-03-01')
test1 <- avg_visitors_data %>% filter(visit_date > '2017-03-01')

str(test)

arima_model1 <- arima(train1$avg_visitors, order=c(0,0,1), seasonal= list(order=c(1,0,0), period = 7)) # average visitors

library(tseries)


Diff <- diff(train1$avg_visitors, lag = 7)

plot.ts(Diff)

adf.test(Diff)

diff2 <- diff(train1$avg_visitors, lag = 1)
plot.ts(diff2)

adf.test(diff2)

acf(diff2)

pacf(diff2)

arima.fit <- auto.arima(tsclean(ts(train1$avg_visitors, frequency = 7)),
                        stepwise = FALSE, approximation = FALSE)

AIC(arima.fit)

summary(arima.fit)

Box.test(arima.fit$residuals, type = "Ljung")

plot(arima.fit$residuals)

AIC(arima.fit)

par(mfrow=c(2,1))

plot(forecast(arima.fit, h = 21), main = "auto.arima")

forecast(arima.fit, h = 21)

forecast

plot(forecast(arima_model1, h = 21))

accuracy(arima_model1)

ses_model1 <- ets(train1$avg_visitors)

ses_model1

fcast_ses_model1 <- forecast(ses_model1, h = 4)

summary(accuracy(fcast_ses_model1))

plot(fcast_ses_model1)

accuracy(fcast_ses_model1)

summary(ses_model1)



accuracy(ses_model1)

hw2 <- HoltWinters(tsclean(ts(train1$avg_visitors, frequency = 7)))

pred1 <- predict(hw2, n.ahead = 50, prediction.interval = T, level = 0.95)

plot(hw2, pred1)

sse2 <- hw2$SSE
mse2 <- sse2/nrow(train1)
rmse2 <- sqrt(mse2)
cat('Root mean squared error is ', rmse2)

#average visitors
ts <- xts(avg_visitors_data[,2], order.by = avg_visitors_data$visit_date) # time series
target <- dailyReturn(ts)
predictor <- lag(target, k = 1)
avg_data <- cbind(target,predictor)
colnames(avg_data) <- c('target', 'predictor')
avg_data <- na.exclude(avg_data)
head(avg_data,3)
str(avg_data)

#average visitors
train_data <- window(avg_data, end = '2017-03-01')
test_data <- window(avg_data, start = '2017-03-02')

head(train_data,3)
head(test_data,3)

avg_d_tree <- rpart(target ~ predictor, train_data) #average visitors model

#avg visitors
options(repr.plot.width = 7, repr.plot.height = 4.5)
rpart.plot(avg_d_tree,extra = 1)

summary(avg_d_tree)

# prediction
avg_test_pred = predict(avg_d_tree, test_data)
sse = sum((avg_test_pred - test_data$target)^2)
mse = sse/nrow(train_data)
rmse = sqrt(mse)
cat('Root mean squared error is',round(rmse,3))

rf_avg_model <- randomForest(target ~ predictor, train_data, ntree = 500, nodesize = 2, replace = T) #average visitors model

rf_avg_model$predicted[1:10] # top 10 predicted value from train data

options(repr.plot.width = 5, repr.plot.height = 4.5)
plot1 <- plot(rf_avg_model)

rf_avg_model$mse[1:10] # mean squared error of first ten trees

# average visitors
avg_test_pred1 = predict(rf_avg_model, test_data)
sse5 = sum((avg_test_pred1 - test_data$target)^2)
mse5 = sse5/nrow(train_data)
rmse5 = sqrt(mse5)
cat('Root mean squared error is', round(rmse5,3))


