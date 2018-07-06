install.packages("TTR")
install.packages("forecast")
install.packages("ggplot2")
install.packages("tseries")
library("tseries")
library("ggplot2")
library("forecast")
library(dplyr)
library("TTR")

##########gg45

data = read.csv("C:/Users/44079266/Desktop/data_latest.csv",header= TRUE)
View(data) 
names(data)

###############################sorting
data$date =  as.Date(as.POSIXct(data$open_date,origin="1970-01-01"))

head(data$date,1)
tail(data$date,1)

temp=data.frame(date=seq(as.Date("2016/11/01"), as.Date("2017/04/30"), by = "day"))

###############################
per_day_total_incident_prioritywise= 
  data %>%
    select(Priority,date) %>%
       group_by(date,Priority) %>%
           summarise(n())

View(per_day_total_incident_prioritywise)

hist(per_day_total_incident_prioritywise$date,per_day_total_incident_prioritywise$`n()`[per_day_total_incident_prioritywise$Priority=='Priority-4'])

###############################

priority1_data = per_day_total_incident_prioritywise %>%
                                filter(Priority=='Priority-1')
?plot
priority1_data = merge(temp, priority1_data, by.x = "date", by.y = "date",all.x = TRUE)
priority1_data$`n()`[is.na(priority1_data$`n()`)]<-0
priority1_data$Priority[is.na(priority1_data$Priority)]<-"Priority-1"

plot(priority1_data$date,priority1_data$`n()`,type='l',xlab="Date",ylab="Count",main="Priority-1 Distribution")
axis(1,at=priority1_data$date[1]:priority1_data$date[181],labels = temp$date)

length(temp$date)
View(priority1_data)

###############################

priority2_data = per_day_total_incident_prioritywise %>%
                                filter(Priority=='Priority-2')

priority2_data = merge(temp, priority2_data, by.x = "date", by.y = "date",all.x = TRUE)
priority2_data$`n()`[is.na(priority2_data$`n()`)]<-0
priority2_data$Priority[is.na(priority2_data$Priority)]<-"Priority-2"
plot(priority2_data$date,priority2_data$`n()`,type='l',xlab="Date",ylab="Count",main="Priority-2 Distribution")

View(priority2_data)

###############################

priority3_data = per_day_total_incident_prioritywise %>%
  filter(Priority=='Priority-3')

priority3_data = merge(temp, priority3_data, by.x = "date", by.y = "date",all.x = TRUE)
priority3_data$`n()`[is.na(priority3_data$`n()`)]<-0
priority3_data$Priority[is.na(priority3_data$Priority)]<-"Priority-3"

plot(priority3_data$date,priority3_data$`n()`,type='l',xlab="Date",ylab="Count",main="Priority-3 Distribution")
View(priority3_data)

###############################

priority4_data = per_day_total_incident_prioritywise %>%
  filter(Priority=='Priority-4')

priority4_data = merge(temp, priority4_data, by.x = "date", by.y = "date",all.x = TRUE)
priority4_data$`n()`[is.na(priority4_data$`n()`)]<-0
priority4_data$Priority[is.na(priority4_data$Priority)]<-"Priority-4"

plot(priority4_data$date,priority4_data$`n()`,type='l',xlab="Date",ylab="Count",main="Priority-4 Distribution")

View(priority4_data)

###############################

priority5_data = per_day_total_incident_prioritywise %>%
  filter(Priority=='Priority-5')

priority5_data = merge(temp, priority5_data, by.x = "date", by.y = "date",all.x = TRUE)
priority5_data$`n()`[is.na(priority5_data$`n()`)]<-0
priority5_data$Priority[is.na(priority5_data$Priority)]<-"Priority-5"

plot(priority5_data$date,priority5_data$`n()`,type='l',xlab="Date",ylab="Count",main="Priority-5 Distribution")

View(priority5_data)

###############################Verification
sum(priority4_data$`n()`)
table(per_day_total_incident_prioritywise$Priority)

names(per_day_total_incident_prioritywise)
per_day_total_incident_prioritywise %>%
  group_by(Priority) %>%
  sum()

###############################
plot(priority4_data$date,priority4_data$`n()`,type='l',col="green",ylim = c(0,120),xlab = "Date",ylab = "No Of Incident")
lines(priority1_data$date,priority1_data$`n()`,col="blue")
lines(priority2_data$date,priority2_data$`n()`,type='l',col="red")
lines(priority3_data$date,priority3_data$`n()`,type='l',col="yellow")
lines(priority5_data$date,priority5_data$`n()`,type='l')

#################################converting to time series data
########doubt___frequency
priority1_tsdata = ts(priority1_data$`n()`,frequency = 52)
priority2_tsdata = ts(priority2_data$`n()`,frequency = 52)
priority3_tsdata = ts(priority3_data$`n()`,frequency = 52)
priority4_tsdata = ts(priority4_data$`n()`,frequency = 52)
priority5_tsdata = ts(priority5_data$`n()`,frequency = 52)
plot(decompose(priority4_tsdata))
##################################Removing outliers
priority2_tsdata=tsclean(priority2_tsdata)
priority3_tsdata=tsclean(priority3_tsdata)
priority4_tsdata=tsclean(priority4_tsdata)
priority5_tsdata=tsclean(priority5_tsdata)

##################################Removing seasonality

plot(log(priority4_data))

priority4_tsdata=priority4_tsdata[TRUE]+1

priority4_tsdata=log(priority4_tsdata)
################################predicting

fit <- auto.arima(priority4_tsdata[1:174])
fore <- forecast(fit, h = 7)
plot(fore)


################################calculating accuracy
test<-priority4_tsdata[175:181]
View(fore$mean)
View(test)
final_values=data.frame('predicted'=exp(1)^fore$mean,'actual'=exp(1)^test)
accuracy(fore,test)

View(data.frame(c('MSE','MAE','MAPE'),c(MSE(test,fore$mean),
MAE(test,fore$mean),
MAPE(test,fore$mean))))



###############################
plot(temp$date[175:181],final_values$predicted,type='l',col="red",ylim = c(0,60),xlab = "Date",ylab = "No Of Incident",lwd=2.5)
lines(temp$date[175:181],final_values$actual,col="blue",lwd=2.5)
legend(temp$date[179],55,c("Predicted","Actual"),
       lty=c(1,1), 
       lwd=c(2.5,2.5),col=c("red","blue")) 
################################ different functions
## mean squared (prediction) error
MSE <- function(y,yhat)
{
  mean((y-yhat)**2)
}
## mean absolute (prediction) error
MAE <- function(y,yhat)
{
  mean(abs(y-yhat))
}
## mean absolute percentage (prediction) error
MAPE <- function(y,yhat,percent=TRUE)
{
  if(percent){
    100*mean(abs( (y-yhat)/y ))
  } else {
    mean(abs( (y-yhat)/y ))
  }
}


##############################
plot(priority5_tsdata)
plot(decompose(priority4_tsdata))
x= stl(priority4_tsdata,s.window = "periodic")
plot(x)

plot.ts(priority4_tsdata)

priority4_data$cleancount = tsclean(priority4_tsdata)

ggplot() +
  geom_line(data = priority4_data, aes(x = date, y = cleancount)) + ylab('Cleaned No of incident')

priority4_data$cleancount7 = ma(priority4_data$cleancount, order=7) # using the clean count with no outliers
priority4_data$cleancount30 = ma(priority4_data$cleancount, order=30)

ggplot() +
  geom_line(data = final_values, aes(x = temp$date[175:181], y = predicted),color='red') +
  geom_line(data = final_values, aes(x = temp$date[175:181], y = actual),color='blue')  +
  ylab('No of incident') + 
  xlab('Date')


plot(priority4_tsdata)
View(fore)

?stl()
?stl()
############################
adf = adf.test(priority4_tsdata)
adf
ndiffs(priority4_tsdata)
log_data = log(priority4_tsdata)
diff_data = diff(log_data,differences = 1)
adf = adf.test(diff_data)
adf

length(diff_data)
plot(log_data)
length
lines(priority4_tsdata,col="red")

fit <- auto.arima(diff_data[1:174])
## forecast for next 2 time points
fore <- forecast(fit, h = 7)
## plot it
plot(fore)
accuracy(fore,test)
test<-diff(test)
MAPE(test,fore$mean)
fore$mean
