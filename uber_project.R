getwd()
setwd("C:/Users/omri_/Downloads/uber_train")
library(geosphere)
library(lubridate)
library(ggplot2)

#setting things up: filtering by location and creating interval counts
{
#loading data
apr <- read.csv("uber-raw-data-apr14.csv")
may <- read.csv("uber-raw-data-may14.csv")
jun <- read.csv("uber-raw-data-jun14.csv")
jul <- read.csv("uber-raw-data-jul14.csv")
aug <- read.csv("uber-raw-data-aug14.csv")
sep <- read.csv("uber-raw-data-sep14-first-2-weeks.csv")
View(apr)

#formatting time data
apr$Date.Time1 <- as.POSIXct(apr$Date.Time,format = "%m/%d/%Y %H:%M:%S")
may$Date.Time1 <- as.POSIXct(may$Date.Time,format = "%m/%d/%Y %H:%M:%S")
jun$Date.Time1 <- as.POSIXct(jun$Date.Time,format = "%m/%d/%Y %H:%M:%S")
jul$Date.Time1 <- as.POSIXct(jul$Date.Time,format = "%m/%d/%Y %H:%M:%S")
aug$Date.Time1 <- as.POSIXct(aug$Date.Time,format = "%m/%d/%Y %H:%M:%S")
sep$Date.Time1 <- as.POSIXct(sep$Date.Time,format = "%Y-%m-%d %H:%M:%S")

#merging data into single dataframe
mergedv1 <- c(apr$Date.Time1,may$Date.Time1,jun$Date.Time1,jul$Date.Time1,aug$Date.Time1,sep$Date.Time1)
mergedv2 <- c(apr$Lat,may$Lat,jun$Lat,jul$Lat,aug$Lat,sep$Lat)
mergedv3 <- c(apr$Lon,may$Lon,jun$Lon,jul$Lon,aug$Lon,sep$Lon)
mergedv4 <- factor(c(as.character(apr$Base),as.character(may$Base),as.character(jun$Base),as.character(jul$Base),as.character(aug$Base),as.character(sep$Base)),levels = levels(apr$Base))
m <- data.frame(mergedv1,mergedv2,mergedv3,mergedv4)
names(m) <- names(apr)[1:4]
View(m)

#creating location subset
nyselat <-  40.706913
nyselon <- -74.011322
nysedist <- numeric(length(m$Lat))
for (i in 1:length(m$Lat))
{
  nysedist[i] <- distHaversine(c(nyselon,nyselat),c(m$Lon[i],m$Lat[i]))
}
m1 <- m[which((nysedist<1000)),1:4]

#creating time intervals
summary(m1$Date.Time)
startdate <- as.POSIXct("01/04/2014 0:00:00",format = "%d/%m/%Y %H:%M:%S")
enddate <- as.POSIXct("16/09/2014 23:45:00",format = "%d/%m/%Y %H:%M:%S")
t_intervals <- seq.POSIXt(from = startdate,to = enddate, by = (15*60))

#counting rides per interval
sorted_m <- sort(m1$Date.Time)
t_counts <- numeric(length(t_intervals))
j <- 1
for(i in 1:(length(t_intervals)-1))
{
  go_next <- F
  while(!go_next)
  {
    if(sorted_m[j] %within% int_diff(c(t_intervals[i],t_intervals[i+1])))
    {
      t_counts[i] <- t_counts[i] + 1
      if(j<length(sorted_m)) j <- j+1
    }
    else
    {
      go_next <- T
    }
  }
}
t_counts[16224] <- sum(sorted_m>t_intervals[16224]) #last interval, didn't run in loop
sum(t_counts) #checking that t_counts sums up to overall ride number
train_df <- data.frame(t_intervals,t_counts)
View(train_df)
}

#importing and rearranging weather description data
{
ny_weather<- read.csv("weather_description1.csv")
ny_weather$datetime <- as.POSIXct(ny_weather$datetime,format = "%d/%m/%Y %H:%M")
#data is for GMT timezone, therefore:
adj_startdate <- as.POSIXct("01/04/2014 04:00:00",format = "%d/%m/%Y %H:%M:%S") 
adj_enddate <- as.POSIXct("17/09/2014 03:45:00",format = "%d/%m/%Y %H:%M:%S")
nyw_within <-  ny_weather[which(ny_weather$datetime %within% int_diff(c(adj_startdate,adj_enddate))),1:2]
wea_description <- character(length(t_counts))
for(i in 1:length(nyw_within$datetime))
{
  wea_description[(1:4 +(4*(i-1)))] <- as.character(nyw_within$New.York[i])
}
wea_description <- factor(wea_description)#,levels = levels(nyw_within$New.York))
train_df$wea_description <- wea_description
View(train_df)
}

#importing and rearranging the temperature data
{
ny_temp<- read.csv("temperature1.csv")
ny_temp$datetime <- as.POSIXct(ny_temp$datetime,format = "%d/%m/%Y %H:%M")
#data is for GMT timezone, therefore:
adj_startdate <- as.POSIXct("01/04/2014 04:00:00",format = "%d/%m/%Y %H:%M:%S") #GMT
adj_enddate <- as.POSIXct("17/09/2014 03:45:00",format = "%d/%m/%Y %H:%M:%S")
nyw_within <-  ny_temp[which(ny_temp$datetime %within% int_diff(c(adj_startdate,adj_enddate))),1:2]
nyc_temp <- numeric(length(t_counts))
for(i in 1:length(nyw_within$datetime))
{
  nyc_temp[(1:4 +(4*(i-1)))] <- nyw_within$New.York[i]
}
summary(nyc_temp)
train_df$nyc_temp <- nyc_temp
#creating linear interpolation of temperature
{
  l_interpolation_temp <- numeric(length(train_df$nyc_temp))
  for(i in 1:4056)
  {
    l_interpolation_temp[4*(i-1)+1] <-  train_df$nyc_temp[4*(i-1)+1]
    l_interpolation_temp[4*(i-1)+2] <- 0.75*train_df$nyc_temp[4*(i-1)+1] + 0.25*train_df$nyc_temp[4*(i)+1]
    l_interpolation_temp[4*(i-1)+3] <- 0.5*train_df$nyc_temp[4*(i-1)+1] + 0.5*train_df$nyc_temp[4*(i)+1]
    l_interpolation_temp[4*(i-1)+4] <- 0.25*train_df$nyc_temp[4*(i-1)+1] + 0.75*train_df$nyc_temp[4*(i)+1]
  }
  l_interpolation_temp[which(is.na(l_interpolation_temp))] <- train_df$nyc_temp[which(is.na(l_interpolation_temp))]
  l_interpolation_temp[1:16]
  train_df$nyc_temp[1:16]
  train_df$l_interpolation_temp <- l_interpolation_temp
  train_df$repeated_temp <- train_df$nyc_temp
  train_df$nyc_temp <- train_df$l_interpolation_temp
}
View(train_df)

}

#adding weekdays and time of day
{
  train_df$weekday <- factor(weekdays.POSIXt(train_df$t_intervals),levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  train_df$hour24 <- hour(train_df$t_intervals)
  train_df$hour24 <- factor(train_df$hour24,levels = as.character(0:23))
  train_df$hour96 <- hour(train_df$t_intervals)*4 +(minute(train_df$t_intervals)%/%15)
  train_df$hour96 <- factor(train_df$hour96,levels = as.character(0:95))
  train_df$month <- factor(as.character(month(train_df$t_intervals,label = T)),levels = c("Apr","May","Jun","Jul","Aug","Sep"))
}

#adding events
{
events <- read.csv("events.csv")
events <- events[1:(length(events[,2])-1),]
colnames(events) <- c("date","event")
events$date <- as.POSIXct(events$date,format = "%d/%m/%Y")
enddate_with_test <- as.POSIXct("30/09/2014 23:45:00",format = "%d/%m/%Y %H:%M:%S")
t_intervals <- seq.POSIXt(from = startdate,to = enddate_with_test, by = (15*60))
events_y_n <- rep(NA,(length(events[,2])*96))
for(i in 1:length(events[,2]))
{
  events_y_n[(1:96 +(96*(i-1)))] <- events[i,2]
}

events_df <- data.frame(t_intervals,events_y_n)
View(events_df)
train_df <- merge(x = train_df, y = events_df, by = "t_intervals", all.x = TRUE)

}

#adding holidays
{
holidays <- read.csv("holidays.csv")
holidays <- holidays[1:(length(events[,2])),]
View(holidays)
colnames(holidays) <- c("date","holiday")
holidays$date <- as.POSIXct(holidays$date,format = "%d/%m/%Y")
holidays_bucket <- data.frame(c(rep(NA,length(holidays[,1])*96)),c(rep(NA,length(holidays[,1])*96)))
colnames(holidays_bucket) <- c("date","holiday")

enddate_with_test <- as.POSIXct("30/09/2014 23:45:00",format = "%d/%m/%Y %H:%M:%S")
t_intervals <- seq.POSIXt(from = startdate,to = enddate_with_test, by = (15*60))
holidays_y_n <- rep(NA,(length(events[,2])*96))
for(i in 1:length(holidays[,2]))
{
  holidays_y_n[(1:96 +(96*(i-1)))] <- holidays[i,2]
}

holidays_df <- data.frame(t_intervals,holidays_y_n)
View(holidays_df)
holidays_df$holidays_y_n[8929:8997] <- 0
train_df <- merge(x = train_df, y = holidays_df, by = "t_intervals", all.x = TRUE)
View(train_df)
}

#importing and rearranging historical weather data
{
library(readr)
hist_avg <- read_table2("hist_avg.txt", col_names = as.character(1:31))
hist_high <- read_table2("hist_high.txt", col_names = as.character(1:31))
hist_low <- read_table2("hist_low.txt", col_names = as.character(1:31))
w_vec_times <- seq.POSIXt(from = as.POSIXct("01/04/2014",format = "%d/%m/%Y"),to = as.POSIXct("30/09/2014 23:45:00",format = "%d/%m/%Y %H:%M:%S"), by = 15*60)
tot_avg <- numeric(183)
tot_high <- numeric(183)
tot_low <- numeric(183)
n_index <- 1
for(i in 1:6)
{
  for(j in 1:31)
  {
    if(j==31)
    {
      if(i %in% c(2,4,5))
      {
        tot_avg[n_index] <- as.numeric(hist_avg[i,j])
        tot_high[n_index] <- as.numeric(hist_high[i,j])
        tot_low[n_index] <- as.numeric(hist_low[i,j])
        
        n_index <- n_index+1
      }
    }
    else
    {
      tot_avg[n_index] <- as.numeric(hist_avg[i,j])
      tot_high[n_index] <- as.numeric(hist_high[i,j])
      tot_low[n_index] <- as.numeric(hist_low[i,j])
      
      n_index <- n_index+1
    }
  }
} #putting vaules in tot_ vectors

tot_avg <- (tot_avg-32)*5/9
tot_high <- (tot_high-32)*5/9
tot_low <- (tot_low-32)*5/9

tot_avg_intervals<- numeric(length(w_vec_times))
tot_high_intervals <- numeric(length(w_vec_times))
tot_low_intervals <- numeric(length(w_vec_times))
for(i in 1:length(tot_avg))
{
  tot_avg_intervals[(1:96 +(96*(i-1)))] <- tot_avg[i]
  tot_high_intervals[(1:96 +(96*(i-1)))] <- tot_high[i]
  tot_low_intervals[(1:96 +(96*(i-1)))] <- tot_low[i]
}
hist_temp_df <- data.frame(w_vec_times,tot_avg_intervals,tot_low_intervals,tot_high_intervals)
hist_train_df <- hist_temp_df[1:16224,]
hist_test_df <- hist_temp_df[16897:17568,]
train_df$hist_temp_avg <- hist_train_df$tot_avg_intervals
train_df$hist_temp_low <- hist_train_df$tot_low_intervals
train_df$hist_temp_high <- hist_train_df$tot_high_intervals

names(hist_test_df)[1:4] <- names(train_df)[c(1,9:11)]
View(hist_temp_df)

}

pred_R_square <- function(predvec,actualvec)
{
  1-sum((predvec-actualvec)^2)/sum((mean(actualvec)-actualvec)^2)
}

#temp predictions
{
#
split_indeces <- sample(1:16224,round(16224*0.3))
temp_train_df <- train_df[-split_indeces,]
temp_validate_df <- train_df[split_indeces,]
  
#building model
temp_check_model <- lm(nyc_temp ~ hour24 + month + hist_temp_avg,data = temp_train_df)
summary(temp_check_model)
temp_validate_df$temp_pred <- predict(temp_check_model,temp_validate_df)
pred_R_square(temp_validate_df$temp_pred,temp_validate_df$nyc_temp) 

#checking model
qqnorm(temp_check_model$residuals/summary(temp_check_model)$sigma)
abline(a=0,b=1,col="red",lwd=2)

#building test explainatory variables
hist_test_df$month <- train_df$month[16000]
hist_test_df$hour24 <- hour(hist_test_df$w_vec_times)
hist_test_df$hour24 <- factor(hist_test_df$hour24,levels = as.character(0:23))

#final test predictions
temp_pred_model <- lm(nyc_temp ~ hour24 + month + hist_temp_avg,data = train_df)
summary(temp_pred_model)
hist_test_df$temp_pred <- predict(temp_pred_model,hist_test_df)

#linear interpolation of preds
l_i_pred <- numeric(length(hist_test_df$temp_pred))
for(i in 1:168)
{
  l_i_pred[4*(i-1)+1] <-  hist_test_df$temp_pred[4*(i-1)+1]
  l_i_pred[4*(i-1)+2] <- 0.75*hist_test_df$temp_pred[4*(i-1)+1] + 0.25*hist_test_df$temp_pred[4*(i)+1]
  l_i_pred[4*(i-1)+3] <- 0.5*hist_test_df$temp_pred[4*(i-1)+1] + 0.5*hist_test_df$temp_pred[4*(i)+1]
  l_i_pred[4*(i-1)+4] <- 0.25*hist_test_df$temp_pred[4*(i-1)+1] + 0.75*hist_test_df$temp_pred[4*(i)+1]
}
l_i_pred[which(is.na(l_i_pred))] <- hist_test_df$temp_pred[which(is.na(l_i_pred))]
hist_test_df$l_i_pred <- l_i_pred
#making final predictions using the weights
hist_test_df$final_temp_pred <- hist_test_df$l_i_pred
View(hist_test_df)
}

#creating training and validation sets
{
  separation_index <- round(16224*0.7)
  uber_train_df <- train_df[1:separation_index,]
  uber_validate_df <- train_df[(separation_index+1):16224,]
}

#linear model
{
#training set has 24 low-temp samples, validation set has none
train_temp_nozero <- uber_train_df$nyc_temp
train_temp_nozero[which(train_temp_nozero<0.5)] <- 0.5
#temp powers
uber_train_df$nyc_temp_p2 <- train_temp_nozero^2
uber_train_df$nyc_temp_p3 <- uber_train_df$nyc_temp^3
uber_train_df$nyc_temp_p4 <- train_temp_nozero^4
uber_train_df$nyc_temp_p5 <- uber_train_df$nyc_temp^5
uber_train_df$nyc_temp_sqrt <-sqrt(train_temp_nozero)
uber_train_df$nyc_temp_log <-log(train_temp_nozero)

#linear interpolation of weather predictions
uber_validate_df$temp_pred <- predict(temp_pred_model,uber_validate_df)
l_pred <- numeric(length(uber_validate_df$temp_pred))
for(i in 1:1215)
{
  l_pred[4*(i)] <-  uber_validate_df$temp_pred[4*(i-1)+1]
  l_pred[4*(i)+1] <- 0.75*uber_validate_df$temp_pred[4*(i)] + 0.25*uber_validate_df$temp_pred[4*(i+1)]
  l_pred[4*(i)+2] <- 0.5*uber_validate_df$temp_pred[4*(i)] + 0.5*uber_validate_df$temp_pred[4*(i+1)]
  l_pred[4*(i)+3] <- 0.25*uber_validate_df$temp_pred[4*(i)] + 0.75*uber_validate_df$temp_pred[4*(i+1)]
}
l_pred[which(l_pred==0)] <- uber_validate_df$temp_pred[which(l_pred==0)]
summary(l_pred)
uber_validate_df$temp_pred <- l_pred

#changing terms to make predictions on predicted temperatures
uber_validate_df$real_temp <- uber_validate_df$nyc_temp
1-sum((uber_validate_df$temp_pred-uber_validate_df$real_temp)^2)/sum((mean(uber_validate_df$real_temp)-uber_validate_df$real_temp)^2)
uber_validate_df$nyc_temp <- uber_validate_df$temp_pred
uber_validate_df$nyc_temp_p2 <- uber_validate_df$temp_pred^2
uber_validate_df$nyc_temp_p3 <- uber_validate_df$temp_pred^3
uber_validate_df$nyc_temp_p4 <- uber_validate_df$temp_pred^4
uber_validate_df$nyc_temp_p5 <- uber_validate_df$temp_pred^5
uber_validate_df$nyc_temp_sqrt <-sqrt(uber_validate_df$temp_pred)
uber_validate_df$nyc_temp_log <-log(uber_validate_df$temp_pred)

#building model
lm1 <- (lm(t_counts~  nyc_temp + nyc_temp_p2 + nyc_temp_p3 + nyc_temp_p4 + hour24 + as.numeric(month) + weekday + weekday*hour24 + events_y_n*hour24 + holidays_y_n*hour24,data =uber_train_df ))
summary(lm1)
uber_validate_df$lm_pred <- (predict(lm1,uber_validate_df))
1-sum((uber_validate_df$lm_pred-uber_validate_df$t_counts)^2)/sum((mean(uber_validate_df$t_counts)-uber_validate_df$t_counts)^2)

#using boxcox regression to determine whether a transformation would be beneficial
boxcox(lm(t_counts+(1)~ nyc_temp + nyc_temp_p2 + nyc_temp_p3 + nyc_temp_p4 + hour24 + as.numeric(month) + weekday + weekday*hour24 + events_y_n*hour24 + holidays_y_n*hour24,data =uber_train_df ))
axis(1,seq(-2,2,0.1),as.character(seq(-2,2,0.1)))
lambda <- 0.5 #we chose 0.5 mainly because it's the value that maximized R^2 in both sets
lm2 <- (lm(t_counts^(lambda)~ nyc_temp + nyc_temp_p2 + nyc_temp_p3 + nyc_temp_p4 + hour24 + as.numeric(month) + weekday + weekday*hour24 + events_y_n*hour24 + holidays_y_n*hour24,data =uber_train_df ))
summary(lm2)
uber_validate_df$lm2_pred <- (predict(lm2,uber_validate_df)^(1/lambda))
1-sum((uber_validate_df$lm2_pred-uber_validate_df$t_counts)^2)/sum((mean(uber_validate_df$t_counts)-uber_validate_df$t_counts)^2)

qqnorm(lm1$residuals/summary(lm1)$sigma)
abline(a=0,b=1,col="red",lwd=2)
qqnorm(lm2$residuals/summary(lm2)$sigma)
abline(a=0,b=1,col="red",lwd=2)
plot(lm1$fitted.values,lm1$residuals)
plot(lm2$fitted.values,lm2$residuals)

View(uber_validate_df)

plot(density(uber_validate_df$lm_pred,bw=1),lwd=2,col="green")
lines(density(uber_validate_df$lm2_pred,bw=1),lwd=2,col="red")
lines(density(uber_validate_df$t_counts,bw=1),lwd=2,col="black")
lines(density(rf_pred,bw=1),lwd=2,col="blue")

#determining model limitations
within_hour_means <- numeric(16224)
for(i in 1:4056)
{
  within_hour_means[1:4 + 4*(i-1)] <-  mean(train_df$t_counts[1:4 + 4*(i-1)])
}
#estimate of the upper limit for a model's R^2 when making exactly the same predictions for each of the quarter-hour intervals of an hour  
sum((within_hour_means-mean(t_counts))^2)/sum((t_counts-mean(t_counts))^2) 
}

#random forest model
{
  #install.packages("randomForest")
  library(randomForest)
  uber_train_df$num_month <- as.numeric(uber_train_df$month)+3
  uber_validate_df$num_month <- as.numeric(uber_validate_df$month)+3
  rf1 <- (randomForest(t_counts~ t_intervals + nyc_temp + hour24 + weekday + num_month + events_y_n + holidays_y_n,data =uber_train_df, mtry=5, ntree=600,nodesize=10))
  uber_validate_df$rf_pred <- predict(rf1,uber_validate_df)
  pred_R_square(rf_pred,uber_validate_df$t_counts)
}

#graphing
{
  library("ggiraph")
  library("ggiraphExtra")
  
  #exploratory analysis plots
  {
    nyc_temp_plot <-   ggplot(train_df, aes(nyc_temp))+   
      geom_density(aes(y = ..density..))+
      labs(title="Distribution Of Temperatures",x="temperature")
    nyc_temp_plot
    
    
    temp_count_plot <-  ggplot(train_df, aes(x=nyc_temp, y=t_counts))+
      geom_point(colour = "darkblue", alpha = 1/10)+
      labs(title="Uber Counts VS Temperatures",x="temperature", y="uber counts")+
      geom_smooth(method='lm',formula=y~x, colour = 2)
    temp_count_plot
    summary(lm(t_counts~nyc_temp, data = train_df))
    
    holidays_count_plot <-  ggplot(train_df, aes(x=as.factor(holidays_y_n), y=t_counts))+
      geom_boxplot(colour = "darkblue")+
      labs(title="Uber Counts in holidays",x="holiday indicator", y="uber counts")
    holidays_count_plot
    
    
    events_count_plot <-  ggplot(train_df, aes(x=as.factor(events_y_n), y=t_counts))+
      geom_boxplot(colour = "darkblue")+
      labs(title="Uber Counts in event day",x="event indicator", y="uber counts")
    events_count_plot
    
    events_eve_count_plot <-  ggplot(train_df[which(train_df$hour24 %in% c("17","18","19","20","21","22")),], aes(x=as.factor(events_y_n), y=t_counts))+
      geom_boxplot(colour = "darkblue")+
      labs(title="Uber Counts (at the evening) in event day",x="event indicator", y="uber counts")
    events_eve_count_plot
    
    weekday_count_plot <-  ggplot(train_df, aes(x=as.factor(weekday), y=t_counts))+
      geom_boxplot(colour = "darkblue")+
      labs(title="Uber Counts per weekday",x="weekday", y="uber counts")
    weekday_count_plot
    
    
    month_count_plot <-  ggplot(train_df, aes(x=as.factor(month), y=t_counts))+
      geom_boxplot(colour = "darkblue")+
      labs(title="Uber Counts per month",x="month", y="uber counts")
    month_count_plot
    
    
    hour_count_plot <-  ggplot(train_df, aes(x=hour24, y=t_counts))+
      geom_point(colour = "darkblue", alpha = 1/10)+
      stat_summary(aes(y = t_counts,group=1), fun.y=mean, colour="red", geom="line",group=1,size = 2)+
      labs(title="Uber Counts per hour",x="hour", y="uber counts")
    hour_count_plot
    
    
    #plot of mean count per hour in working days and weekends
    
    #####################
    e <- which(as.numeric(train_df$weekday) %in% c(2:6))
    y1 <- by(train_df$t_count[e],train_df$hour24[e], mean)
    y2 <- by(train_df$t_count[-e],train_df$hour24[-e], mean)
    u <- sort(unique(train_df$hour24))
    y1 <- rbind(y1)
    y2 <- rbind(y2)
    a <- rbind(u,y1,y2)
    a <- t(a)
    a <- data.frame(a)
    a$u <- a$u-1
    a
    names(a) <- c("hour","mean working day","mean weekend")
    a <-  melt(a, id.vars="hour")
    
    count_hour_workingday_weekend <- ggplot(a, aes(hour,value, col=variable)) + 
      geom_line(size = 2) +
      labs(title="Uber Counts per hour",x="hour", y="uber counts")+
      scale_x_continuous(breaks=seq(0, 23, 2))
    count_hour_workingday_weekend
  } 
    
  #creating day indeces
  {
    uber_validate_df$day <- day(uber_validate_df$t_intervals)
    day_index <- numeric(length(uber_validate_df$day))
    j <- 1
    current_Day <- uber_validate_df$day[1]
    day_index[1] <- j
    for(i in 2:length(day_index))
    {
      if(uber_validate_df$day[i]!=current_Day)
      {
        current_Day <- uber_validate_df$day[i]
        j <- j+1
      }
      day_index[i] <- j
    }
    summary(day_index)
    uber_validate_df$day_index <- day_index
    
  }
  
  #daily count
  {
  real_day_count <- as.vector(by(uber_validate_df$t_counts,day_index,sum))
  pred_day_count_lm2 <- as.vector(by(uber_validate_df$lm2_pred,day_index,sum))
  pred_day_count_rf <- as.vector(by(uber_validate_df$rf_pred,day_index,sum))
  plot(1:51,real_day_count,cex=1.2,pch=16, xaxt='n',xlab = "day",ylab = "daily ride count",main = "Model performance by daily ride count",panel.first = {
  lines(1:51,pred_day_count_lm2,col="red",lwd=3)
  lines(1:51,pred_day_count_rf,col="blue",lwd=3)
  axis(1,at=seq(1,50,7),labels = c("28/07","04/08","11/08","18/08","25/08","01/09","08/09","15/09"))
  })
  }
  
  #quarter_hour view (random weekday)
  {
  random_day_index <- sample(c(seq(2,51,7),seq(3,45,7),seq(7,49,7),seq(8,50,7)),1)
  plot(1:96,uber_validate_df$t_counts[which(uber_validate_df$day_index==random_day_index)],xaxt='n',pch=16,xlab = "hour of day",ylab = "uber ride count",main = "Random weekday Uber count by quarter hour",
       panel.first = {
         lines(1:96,uber_validate_df$lm2_pred[which(uber_validate_df$day_index==random_day_index)],col="red",lwd=2)
         lines(1:96,uber_validate_df$rf_pred[which(uber_validate_df$day_index==random_day_index)],col="blue",lwd=2)
         axis(1,at=seq(0,96,4),labels = as.character(0:24))
       })
  }
  
  #quarter_hour view (random weekend day)
  {
    random_day_index <- sample(c(seq(5,47,7),seq(6,48,7)),1)
    plot(1:96,uber_validate_df$t_counts[which(uber_validate_df$day_index==random_day_index)],xaxt='n',pch=16,xlab = "hour of day",ylab = "uber ride count",main = "Random weekend Uber count by quarter hour",
         panel.first = {
           lines(1:96,uber_validate_df$lm2_pred[which(uber_validate_df$day_index==random_day_index)],col="red",lwd=2)
           lines(1:96,uber_validate_df$rf_pred[which(uber_validate_df$day_index==random_day_index)],col="blue",lwd=2)
           axis(1,at=seq(0,96,4),labels = as.character(0:24))
         })
  }
  
  #temperature predictions over real values
  {
    temp_pred_plot <-  ggplot(temp_validate_df, aes(x=t_intervals))+
      geom_point(aes(y=nyc_temp, colour = "real temp"))+
      labs(title="Temp VS Time",x="time", y="temp")+
      geom_point(aes(y=temp_pred, colour = "predicted temp"))+
      scale_colour_manual(values=c("darkblue", "red"))
    temp_pred_plot
  }

}

#test set predictions
{
  uber_test_df <- read.csv("uber_test_empty.csv")
  View(uber_test_df)
  uber_test_df$t_intervals <-  as.POSIXct(uber_test_df$Time_Interval,format = "%Y-%m-%d %H:%M:%S")
  #adding weekdays, hour and month
  uber_test_df$weekday <- factor(weekdays.POSIXt(uber_test_df$t_intervals),levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
  uber_test_df$hour24 <- hour(uber_test_df$t_intervals)
  uber_test_df$hour24 <- factor(uber_test_df$hour24,levels = as.character(0:23))
  uber_test_df$month <- factor(as.character(month(uber_test_df$t_intervals,label = T)),levels = c("Apr","May","Jun","Jul","Aug","Sep"))
  #adding holidays and events
  startindex_event <- which(events_df$t_intervals==as.POSIXct("24/09/2014 00:00:00",format = "%d/%m/%Y %H:%M:%S"))
  endindex_event <- which(events_df$t_intervals==as.POSIXct("30/09/2014 23:45:00",format = "%d/%m/%Y %H:%M:%S"))
  uber_test_df$events_y_n <- events_df$events_y_n[startindex_event:endindex_event]
  uber_test_df$holidays_y_n <- holidays_df$holidays_y_n[startindex_event:endindex_event]
  #adding the predicted temperatures
  uber_test_df$nyc_temp <- hist_test_df$final_temp_pred
  uber_test_df$nyc_temp_p2 <- uber_test_df$nyc_temp^2
  uber_test_df$nyc_temp_p3 <- uber_test_df$nyc_temp^3
  uber_test_df$nyc_temp_p4 <- uber_test_df$nyc_temp^4
  #extending the final model to the whole train df
  lambda <- 0.5
  final_lm <- (lm(t_counts^(lambda)~ nyc_temp + nyc_temp_p2 + nyc_temp_p3 + nyc_temp_p4 + hour24 + month + weekday + weekday*hour24 + events_y_n*hour24 + holidays_y_n*hour24,data =train_df ))
  summary(final_lm)
  uber_test_df$number_of_pickups <- (predict(final_lm,uber_test_df)^(1/lambda))
  summary(uber_test_df$number_of_pickups)
  #making 2-column table for submition
  final_df <- uber_test_df[,c(1,12)]
  View(final_df)
  write.csv(final_df,'uber_test.csv',row.names = F)
}