data<-read.csv("./course 5 project 1/activity.csv")
library(ggplot2)
library(dplyr)
head(data)
summary(data)
data1<- data%>%
        group_by(date)%>%
        summarise(total = sum(steps,na.rm = T))
hist(data1$total,breaks = seq(0,25000, by = 2500),
        xlab = "Total steps per day",main = "histogram of total steps per day",
        col = "red")

mean1<- mean(data$steps,na.rm = T)
median1<- median(data$steps, na.rm = T)

data2<- data%>%
        group_by(date)%>%
        summarise(mean = mean(steps,na.rm = T),
                median = median(steps))
data2$date<-as.Date(data2$date)

g2<-ggplot(data2, aes(x = date, y = mean))
g2 + geom_line()+
        labs(title = "Mean steps per day",
                y = "Mean steps",
                x = "Date")

data3<- data%>%
        group_by(interval)%>%
        summarise(mean = mean(steps,na.rm = T),
                max = max(steps,na.rm = T))

##imputing missing value
datana<- data %>%
        filter(is.na(steps))
nrow(datana)
datana$steps <- data3$mean[match(datana$interval, data3$interval)]
data$steps[is.na(data$steps)] <- datana$steps

data4<- data%>%
        group_by(date)%>%
        summarise(total = sum(steps,na.rm = T))
hist(data4$total,breaks = seq(0,25000, by = 2500),
        xlab = "Total steps per day",main = "histogram of total steps per day",
        col = "red")

##weekdays and weekend
Sys.setlocale(locale="English_United States") 
data$days <- ifelse(weekdays(as.Date(data$date)) == c("Saturday","Sunday"),"weekends","weekdays")

data5<- data %>%
        group_by(days,interval)%>%
        summarise(mean = mean(steps))

g3<-ggplot(data5, aes(x = interval, y = mean,colour = days))
g3 + geom_line()+
        facet_grid(days~.)+
        theme_grey()+
        ggtitle("Mean Steps per interval of week/weekend")+
        labs(y = "Mean Steps")




