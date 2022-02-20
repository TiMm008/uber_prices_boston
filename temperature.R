#comparison between "temperature" and "price"
#classify the temperature
uber_weather<-uber_weather%>%
  mutate(n_temp_range=case_when(temp<=20~"<=20",
                                temp>20&temp<=30~"20-30",
                                temp>30&temp<=40~"30-40",
                                temp>40&temp<=50~"40-50",
                                temp>50~">50"))

uber_weather$n_temp_range<-factor(uber_weather$n_temp_range, levels = c("<=20", "20-30", "30-40", "40-50", ">50"))

#summary temperature
summary(uber_weather$temp)

#average
avg_temp<-mean(uber_weather$temp, na.rm = T)
avg_temp

#correlation
ct_temp<-cor.test(uber_weather$price, uber_weather$temp)
ct_temp

#frequency of temperature
fre_temp<-ggplot(uber_weather[!is.na(uber_weather$temp),], aes(x=temp))+
  geom_bar()+
  labs(title="Frequency of Temperature", x="Temperature(°F)", y="Frequency" )
fre_temp

#temperature trend in pick up time
temp_time<-ggplot(uber_weather[!is.na(uber_weather$n_pick_up_time)&!is.na(uber_weather$temp),], aes(x=n_pick_up_time, y=temp))+
  geom_point(aes(color=temp))+
  geom_smooth(color = "red",size = 0.8) +
  scale_colour_gradient2(low="blue", mid = "green", high = "red", midpoint = avg_temp)+
  labs(title="Trend of Temperature", x="", y="Temperature(°F)")
temp_time

#linear regression between price and temperature
p_temp<-ggplot(uber_weather[!is.na(uber_weather$temp)&!is.na(uber_weather$price),], aes(x=temp,y=price))+
  geom_point()+
  geom_smooth(method = "lm", col = "red")+
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),axis.line.y=element_line())+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Linear Regression of Temperature and Price", x="Temperature(°F)", y="Price", fill="Frequency")
p_temp


#frequency of each range of temperature
fre_range_temp<-ggplot(uber_weather[!is.na(uber_weather$n_temp_range),], aes(x=n_temp_range))+
  geom_bar()+
  labs(title="Frequency in temperature Range", x="Temperature(°F)",y="Frequency")
fre_range_temp

#average price in each range of temperature
avg_p_each_temp<-as.data.frame(aggregate(uber_weather$price,by=list(uber_weather$n_temp_range),FUN=mean,na.rm=TRUE))%>%
  mutate(Condition=if_else(x>avg_p, "greater than average price", "less than average price"))
avg_p_each_temp

bar_avg_p_each_temp<-ggplot(avg_p_each_temp, aes(x=Group.1, y=x, fill=Condition))+
  geom_col()+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Title", x="Temperature", y="")
bar_avg_p_each_temp
