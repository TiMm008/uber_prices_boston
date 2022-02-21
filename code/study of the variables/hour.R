#summary rush hour
summary(uber_weather$traffic)

#correlation
ct_traffic<-cor.test(uber_weather$price, uber_weather$traffic)
ct_traffic

#frequency of hour
uber_weather$n_hour<-factor(uber_weather$n_hour, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9","10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))

fre_hour<-ggplot(uber_weather, aes(x=as.character(n_hour)))+
  geom_bar()+
  labs(title="Frequency of Hour", x="Hour", y="Frequency")
fre_hour

#price in each hour
box_p_hour<-ggplot(uber_weather, aes(x=n_hour, y=price))+
  geom_boxplot()+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Boxplot Hour According to Price", x="Hour", y="Price")
box_p_hour

#price/mile in each hour
box_pm_hour<-ggplot(uber_weather, aes(x=n_hour, y=per_mile))+
  geom_boxplot()+
  labs(title="Average Price Per Mile by Hour", x="Hour", y="Avg(Price/Distance)")
box_pm_hour

#frequency of hour and source
fre_s_hour<-ggplot(uber_weather, aes(x=n_hour, y=source))+
  geom_bin2d()+
  labs(title="Frequency by Hour and Source", x="Hour", y="Source", fill="Frequency")
fre_s_hour

#average price by hour
avg_p_each_hour<-as.data.frame(aggregate(uber_weather$price,by=list(uber_weather$n_hour),FUN=mean))%>%
  mutate(Condition=ifelse(x>avg_p, "greater than average price", "less than average price"))
avg_p_each_hour

bar_avg_p_each_hour<-ggplot(avg_p_each_hour, aes(x=Group.1, y=x,fill=Condition))+
  geom_col()+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Average Price by Hour", x="Hour", y="Price")
bar_avg_p_each_hour

#average price per mile by hour
avg_pm_each_hour<-as.data.frame(aggregate(uber_weather$per_mile,by=list(uber_weather$n_hour),FUN=mean))

bar_pm_each_hour<-ggplot(avg_pm_each_hour, aes(x=Group.1, y=x))+
  geom_col()+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Average Price Per Mile by Hour", x="Hour", y="Avg(Price/Distance)")
bar_pm_each_hour







