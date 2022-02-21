#comparison between "price" and "humidity"
#classify the humidity
uber_weather<-uber_weather%>%
  mutate(n_humidity_range=case_when(humidity<=0.5~"<=0.5",
                                    humidity>0.5&humidity<=0.6~"0.5-0.6",
                                    humidity>0.6&humidity<=0.7~"0.6-0.7",
                                    humidity>0.7&humidity<=0.8~"0.7-0.8",
                                    humidity>0.8&humidity<=0.9~"0.8-0.9",
                                    humidity>0.9~">0.9"))


uber_weather<- within(uber_weather,
                      n_humidity_range <- factor(
                        n_humidity_range,levels=c("<=0.5","0.5-0.6","0.6-0.7","0.7-0.8","0.8-0.9",">0.9")))

#max humidity
summary(cab_rides$humidity)

#average humidity
avg_h<-mean(uber_weather$humidity, na.rm = T)
avg_h

#correlation
ct_h<-cor.test(uber_weather$price, uber_weather$humidity)
ct_h

#frequency of humidity
fre_h<-ggplot(uber_weather[!is.na(uber_weather$humidity),], aes(x=humidity))+
  geom_bar()+
  labs(title="Frequency of Humidity", x="Humidity", y="Frequency")
fre_h

#average price in each range of humidity
avg_p_each_h<-as.data.frame(aggregate(uber_weather$price,by=list(uber_weather$n_humidity_range),FUN=mean,na.rm=TRUE))%>%
  mutate(Condition=if_else(x>avg_p, "greater than average price", "less than average price"))
avg_p_each_h

bar_avg_p_each_h<-ggplot(avg_p_each_h, aes(x=Group.1, y=x, fill=Condition))+
  geom_col()+
  scale_y_continuous(labels=scales::dollar_format())+
  labs(title="Average Price in Each Humidity Range", x="Humidity", y="")
bar_avg_p_each_h